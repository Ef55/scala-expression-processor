package exproc

import scala.quoted.*
import scala.compiletime.*

case class ExpressionProcessorException(msg: String) extends Exception {
  override def toString = msg
}
case class ExpressionProcessorImplementationError(msg: String) extends Error {
  override def toString = msg
}

trait Processor[Processed] {
  final def sequenceDispatch(ls: Seq[Processed]): Processed = ls.length match 
    case 0 => throw ExpressionProcessorImplementationError("Empty sequence processed.")
    case 1 => ls.head
    case _ => sequence(ls)

  def sequence(ls: Seq[Processed]): Processed
  def ifThenElse(cond: Processed, thenn: Processed, elze: Processed): Processed
  def whileLoop(cond: Processed, body: Processed): Processed
}

inline def process[Processed](processor: Processor[Processed])(inline expr: Any): Processed =
  ${processImpl('{processor}, '{expr})}

private def processImpl[Processed](processor: Expr[Processor[Processed]], expr: Expr[Any])(using Type[Processed], Quotes): Expr[Processed] = 
  import quotes.reflect.*

  extension (t: Term) {
    def isExprOf[T](using Type[T]): Boolean = t.tpe <:< TypeRepr.of[T]

    def processedOrError: Expr[Processed] =
      if !t.isProcessable then throw ExpressionProcessorException(s"Impossible to process expression: `${t}`")
      else t.processed

    def isProcessable: Boolean = isExprOf[Processed]

    def processed: Expr[Processed] = 
      require(isProcessable, "Processor error: `processed` should only be called with a processable expression.")
      if t.isExprOf[Processed] then t.asExprOf[Processed]
      else throw ExpressionProcessorImplementationError(s"Term `${t}` could not be processed")
  }

  object ImplicitConversion {
    def unapply(t: Term): Option[(Term, TypeRepr, TypeRepr)] = t match 
      case a@Apply(Select(conversion, "apply"), arg :: Nil) => 
        val convKind = TypeRepr.of[Conversion[_, _]]
        val conv = convKind.appliedTo(List(arg.tpe, a.tpe))
        if conv.typeSymbol == conversion.tpe.typeSymbol then
          Some((arg, a.tpe, arg.tpe))
        else 
          None
      case _ => None
  }

  object Transform extends TreeMap {
    // override def transformStatement(s: Statement)(owner: Symbol): Statement = s match 
    //   case t: Term => transformTerm(t)(owner)
    //   case _ => throw ExpressionProcessorException(s"Unsupported statement: ${s.getClass}")

    override def transformTerm(t: Term)(owner: Symbol): Term = t match 
      case ImplicitConversion(conv, from, to) => conv

      case Block(statements, term) =>
        val pStatements = transformStats(statements :+ term)(owner)
        val exprs = pStatements.collect{
          case t: Term if t.isProcessable => t.processed
        }
        val remStatements = pStatements.filter(s => !s.isInstanceOf[Term] || !s.asInstanceOf[Term].isProcessable)
        Block(
          pStatements,
          '{ ${processor}.sequenceDispatch(${Expr.ofList(exprs)}) }.asTerm
        )

      case If(cond, thenn, elze) =>
        val pCond = transformTerm(cond)(owner)
        if pCond.isProcessable then 
          val pThen = transformTerm(thenn)(owner).processedOrError
          val pElse = transformTerm(elze)(owner).processedOrError
          '{ 
            ${processor}.ifThenElse(
              ${ pCond.processed },
              ${ pThen },
              ${ pElse }
            ) 
          }.asTerm
        else
          super.transformTerm(t)(owner)

      case While(cond, body) =>
        val pCond = transformTerm(cond)(owner)
        if pCond.isProcessable then
          val pBody = transformTerm(body)(owner).processedOrError
          '{ ${processor}.whileLoop( ${pCond.processed}, ${pBody}) }.asTerm
        else 
          super.transformTerm(t)(owner)

      case _ => super.transformTerm(t)(owner)
  }

  Transform.transformTerm(expr.asTerm)(Symbol.spliceOwner).asExprOf[Processed]