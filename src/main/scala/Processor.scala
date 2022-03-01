package exproc

import scala.quoted.*
import scala.compiletime.*

package object Utils {
  extension (s: String)
    private def emphColor = Console.BLUE
    def emph: String = emphColor + s.replace("\n", s"\n${emphColor}")
}

case class ExpressionProcessorException(msg: String) extends Exception {
  override def toString = msg
}
class FeatureMisuseException(msg: String) extends ExpressionProcessorException(msg) {}

case class ExpressionProcessorImplementationError(msg: String) extends Error {
  override def toString = msg
}


trait Processor[Processed[+_]] {
  import Utils.*

  def sequence[T](fsts: Seq[Processed[Any]], last: Processed[T]): Processed[T]
  def ifThenElse[T](cond: Processed[Boolean], thenn: Processed[T], elze: Processed[T]): Processed[T]
  def whileLoop(cond: Processed[Boolean], body: Processed[Any]): Processed[Unit]

  def empty: Processed[Unit] = throw ExpressionProcessorException("Unavailable feature: empty block")

  final transparent inline def apply[T](inline expr: Processed[T]): Processed[T] = process(this)(expr)

  private def conversionToBeErased(element: String): Nothing = 
    throw FeatureMisuseException(s"${element.emph} should have been erased during processing; using this conversion in unintended places is illegal.")
  given Conversion[Processed[Boolean], Boolean] = conversionToBeErased("Conversion[Processed[Boolean], Boolean]")
}

inline def process[Processed[+_], T](processor: Processor[Processed])(inline expr: Processed[T]): Processed[T] =
  ${processImpl('{processor}, '{expr})}

private def processImpl[Processed[+_], T](processor: Expr[Processor[Processed]], expr: Expr[Processed[T]])(using Type[Processed], Type[T], Quotes): Expr[Processed[T]] = 
  import quotes.reflect.*
  import Utils.*

  extension (t: Term) {
    def isExprOf[S](using Type[S]): Boolean = t.tpe <:< TypeRepr.of[S]

    def isProcessed[S](using Type[S]): Boolean = isExprOf[Processed[S]]

    private def processedInternal[S](onError: String => Nothing)(using Type[S]): Expr[Processed[S]] =
      if !t.isProcessed[S] then 
        onError(
            "Error while processing\n%s\nImpossible to process expression\n%s\nwith type %s\nexpected %s".format(
            Printer.TreeCode.show(expr.asTerm).emph,
            Printer.TreeCode.show(t).emph,
            Printer.TypeReprCode.show(t.tpe).emph,
            Printer.TypeReprCode.show(TypeRepr.of[Processed[S]]).emph
          )
        )
      else t.asExprOf[Processed[S]]

    def processedOrError[S](using Type[S]): Expr[Processed[S]] =
      processedInternal[S](err => throw ExpressionProcessorException(err))

    def processed[S](using Type[S]): Expr[Processed[S]] = 
      processedInternal(err => throw ExpressionProcessorImplementationError(err))
  }

  object ImplicitConversion {
    def unapply(t: Term): Option[(Term, TypeRepr, TypeRepr)] = t match 
      case a@Apply(Select(conversion, "apply"), arg :: Nil) => 
        val convKind = TypeRepr.of[Conversion[_, _]]
        val conv = convKind.appliedTo(List(arg.tpe, a.tpe))
        if conv.typeSymbol == conversion.tpe.typeSymbol && conversion.symbol.flags.is(Flags.Given) then
          Some((arg, arg.tpe, a.tpe))
        else 
          None
      case _ => None
  }

  object BoolUnwrapping {
    def unapply(t: Term): Option[Term] = t match {
      case ImplicitConversion(converted, from, to) 
        if from <:< TypeRepr.of[Processed[Boolean]] && to =:= TypeRepr.of[Boolean] => Some(converted)
      case _ => None 
    }
  }

  object Transform extends TreeMap {

    def splitStatements(statements: List[Statement])(owner: Symbol): (List[Statement], List[Expr[Processed[Any]]]) =
      val pStatements = transformStats(statements)(owner)
      val exprs = pStatements.collect{
        case t: Term if t.isProcessed[Any] => t.processed[Any]
      }
      //val remStatements = pStatements.filter(s => !s.isInstanceOf[Term] || !s.asInstanceOf[Term].isProcessed)
      (pStatements, exprs)

    override def transformTerm(t: Term)(owner: Symbol): Term = t match 
      case Block(statements, term) =>
        val (pStatements, exprs) = splitStatements(statements)(owner)
        val last = transformTerm(term)(owner).processed
        Block(
          pStatements,
          '{ ${processor}.sequence(${Expr.ofList(exprs)}, ${last}) }.asTerm
        )

      case If(BoolUnwrapping(cond), thenn, elze) =>
        val pCond = transformTerm(cond)(owner)
        if pCond.isProcessed[Boolean] then 
          val pThen = transformTerm(thenn)(owner).processedOrError
          val pElse = transformTerm(elze)(owner).processedOrError
          '{ 
            ${processor}.ifThenElse(
              ${ pCond.processed[Boolean] },
              ${ pThen },
              ${ pElse }
            ) 
          }.asTerm
        else
          super.transformTerm(t)(owner)

      case While(BoolUnwrapping(cond), Block(statements, Literal(UnitConstant()))) =>
        val pCond = transformTerm(cond)(owner)
        if pCond.isProcessed[Boolean] then
          val (pStatements, exprs) = splitStatements(statements)(owner)
          val (firsts: Expr[List[Processed[Any]]], last) = exprs match 
            case Nil => (Nil, '{${processor}.empty})
            case f :+ l => (Expr.ofList(f), l)
            case _ => throw ExpressionProcessorImplementationError("Match should have been exhaustive...")
          Block(
            pStatements,
            '{ ${processor}.whileLoop( ${pCond.processed[Boolean]}, ${processor}.sequence(${firsts}, ${last})) }.asTerm
          )
        else 
          super.transformTerm(t)(owner)

      case _ => super.transformTerm(t)(owner)
  }

  Transform.transformTerm(expr.asTerm)(Symbol.spliceOwner).processedOrError[T]
