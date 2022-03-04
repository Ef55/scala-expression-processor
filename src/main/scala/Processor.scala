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
class InvalidProtoType(typeName: String) extends ExpressionProcessorException(s"Invalid proto-type: ${typeName}") {}

case class ExpressionProcessorImplementationError(msg: String) extends Error {
  override def toString = msg
}


trait Processor[Processed[+_], Variable[+t] <: Processed[t]] {
  import Utils.*

  def variable[T](id: Identifier): Variable[T]
  def initializer[T](va: Variable[T], init: Processed[T]): Processed[Unit]
  def constant[T](t: T): Processed[T]
  def sequence[T](fsts: Seq[Processed[Any]], last: Processed[T]): Processed[T]
  def ifThenElse[T](cond: Processed[Boolean], thenn: Processed[T], elze: Processed[T]): Processed[T]
  def whileLoop(cond: Processed[Boolean], body: Processed[Any]): Processed[Unit]

  def empty: Processed[Unit] = throw ExpressionProcessorException("Unavailable feature: empty block")

  final inline def apply[T](inline expr: Processed[T]): Processed[T] = process(this)(expr)

  private def conversionToBeErased(element: String): Nothing = 
    throw FeatureMisuseException(s"${element.emph} should have been erased during processing; using this conversion in unintended places is illegal.")
  
  given boolUnwrapper: Conversion[Processed[Boolean], Boolean] = conversionToBeErased("Conversion[Processed[Boolean], Boolean]")
  given variableInitialization[T]: Conversion[T, Variable[T]] = conversionToBeErased("Conversion[T, Processed[T]]")
}

inline def process[Processed[+_], Variable[+t] <: Processed[t], T](processor: Processor[Processed, Variable])(inline expr: Processed[T]): Processed[T] =
  ${processImpl('{processor}, '{expr})}

private def processImpl[Processed[+_], Variable[+t] <: Processed[t], T]
(processorExpr: Expr[Processor[Processed, Variable]], expr: Expr[Processed[T]])
(using Type[Processed], Type[Variable], Type[T], Quotes): Expr[Processed[T]] = 
  import quotes.reflect.*
  import Utils.*

  object processor {
    private inline def member(name: String): Term = 
      val methds = symbol.declaredMethod(name)
      assert(methds.length == 1)
      term.select(methds.head)

    val symbol = TypeRepr.of[Processor].typeSymbol
    val term = processorExpr.asTerm

    val variable = member("variable")
    val initializer = member("initializer")
    val constant = member("constant")
    val sequence = member("sequence")
    val ifThenElse = member("ifThenElse")
    val whileLoop = member("whileLoop")
  }  

  extension (t: Term) {
    def isExprOf[S](using Type[S]): Boolean = t.tpe <:< TypeRepr.of[S]

    def isProcessedOf[S](using Type[S]): Boolean = isExprOf[Processed[S]]
    
    private def processedInternal[S](onError: String => Nothing)(using Type[S]): Expr[Processed[S]] =
      if !t.isProcessedOf[S] then 
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
      processedInternal[S](err => throw ExpressionProcessorImplementationError(err))
  }

  def processes[S](t: Term)(using Type[S]): Expr[Processed[S]] =
    t.processed[S]

  object ImplicitConversion {
    def unapply(t: Term): Option[(Term, TypeRepr, TypeRepr)] = t match 
      case a@Apply(Select(conversion, "apply"), arg :: Nil) => 
        val convKind = TypeRepr.of[Conversion]
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

  object InitializationUnwrapping {
    def unapply(t: Term): Option[(Term, TypeRepr)] = t match {
      case ImplicitConversion(converted, from, to) 
        if TypeRepr.of[Variable].appliedTo(from) <:< to => Some((converted, from))
      case _ => None 
    }
  }

  object ProcessedParameter {
    def unapply(tt: TypeRepr): Option[TypeRepr] = 
      require(tt <:< TypeRepr.of[Processed[Any]])
      val bt = tt.baseType(TypeRepr.of[Processed].typeSymbol)
      bt match {
        case AppliedType(cstr, arg :: Nil) => 
          assert(cstr =:= TypeRepr.of[Processed])
          Some(arg)
        case _ => None
      }
  }

  object Transform extends TreeMap {

    override def transformStatement(s: Statement)(owner: Symbol) = 
      throw ExpressionProcessorImplementationError("Method should not have been called.")

    override def transformStats(ls: List[Statement])(owner: Symbol): List[Statement] =
      def transformStatement(s: Statement): List[Statement] = s match 
        case ValDef(name, tt, Some(InitializationUnwrapping(init, initt))) => 
          val ProcessedParameter(typ) = tt.tpe
          val va = 
            processor
              .variable
              .appliedToType(typ)
              .appliedTo('{Identifier(${Expr(name)})}.asTerm)
          List(
            ValDef.copy(s)(name, tt, Some(va)),   // Meta-variable definition
            processor                             // Proto-variable definition
              .initializer
              .appliedToType(typ)
              .appliedTo(
                va, 
                processor
                  .constant
                  .appliedToType(typ)
                  .appliedTo(init)
              )
          )
        case _ => List(super.transformStatement(s)(owner))

      ls.flatMap(transformStatement)

    def splitStatements(statements: List[Statement])(owner: Symbol): (List[Statement], List[Expr[Processed[Any]]]) =
      val pStatements = transformStats(statements)(owner)
      val exprs = pStatements.collect{
        case t: Term if t.isProcessedOf[Any] => t.processed[Any]
      }
      //val remStatements = pStatements.filter(s => !s.isInstanceOf[Term] || !s.asInstanceOf[Term].isProcessed)
      (pStatements, exprs)

    override def transformTerm(t: Term)(owner: Symbol): Term = t match 
      case Block(statements, term) =>
        val (pStatements, exprs) = splitStatements(statements)(owner)       
        val last = transformTerm(term)(owner)
        val ProcessedParameter(typ) = t.tpe

        Block(
          pStatements,
          processor
            .sequence
            .appliedToType(typ)
            .appliedToArgs(List(
              Expr.ofList(exprs).asTerm, 
              last
            ))
        )

      case If(BoolUnwrapping(cond), thenn, elze) =>
        assert(cond.isProcessedOf[Boolean])
        assert(thenn.isProcessedOf[Any])
        assert(elze.isProcessedOf[Any])
        val pCond = transformTerm(cond)(owner)
        val pThen = transformTerm(thenn)(owner)
        val pElse = transformTerm(elze)(owner)
        val ProcessedParameter(typ) = t.tpe
        processor
          .ifThenElse
          .appliedToType(typ)
          .appliedToArgs(List(
            pCond, 
            pThen, 
            pElse
          ))

      case While(BoolUnwrapping(cond), Block(statements, Literal(UnitConstant()))) =>
        assert(cond.isProcessedOf[Boolean])
        val pCond = transformTerm(cond)(owner)
        val (pStatements, exprs) = splitStatements(statements)(owner)
        val (firsts: Expr[List[Processed[Any]]], last) = exprs match 
          case Nil => (Nil, '{${processorExpr}.empty})
          case f :+ l => (Expr.ofList(f), l)
          case _ => throw ExpressionProcessorImplementationError("Match should have been exhaustive...")
        Block(
          pStatements,
          processor
            .whileLoop
            .appliedToArgs(List(
              pCond,
              processor
                .sequence
                .appliedToType(TypeRepr.of[Any])
                .appliedToArgs(List(
                  firsts.asTerm,
                  last.asTerm
                ))
            ))
        )

      case _ => super.transformTerm(t)(owner)
  }

  val r = Transform.transformTerm(expr.asTerm)(Symbol.spliceOwner).processedOrError[T]
  //println(s"Processed:\n${r.asTerm.show}")
  r
