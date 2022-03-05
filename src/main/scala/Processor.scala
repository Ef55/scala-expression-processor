package exproc

import scala.quoted.*

/** Thrown when the processing requires an optional feature which is 
  * not supported by the processor at hand.
  * 
  * @param feature Name of the unsupported feature.
  * 
  * @see [[exproc.Processor]] for a list of optional features.
  */
sealed case class UnsupportedFeature(feature: String) extends Exception {
  override def toString = s"Missing feature --- ${feature}"
}

/** Thrown when the processing reaches and invalid state.
  * 
  * Indicates an implementation error in the library.
  */
final case class ExpressionProcessorImplementationError protected (msg: String) extends Error {
  override def toString = msg
}

/** Defines methods used to transform scala code into 
  * a user-defined 
  *  [[https://en.wikipedia.org/wiki/Abstract_syntax_tree Abstract Syntax Tree]] (AST).
  * 
  * The sub-classes should also define a suitable 
  * [[https://en.wikipedia.org/wiki/Domain-specific_language Domain Specific Language]] (DSL).
  * 
  * The scala code is translated using 
  * {{{def apply[T](inline expr: Processed[T]): Processed[T]}}}
  * 
  * @tparam Processed Type of the resulting AST.
  * @tparam Variable Type of a variable in the AST.
  * 
  * @groupprio base 1
  * @groupname base Usage
  * @groupprio cstr 2
  * @groupname cstr Constructor
  * @groupprio opt 3
  * @groupname opt Optional constructors
  * @groupprio dsl 4
  * @groupname dsl DSL
  */
trait Processor[Processed[+_], Variable[+t] <: Processed[t]] {
  /** Uses this processor to transform the provided scala code.
    *
    * @group base
    */ 
  final inline def apply[T](inline expr: Processed[T]): Processed[T] = process(this)(expr)    

  /** Constructs a variable.
    * 
    * @group cstr
    */
  def variable[T](id: Identifier): Variable[T]

  /** Constructs the initialization of a variable.
    * 
    * @group cstr
    */
  def initializer[T](va: Variable[T], init: Processed[T]): Processed[Unit]

  /** Constructs a constant.
    * 
    * @group cstr
    */
  def constant[T](t: T): Processed[T]

  /** Constructs a sequence of multiple statements.
    * 
    * @group cstr
    */
  def sequence[T](fsts: Seq[Processed[Any]], last: Processed[T]): Processed[T]

  /** Constructs an if-then-else.
    * 
    * @group cstr
    */
  def ifThenElse[T](cond: Processed[Boolean], thenn: Processed[T], elze: Processed[T]): Processed[T]

  /** Constructs a while-loop.
    * 
    * @group cstr
    */
  def whileLoop(cond: Processed[Boolean], body: Processed[Any]): Processed[Unit]

  /** Constructs an empty AST.
    * 
    * @group opt
    */
  def empty: Processed[Unit] = throw UnsupportedFeature("Unavailable feature: empty")

  private final def conversionToBeErased(element: String): Nothing = 
    throw ExpressionProcessorImplementationError(s"The given ${element} should have been erased during processing;.")
  
  /** Converts a processed expression so that it can be used
    * as the condition of a if/while.
    * 
    * @group dsl
    */
  given boolUnwrapper: Conversion[Processed[Boolean], Boolean] = 
    conversionToBeErased("Conversion[Processed[Boolean], Boolean]")

  /** Converts a value into a processed expression
    * so that it can be used as an initializer of a variable.
    * 
    * @group dsl
    */
  given variableInitialization[T]: Conversion[T, Variable[T]] = 
    conversionToBeErased("Conversion[T, Processed[T]]")
}

private inline def process[Processed[+_], Variable[+t] <: Processed[t], T](processor: Processor[Processed, Variable])(inline expr: Processed[T]): Processed[T] =
  ${processImpl('{processor}, '{expr})}

private def processImpl[Processed[+_], Variable[+t] <: Processed[t], T]
(processorExpr: Expr[Processor[Processed, Variable]], expr: Expr[Processed[T]])
(using Type[Processed], Type[Variable], Type[T], Quotes): Expr[Processed[T]] = 
  import quotes.reflect.*

  object processor {
    private inline def member(name: String): Term = 
      val methds = symbol.declaredMethod(name)
      assert(methds.length == 1)
      term.select(methds.head)

    val symbol = TypeRepr.of[Processor].typeSymbol
    val term = processorExpr.asTerm

    def variable(t: TypeRepr)(id: Term): Term = 
      member("variable").appliedToType(t).appliedTo(id)
    def initializer(t: TypeRepr)(va: Term, init: Term): Term = 
      member("initializer").appliedToType(t).appliedToArgs(List(va, init))
    def constant(t: TypeRepr)(cst: Term): Term = 
      member("constant").appliedToType(t).appliedTo(cst)
    def sequence(t: TypeRepr)(fsts: Term, last: Term): Term = 
      member("sequence").appliedToType(t).appliedToArgs(List(fsts, last))
    def ifThenElse(t: TypeRepr)(cond: Term, thenn: Term, elze: Term): Term = 
      member("ifThenElse").appliedToType(t).appliedToArgs(List(cond, thenn, elze))
    def whileLoop(cond: Term, body: Term): Term = 
      member("whileLoop").appliedToArgs(List(cond, body))
    def empty: Term = member("empty")

    def sequenceSeq(ls: List[Expr[Processed[Any]]]): Term = 
      require(!ls.isEmpty)
      val (firsts, last) = (ls: @unchecked) match 
        case solo :: Nil => (List.empty[Expr[Processed[Any]]], solo)
        case firsts :+ last => (firsts, last)

      processor.sequence(TypeRepr.of[Any])(
        Expr.ofList(firsts).asTerm,
        last.asTerm
      )
  }  

  extension (t: Term) {
    def isExprOf[S](using Type[S]): Boolean = t.tpe <:< TypeRepr.of[S]

    def isProcessedOf[S](using Type[S]): Boolean = isExprOf[Processed[S]]
  }


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

    override def transformTree(t: Tree)(owner: Symbol): Tree = t match
      case s: Statement => 
        transformToStatements(s)(owner) match 
          case s :: Nil => s
          case s :: expr :: Nil => expr match 
            case expr: Term => Block(List(s), expr)
            case _ => throw ExpressionProcessorImplementationError(s"transformToStatements second statement should be an expression.")
          case _ => throw ExpressionProcessorImplementationError(s"transformToStatements should not return more than two statements.")
      case _ => super.transformTree(t)(owner)

    override def transformStatement(s: Statement)(owner: Symbol) = 
      throw ExpressionProcessorImplementationError("Method should not have been called.")

    def transformToStatements(s: Statement)(owner: Symbol): List[Statement] = s match 
      case ValDef(name, tt, Some(InitializationUnwrapping(init, initt))) => 
        val ProcessedParameter(typ) = tt.tpe
        val va = 
          processor
            .variable(typ)('{Identifier(${Expr(name)})}.asTerm)
        List(
          ValDef.copy(s)(name, tt, Some(va)),   // Meta-variable definition
          processor.initializer(typ)(           // Proto-variable definition
            va,
            processor.constant(typ)(init)
          )
        )
      case _ => List(super.transformStatement(s)(owner))

    override def transformStats(ls: List[Statement])(owner: Symbol): List[Statement] =
      ls.flatMap(transformToStatements(_)(owner))

    def splitStatements(statements: List[Statement])(owner: Symbol): (List[Statement], List[Expr[Processed[Any]]]) =
      val pStatements = transformStats(statements)(owner)
      val exprs = pStatements.collect{
        case t: Term if t.isProcessedOf[Any] => t.asExprOf[Processed[Any]]
      }
      //val remStatements = pStatements.filter(s => !s.isInstanceOf[Term] || !s.asInstanceOf[Term].isProcessed)
      (pStatements, exprs)

    override def transformTerm(t: Term)(owner: Symbol): Term = t match 

      case InitializationUnwrapping(Literal(UnitConstant()), _) =>
        processor.empty

      case Block(statements, InitializationUnwrapping(Literal(UnitConstant()), _)) =>
        val (pStatements, exprs) = splitStatements(statements)(owner)

        Block(
          pStatements,
          processor.sequence(TypeRepr.of[Unit])(
            Expr.ofList(exprs).asTerm,
            processor.empty
          )
        )

      case InitializationUnwrapping(t, typ) =>
        report.errorAndAbort(
          s"${t.show}: ${typ.show} is not of type ${TypeRepr.of[Processed].typeSymbol.name}[T]",
          t.pos
        )
      
      case BoolUnwrapping(t) => 
        report.errorAndAbort(
          s"${t.show}: is being converted to Boolean, which is only allowed in the condition of a if/while.",
          t.pos
        )

      case Block(statements, term) =>
        val (pStatements, exprs) = splitStatements(statements)(owner)       
        val last = transformTerm(term)(owner)
        val ProcessedParameter(typ) = t.tpe

        Block(
          pStatements,
          processor.sequence(typ)(
            Expr.ofList(exprs).asTerm, 
            last
          )
        )

      case If(BoolUnwrapping(cond), thenn, elze) =>
        assert(cond.isProcessedOf[Boolean])
        if !thenn.isProcessedOf[Any] || !elze.isProcessedOf[Any] then
          report.errorAndAbort("Cannot use proto-if-then-else to return meta-values.", t.pos)
        val pCond = transformTerm(cond)(owner)
        val pThen = transformTerm(thenn)(owner)
        val pElse = transformTerm(elze)(owner)
        val ProcessedParameter(typ) = t.tpe
        processor.ifThenElse(typ)(
          pCond, 
          pThen, 
          pElse
        )

      case While(BoolUnwrapping(cond), Block(statements, Literal(UnitConstant()))) =>
        /* Note: this case handles the full `<while> cond <block> statements () </block> </while>`
         * at once to avoid an unnecessary call to `processor.empty`
         */
        
        assert(cond.isProcessedOf[Boolean])
        val pCond = transformTerm(cond)(owner)
        val (pStatements, exprs) = splitStatements(statements)(owner)

        Block(
          pStatements,
          processor.whileLoop(
            pCond,
            processor.sequenceSeq(exprs)
          )
        )

      case _ => super.transformTerm(t)(owner)
  }

  //println(s"Input:\n${expr.asTerm.show}")
  val r = Transform.transformTerm(expr.asTerm)(Symbol.spliceOwner).asExprOf[Processed[T]]
  //println(s"Processed:\n${r.asTerm.show}")
  r
