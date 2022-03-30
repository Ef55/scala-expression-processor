package exproc

import scala.quoted.*

/** Thrown when the processing reaches and invalid state.
  * 
  * Indicates an implementation error in the library.
  */
final case class ExpressionProcessorImplementationError protected (msg: String) extends Error {
  override def toString = msg
}

/** Contains "low-priority givens" used 
  * to allow processed programs to typecheck prior to processing.
  */ 
trait TypeTricker[Processed[+_], Variable[+t] <: Processed[t]] {
  private final def conversionToBeErased(element: String): Nothing = 
    throw ExpressionProcessorImplementationError(s"The given ${element} should have been erased during processing;.")

  /** Converts a processed expression so that it can be used
    * as the condition of a if/while.
    * 
    * Allows things such as:
    * {{{
    * val x: Variable[Boolean] = ...
    * if x then ... else ...
    * while x do ...
    * }}}
    */
  given boolUnwrapper: Conversion[Processed[Boolean], Boolean] = 
    conversionToBeErased("Conversion[Processed[Boolean], Boolean]")

  /** Converts a value into a variable
    * so that it can be used as an initializer of a variable.
    * 
    * Allows things such as:
    * {{{
    * val x: Variable[Int] = 0
    * }}}
    */
  given variableConstantInitialization[T, S](using cv: Conversion[T, Processed[S]]): Conversion[T, Variable[S]] =
    conversionToBeErased("Conversion[T, Processed[T]]")

  /** Converts a processed value (i.e. an AST) into a variable
    * so that it can be used as an initializer of a variable.
    * 
    * Allows things such as:
    * {{{
    * val x: Variable[Int] = constant(0)
    * val y: Variable[Int] = y
    * }}}
    */
  given variableExpressionInitialization[T]: Conversion[Processed[T], Variable[T]] =
    conversionToBeErased("Conversion[Processed[T], Variable[T]]")
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
  * In the documentation of its methods, we will use |...|
  * to represent the application of this translation.
  * 
  * The sequence construction [[exproc.AstBuilder.sequence]] will also sometimes 
  * be eluded for clarity.
  * 
  * @tparam Processed Type of the resulting AST.
  * @tparam Variable Type of a variable in the AST.
  */
trait AstBuilder[Processed[+_], Variable[+t] <: Processed[t]] extends Builder[Processed] with TypeTricker[Processed, Variable] {
  /** Uses this AST builder to transform the provided scala code.
    */ 
  final override inline def apply[T](inline expr: Processed[T])(using config: BuilderConfig): Processed[T] = processAst(this)(expr)(config)

  /** Constructs a variable.
    * 
    * Used in variable declaration:
    * {{{val x: Variable[T] = init}}} 
    * becomes
    * {{{
    * val x: Variable[T] = variable[T]("x")
    * initialize(x, |init|)
    * }}}
    * assuming
    * {{{init: Processed[T]}}}
    */
  def variable[T](id: Identifier): Variable[T]

  /** Constructs the initialization of a variable.
    * 
    * Used in variable declaration:
    * {{{val x: Variable[T] = init}}} 
    * becomes
    * {{{
    * val x: Variable[T] = variable[T]("x")
    * initialize(x, |init|)
    * }}}
    * assuming
    * {{{init: Processed[T]}}}
    */
  def initialize[T](va: Variable[T], init: Processed[T]): Processed[Unit]

  /** Constructs the assignation of a variable.
    * 
    * Used in variable re-assignation:
    * {{{
    * var x: Variable[T] = init
    * x = newVal
    * }}} 
    * becomes
    * {{{
    * val x: Variable[T] = variable[T]("x")
    * initialize(x, |init|)
    * assign(x, |newVal|)
    * }}}
    * assuming
    * {{{init: Processed[T]}}}
    */
  def assign[T](va: Variable[T], init: Processed[T]): Processed[Unit]

  /** Constructs a constant.
    * 
    * Used in variable declaration:
    * {{{val x: Variable[T] = init}}} 
    * becomes
    * {{{
    * val x: Variable[T] = variable[T]("x")
    * initialize(x, constant(init))
    * }}}
    * assuming
    * {{{init: T}}}
    */
  def constant[T](t: T): Processed[T]

  /** Constructs a sequence of multiple statements.
    * 
    * Used in code blocks:
    * {{{
    * {
    *   val x = ...
    *   while ...
    *   x
    * }
    * }}}
    * becomes
    * {{{sequence(Seq(|val x = ...|, |while ...|), |x|)}}}
    */
  def sequence[T](fsts: Seq[Processed[Any]], last: Processed[T]): Processed[T]

  /** Constructs an if-then-else.
    * 
    * Used in if-then-else blocks:
    * {{{
    * if x === Constant(0) then
    *   constant(1)
    * else
    *   constant(0)
    * }}}
    * becomes
    * {{{ifThenElse(|x === constant(0)|, |constant(1)|, |constant(0)|)}}}
    */
  def ifThenElse[T](cond: Processed[Boolean], thenn: Processed[T], elze: Processed[T]): Processed[T]

  /** Constructs a while-loop.
    * 
    * Used in while-loops:
    * {{{
    * var x: Variable[Int] = init
    * while cond do
    *   x = newVal
    * }}}
    * becomes
    * {{{
    * var x: Variable[Int] = variable[Int]("x")
    * initialize(x, |init|)
    * whileLoop(|cond|, assign(x, |newVal|))
    * }}}
    */
  def whileLoop(cond: Processed[Boolean], body: Processed[Any]): Processed[Unit]

  /** Calls [[exproc.AstBuilder.constant]] when needed.
    */ 
  given autoConstant[T]: Conversion[T, Processed[T]] with
    def apply(t: T) = constant(t)
}

private inline def processAst[Processed[+_], Variable[+t] <: Processed[t], T]
(processor: AstBuilder[Processed, Variable])
(inline expr: Processed[T])
(config: BuilderConfig): Processed[T] =
  ${processAstImpl('{processor}, '{expr})('{config})}

private def processAstImpl[Processed[+_], Variable[+t] <: Processed[t], T]
(processorExpr: Expr[AstBuilder[Processed, Variable]], expr: Expr[Processed[T]])
(config: Expr[BuilderConfig])
(using Type[Processed], Type[Variable], Type[T], Quotes): Expr[Processed[T]] = 
  import quotes.reflect.*

  object processor {
    private inline def member(name: String): Term = 
      val methds = symbol.declaredMethod(name)
      assert(methds.length == 1)
      term.select(methds.head)

    val symbol = TypeRepr.of[AstBuilder].typeSymbol
    val term = processorExpr.asTerm

    def variableType(t: TypeRepr): String = s"${TypeRepr.of[Variable].typeSymbol.name}[${t.typeSymbol.fullName}]"

    def variable(t: TypeRepr)(id: Term): Term = 
      member("variable").appliedToType(t).appliedTo(id)
    def initialize(t: TypeRepr)(va: Term, init: Term): Term = 
      member("initialize").appliedToType(t).appliedToArgs(List(va, init))
    def assign(t: TypeRepr)(va: Term, init: Term): Term = 
      member("assign").appliedToType(t).appliedToArgs(List(va, init))
    def constant(t: TypeRepr)(cst: Term): Term = 
      member("constant").appliedToType(t).appliedTo(cst)
    def sequence(t: TypeRepr)(fsts: Term, last: Term): Term = 
      member("sequence").appliedToType(t).appliedToArgs(List(fsts, last))
    def sequence(t: TypeRepr)(fsts: List[Term], last: Term): Term = 
      sequence(t)(Expr.ofList(fsts.map(_.asProcessed)).asTerm, last)
    def ifThenElse(t: TypeRepr)(cond: Term, thenn: Term, elze: Term): Term = 
      member("ifThenElse").appliedToType(t).appliedToArgs(List(cond, thenn, elze))
    def whileLoop(cond: Term, body: Term): Term = 
      member("whileLoop").appliedToArgs(List(cond, body))

    def sequenceSeq(ls: List[Term]): Term = 
      require(!ls.isEmpty)
      val (firsts, last) = (ls: @unchecked) match 
        case solo :: Nil => (List.empty, solo)
        case firsts :+ last => (firsts, last)

      processor.sequence(TypeRepr.of[Any])(
        firsts,
        last
      )
  }  

  extension (t: Term) {
    def isExprOf(tpe: TypeRepr): Boolean = t.tpe <:< tpe

    def isProcessedOf(tpe: TypeRepr): Boolean = isExprOf(TypeRepr.of[Processed].appliedTo(tpe))

    def asProcessedOf[S](using Type[S]): Expr[Processed[S]] = 
      require(isProcessedOf(TypeRepr.of[S]))
      t.asExprOf[Processed[S]]

    def isProcessedBoolean: Boolean = isProcessedOf(TypeRepr.of[Boolean])

    def isProcessed: Boolean = isProcessedOf(TypeRepr.of[Any])

    def asProcessed: Expr[Processed[Any]] = asProcessedOf[Any]

    def isVariable: Boolean = isExprOf(TypeRepr.of[Variable[Any]])
  }

  object BoolUnwrapping {
    private val base = new ImplicitUnwrapper[Processed]{}
    def unapply(t: Term): Option[Term] =
      base.unapply(t).filter(_ => t.tpe =:= TypeRepr.of[Boolean])
  }

  object InitializerUnwrapping {
    private val base = new SpecificImplicitConversion[Any, Variable[Any]]{}
    def unapply(t: Term): Option[(Term, Option[Term])] =
      base.unapply(t).map{ case (conversion, converted) => 
        val subconversion = conversion match
          case Apply(_, arg :: Nil) => Some(arg) // Retrieve the (implicit) Conversion[T, Processed[S]]
          case _ => None
        (converted, subconversion)
      }
  }

  object AutoConstantUnwrapping extends ImplicitWrapper[Processed]

  object ProcessedParameter extends Unwrap[Processed]

  object Transform extends TreeMap {

    override def transformTree(t: Tree)(owner: Symbol): Tree = t match
      case s: Statement => 
        transformToStatements(s)(owner) match 
          case s :: Nil => s
          case s :: expr :: Nil => expr match 
            case expr: Term => Block.copy(t)(List(s), expr)
            case _ => throw ExpressionProcessorImplementationError(s"transformToStatements second statement should be an expression.")
          case _ => throw ExpressionProcessorImplementationError(s"transformToStatements should not return more than two statements.")
      case _ => super.transformTree(t)(owner)

    override def transformStatement(s: Statement)(owner: Symbol) = 
      throw ExpressionProcessorImplementationError("Method should not have been called.")

    def transformAssignee(t: Term)(owner: Symbol): Term = {
      t match 
        case InitializerUnwrapping(init, None) => init
        case InitializerUnwrapping(init, Some(arg)) =>
          Select.unique(arg, "apply").appliedTo(transformTerm(init)(owner))
        case _ => transformTerm(t)(owner)
    }.ensuring(_.isProcessed)

    def transformToStatements(s: Statement)(owner: Symbol): List[Statement] = s match 
      case ValDef(name, tt@ProcessedParameter.TypeTree(typ), Some(initializer)) =>
        def flagWarning(flag: Flags, name: String): Unit = 
          if s.symbol.flags.is(flag) then 
            report.warning(s"${name.capitalize} is ignored for proto-variable definitions.", s.pos)

        val init = transformAssignee(initializer)(s.symbol)

        flagWarning(Flags.Lazy, "lazy")
        flagWarning(Flags.Inline, "inline")

        val vd = 
          ValDef.copy(s)(
            name,
            tt,
            Some(processor.variable(typ)('{Identifier(${Expr(name)})}.asTerm))
          )
        List(
          vd,                           // Meta-variable definition
          processor.initialize(typ)(    // Proto-variable definition
            Ref(vd.symbol),
            init.changeOwner(owner)
          )
        )
      case _ => List(super.transformStatement(s)(owner))

    override def transformStats(ls: List[Statement])(owner: Symbol): List[Statement] =
      ls.flatMap(transformToStatements(_)(owner))

    def splitStatements(statements: List[Statement])(owner: Symbol): (List[Statement], List[Term]) =
      val pStatements = transformStats(statements)(owner)
      pStatements.foldRight[(List[Statement], List[Term])]((Nil, Nil)){ 
        case (statement, (statements, exprs)) => statement match
          case t: Term if t.isProcessed => 
            val vs = Symbol.newVal(owner, s"processed", TypeRepr.of[Processed[Any]], Flags.EmptyFlags, Symbol.noSymbol)
            val vd = ValDef(vs, Some(t.changeOwner(vs)))
            val ref = Ref(vs)
            (vd :: statements, ref :: exprs)
          case _ => (statement :: statements, exprs)
      }

    def forceProcessed(t: Term)(typ: TypeRepr): Term = 
      val tpe: TypeRepr = typ match {
        case ProcessedParameter(t) => t
        case t => t
      }

      if t.isProcessedOf(tpe) then
        t
      else if t.isProcessed then
        throw ExpressionProcessorImplementationError(
          "%s is processed, but has incorrect type %s; expected %s".format(
            Printer.TreeCode.show(t),
            processor.variableType(t.tpe),
            processor.variableType(tpe)
          )
        )
      else
        processor.constant(tpe)(t)

    override def transformTerm(t: Term)(owner: Symbol): Term = t match 
      case BoolUnwrapping(t) => 
        report.errorAndAbort(
          s"Invalid conversion to Boolean; it is only allowed in the condition of a if/while.",
          t.pos
        )

      case InitializerUnwrapping(_, _) =>
        report.errorAndAbort(
          s"Invalid conversion to ${Printer.TypeReprCode.show(t.tpe)}",
          t.pos
        )
      
      case AutoConstantUnwrapping(a@Assign(lhs, _)) if lhs.isVariable => transformTerm(a)(owner)
      case AutoConstantUnwrapping(w@While(_, _)) => forceProcessed(transformTerm(w)(owner))(TypeRepr.of[Any])

      case Block(statements, term) =>  
        val last = transformTerm(term)(owner)
        last.tpe match 
          case ProcessedParameter(typ) =>
            val (pStatements, exprs) = splitStatements(statements)(owner)     
            Block.copy(t)(
              pStatements,
              processor.sequence(typ)(
                exprs,
                last
              )
            )
          case _ => t

      case Assign(lhs, rhs) if lhs.isVariable =>
        val ProcessedParameter(typ) = lhs.tpe
        val assignee = transformAssignee(rhs)(owner)
        processor.assign(typ)(lhs, assignee)

      case If(BoolUnwrapping(cond), thenn, elze) =>
        assert(cond.isProcessedBoolean)
        val pCond = transformTerm(cond)(owner)
        val pThen = forceProcessed(transformTerm(thenn)(owner))(t.tpe)
        val pElse = forceProcessed(transformTerm(elze)(owner))(t.tpe)
        
        val ProcessedParameter(typ) = pThen.tpe
        processor.ifThenElse(typ)(
          pCond, 
          pThen, 
          pElse
        )

      case While(BoolUnwrapping(cond), body) =>
        assert(cond.isProcessedBoolean)
        val pCond = transformTerm(cond)(owner)

        body match 
          case b@Block(statements, l@Literal(UnitConstant())) =>
            // The trailing unit will be inferred by the compiler in most cases, which breaks everything 
            // if we do not ignore it.
            val (pStatements, exprs) = splitStatements(statements)(owner)
            Block.copy(b)(
              pStatements,
              processor.whileLoop(
                pCond,
                processor.sequenceSeq(exprs)
              )
            )

          case _ => 
            val pBody = transformTerm(body)(owner)

            if !pBody.isProcessed then
              report.errorAndAbort("Cannot use proto-while to return meta-values.", t.pos)

            processor.whileLoop(
              pCond,
              transformTerm(body)(owner)
            )
        

      case _ => super.transformTerm(t)(owner)
  }


  val before = s"Input:\n${expr.asTerm.show}"
  val r = Transform.transformTerm(expr.asTerm)(Symbol.spliceOwner).asProcessedOf[T]
  val after = s"Processed:\n${Printer.TreeCode.show(r.asTerm)}"
  '{
    if ${config}.printCode then
        println(${Expr(before)})
    if ${config}.printCode then
      println(${Expr(after)})
    ${r}
  }
