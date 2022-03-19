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
    * 
    * @group dsl
    */
  given boolUnwrapper: Conversion[Processed[Boolean], Boolean] = 
    conversionToBeErased("Conversion[Processed[Boolean], Boolean]")

  /** Converts a value into a variable
    * so that it can be used as an initializer of a variable.
    * 
    * Allows things such as:
    * {{{
    * val x: Variable[Unit] = empty
    * }}}
    * 
    * @group dsl
    */
  given variableConstantInitialization[T, S](using cv: Conversion[T, Processed[S]]): Conversion[T, Variable[S]] =
    conversionToBeErased("Conversion[T, Processed[T]]")

  /** Converts a processed value (i.e. an AST) into a variable
    * so that it can be used as an initializer of a variable.
    * 
    * Allows things such as:
    * {{{
    * val x: Variable[Int] = 0
    * }}}
    * 
    * @group dsl
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
trait Processor[Processed[+_], Variable[+t] <: Processed[t]] extends TypeTricker[Processed, Variable] {
  /** Uses this processor to transform the provided scala code.
    *
    * @group base
    */ 
  final inline def apply[T](inline expr: Processed[T]): Processed[T] = process(this)(expr)    

  /** Constructs a variable.
    * 
    * Used in variable declaration:
    * {{{val x: Variable[T] = init}}} 
    * becomes
    * {{{
    * val x: Variable[T] = Variable[T]("x")
    * assign(x, |init|)
    * }}}
    * assuming
    * {{{init: Processed[T]}}}
    * 
    * @group cstr
    */
  def variable[T](id: Identifier): Variable[T]

  /** Constructs the initialization of a variable.
    * 
    * Used in variable declaration:
    * {{{val x: Variable[T] = init}}} 
    * becomes
    * {{{
    * val x: Variable[T] = Variable[T]("x")
    * assign(x, |init|)
    * }}}
    * assuming
    * {{{init: Processed[T]}}}
    * 
    * @group cstr
    */
  def initialize[T](va: Variable[T], init: Processed[T]): Processed[Unit]

  def assign[T](va: Variable[T], init: Processed[T]): Processed[Unit]

  /** Constructs a constant.
    * 
    * Used in variable declaration:
    * {{{val x: Variable[T] = init}}} 
    * becomes
    * {{{
    * val x: Variable[T] = Variable[T]("x")
    * assign(x, constant(init))
    * }}}
    * assuming
    * {{{init: T}}}
    * 
    * @group cstr
    */
  def constant[T](t: T): Processed[T]

  /** Constructs a sequence of multiple statements.
    * 
    * Used in code blocks:
    * {{{
    * \{
    *   val x = ...
    *   while ...
    *   x
    * \}
    * }}}
    * becomes
    * {{{sequence(Seq(|val x = ...|, |while ...|), |x|)}}}
    * 
    * @group cstr
    */
  def sequence[T](fsts: Seq[Processed[Any]], last: Processed[T]): Processed[T]

  /** Constructs an if-then-else.
    * 
    * Used in if-then-else blocks:
    * {{{
    * if x === Constant(0) then
    *   Constant(1)
    * else
    *   Constant(0)
    * }}}
    * becomes
    * {{{ifThenElse(|x === Constant(0)|, |Constant(1)|, |Constant(0)|)}}}
    * 
    * @group cstr
    */
  def ifThenElse[T](cond: Processed[Boolean], thenn: Processed[T], elze: Processed[T]): Processed[T]

  /** Constructs a while-loop.
    * 
    * Used in while-loops:
    *
    * 
    * @note The body of the while-loop does not need to end with an expression;
    * in particular, code such as 
    * {{{
    * while ... do
    *   val x: Variable[Int] = 0
    * }}}
    * in perfectly valid, even if [[exproc.Processor.empty]] is not overriden.
    *  
    * @group cstr
    */
  def whileLoop(cond: Processed[Boolean], body: Processed[Any]): Processed[Unit]

  given autoConstant[T]: Conversion[T, Processed[T]] with
    def apply(t: T) = constant(t)
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
    def isExprOf[S](using Type[S]): Boolean = t.tpe <:< TypeRepr.of[S]

    def isProcessedOf[S](using Type[S]): Boolean = isExprOf[Processed[S]]

    def asProcessedOf[S](using Type[S]): Expr[Processed[S]] = 
      require(isProcessedOf[S])
      t.asExprOf[Processed[S]]

    def isProcessed: Boolean = isProcessedOf[Any]

    def asProcessed: Expr[Processed[Any]] = asProcessedOf[Any]

    def isVariable: Boolean = isExprOf[Variable[Any]]
  }


  object ImplicitConversion {
    def unapply(t: Term): Option[(Term, Term, TypeRepr, TypeRepr)] = t match 
      case a@Apply(Select(conversion, "apply"), arg :: Nil) => 
        val convKind = TypeRepr.of[Conversion]
        val conv = convKind.appliedTo(List(arg.tpe, a.tpe))
        if conversion.tpe <:< conv && conversion.symbol.flags.is(Flags.Given) then
          Some((conversion, arg, arg.tpe, a.tpe))
        else 
         None
      case _ => None
  }

  object BoolUnwrapping {
    def unapply(t: Term): Option[Term] = t match {
      case ImplicitConversion(_, converted, from, to) 
        if from <:< TypeRepr.of[Processed[Boolean]] && to =:= TypeRepr.of[Boolean] => Some(converted)
      case _ => None 
    }
  }

  object InitializerUnwrapping {
    def unapply(t: Term): Option[(Term, Option[Term], TypeRepr, TypeRepr)] = t match {
      case ImplicitConversion(conv, converted, from, to) 
        if to <:< TypeRepr.of[Variable[Any]] => 
          val arg = conv match 
            case Apply(_, arg :: Nil) => Some(arg)
            case _ => None
          Some((converted, arg, from, to))
      case _ => None 
    }
  }

  object AutoConstantUnwrapping {
    def unapply(t: Term): Option[Term] =
      t match 
        case ImplicitConversion(_, converted, from, to)
          if to <:< TypeRepr.of[Processed].appliedTo(from) => Some(converted)
        case _ => None
  }

  object ProcessedParameterTT {
    def unapply(tt: TypeTree): Option[TypeRepr] = 
      ProcessedParameter.unapply(tt.tpe)
  }

  object ProcessedParameter {
    def unapply(tt: TypeRepr): Option[TypeRepr] = 
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
            case expr: Term => Block.copy(t)(List(s), expr)
            case _ => throw ExpressionProcessorImplementationError(s"transformToStatements second statement should be an expression.")
          case _ => throw ExpressionProcessorImplementationError(s"transformToStatements should not return more than two statements.")
      case _ => super.transformTree(t)(owner)

    override def transformStatement(s: Statement)(owner: Symbol) = 
      throw ExpressionProcessorImplementationError("Method should not have been called.")

    def transformAssignee(t: Term)(owner: Symbol): Term = {
      t match 
        case InitializerUnwrapping(init, None, from, _) /*if from <:< TypeRepr.of[Processed[Any]]*/ => init
        case InitializerUnwrapping(init, Some(arg), from, ProcessedParameter(to)) =>
          Select.unique(arg, "apply").appliedTo(transformTerm(init)(owner))
        case _ => transformTerm(t)(owner)
    }.ensuring(_.isProcessed)

    def transformToStatements(s: Statement)(owner: Symbol): List[Statement] = s match 
      case ValDef(name, tt@ProcessedParameterTT(typ), Some(initializer)) =>
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
            init
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
            val vd = ValDef(vs, Some(t))
            val ref = Ref(vs)
            (vd :: statements, ref :: exprs)
          case _ => (statement :: statements, exprs)
      }

    def pushConstant(t: Term)(tpe: TypeRepr): Term = {
      t match
        case _ if t.isProcessed => t
        case If(c@BoolUnwrapping(_), thenn, elze) => If.copy(t)(c, pushConstant(thenn)(tpe), pushConstant(elze)(tpe))
        case Block(statements, term) => Block.copy(t)(statements, pushConstant(term)(tpe))
        case _ => processor.constant(tpe)(t)
    }

    override def transformTerm(t: Term)(owner: Symbol): Term = t match 
      case BoolUnwrapping(t) => 
        report.errorAndAbort(
          s"Invalid conversion to Boolean; it is only allowed in the condition of a if/while.",
          t.pos
        )

      case InitializerUnwrapping(t, _, from, _) =>
        report.errorAndAbort(
          s"Invalid conversion to ${processor.variableType(from)}",
          t.pos
        )
      
      case AutoConstantUnwrapping(a@Assign(lhs, rhs)) if lhs.isVariable => transformTerm(a)(owner)

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
        assert(cond.isProcessedOf[Boolean])
        val pCond = transformTerm(cond)(owner)
        val pThen = pushConstant(transformTerm(thenn)(owner))(t.tpe)
        val pElse = pushConstant(transformTerm(elze)(owner))(t.tpe)
        
        val ProcessedParameter(typ) = pThen.tpe
        processor.ifThenElse(typ)(
          pCond, 
          pThen, 
          pElse
        )

      case While(BoolUnwrapping(cond), body) =>
        assert(cond.isProcessedOf[Boolean])
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

  //println(s"Input:\n${expr.asTerm.show}")
  val r = Transform.transformTerm(expr.asTerm)(Symbol.spliceOwner).asProcessedOf[T]
  //println(s"Processed:\n${Printer.TreeCode.show(r.asTerm)}")
  r
