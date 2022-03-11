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
  given variableConstantInitialization[T](using Conversion[T, Processed[T]]): Conversion[T, Variable[T]] = 
    // Using to lower priority
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
  def initializer[T](va: Variable[T], init: Processed[T]): Processed[Unit]

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

    def isProcessed: Boolean = isProcessedOf[Any]
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

  object InitializerUnwrapping {
    def unapply(t: Term): Option[(Term, TypeRepr)] = t match {
      case ImplicitConversion(converted, from, to) 
        if to <:<  TypeRepr.of[Any] => Some((converted, from))
      case _ => None 
    }
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
            case expr: Term => Block(List(s), expr)
            case _ => throw ExpressionProcessorImplementationError(s"transformToStatements second statement should be an expression.")
          case _ => throw ExpressionProcessorImplementationError(s"transformToStatements should not return more than two statements.")
      case _ => super.transformTree(t)(owner)

    override def transformStatement(s: Statement)(owner: Symbol) = 
      throw ExpressionProcessorImplementationError("Method should not have been called.")

    def transformAssignee(t: Term)(owner: Symbol): Term = {
      t match 
        case InitializerUnwrapping(init, from) if from <:< TypeRepr.of[Processed[Any]] => init
        case InitializerUnwrapping(init, from) => processor.constant(from)(transformTerm(init)(owner))
        case _ => transformTerm(t)(owner)
    }.ensuring(_.isProcessed)

    def transformToStatements(s: Statement)(owner: Symbol): List[Statement] = s match 
      case ValDef(name, tt@ProcessedParameterTT(typ), Some(initializer)) =>
        def flagWarning(flag: Flags, name: String): Unit = 
          if s.symbol.flags.is(Flags.Lazy) then 
            report.warning(s"${name.capitalize} is ignored for proto-variable definitions.", s.pos)

        val init = transformAssignee(initializer)(owner)

        flagWarning(Flags.Lazy, "lazy")
        flagWarning(Flags.Inline, "inline")
        flagWarning(Flags.Mutable, "mutable")

        val vd = 
          ValDef.copy(s)(
            name,
            tt,
            Some(processor.variable(typ)('{Identifier(${Expr(name)})}.asTerm))
          )
        List(
          vd,                           // Meta-variable definition
          processor.initializer(typ)(   // Proto-variable definition
            Ref(vd.symbol),
            init
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

      case InitializerUnwrapping(t, from) =>
        report.errorAndAbort(
          s"Invalid conversion to ${processor.variableType(from)}",
          t.pos
        )
      
      case Block(statements, term) =>  
        val last = transformTerm(term)(owner)
        t.tpe match 
          case ProcessedParameter(typ) =>
            val (pStatements, exprs) = splitStatements(statements)(owner)     
            Block(
              pStatements,
              processor.sequence(typ)(
                Expr.ofList(exprs).asTerm, 
                last
              )
            )
          case _ => t

        

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
          case Block(statements, l@Literal(UnitConstant())) =>
            // The trailing unit will be inferred by the compiler in most cases, which breaks everything 
            // if we do not ignore it.
            val (pStatements, exprs) = splitStatements(statements)(owner)
            Block(
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
  val r = Transform.transformTerm(expr.asTerm)(Symbol.spliceOwner).asExprOf[Processed[T]]
  //println(s"Processed:\n${r.asTerm.show}")
  r
