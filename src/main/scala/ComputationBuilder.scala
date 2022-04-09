package exproc

import scala.quoted.*
import exproc.utils.*


trait ComputationBuilder[Computation[_]] extends Builder[Computation] {
  private final def conversionToBeErased(element: String): Nothing = 
    throw ExpressionProcessorImplementationError(s"The given ${element} should have been erased during processing.")


  inline def bind[T, S](inline m: Computation[T], inline f: T => Computation[S]): Computation[S]

  inline def unit[T](inline t: => T): Computation[T]

  inline def init[T](inline c: () => Computation[T]): Computation[T]

  given binder[T]: Conversion[Computation[T], T] =
    conversionToBeErased("Conversion[Computation[T], T]")

  extension [T](c: Computation[T]) {
    def unary_! : T = binder(c)
  }

  final inline def undefined(reason: String): Nothing = scala.compiletime.error(reason)
  final inline def apply[T](inline c: Computation[T])(using config: BuilderConfig): Computation[T] = buildComputation(this)(c)
}

trait DefaultInit[Computation[_]] { self: ComputationBuilder[Computation] =>
  inline def init[T](inline c: () => Computation[T]): Computation[T] = c()
}

private inline def buildComputation[Computation[_], T]
(inline builder: ComputationBuilder[Computation])
(inline computation: Computation[T]): Computation[T] =
  ${buildComputationImpl('{builder}, '{computation})}

private def buildComputationImpl[Computation[_], T]
(builderExpr: Expr[ComputationBuilder[Computation]], computation: Expr[Computation[T]])
(using Type[Computation], Type[T], Quotes): Expr[Computation[T]] = 
  import quotes.reflect.*

  object builder {
    private inline def member(name: String): Term = 
      val methds = symbol.declaredMethod(name)
      assert(methds.length == 1)
      term.select(methds.head)

    val symbol = TypeRepr.of[ComputationBuilder].typeSymbol
    val term = builderExpr.asTerm

    def computation(tpe: TypeRepr): TypeRepr = TypeRepr.of[Computation].appliedTo(tpe)

    def bind(t: TypeRepr, s: TypeRepr)(m: Term, f: Term): Term =
      member("bind").appliedToTypes(List(t, s)).appliedToArgs(List(m, f))

    def init(t: TypeRepr)(c: Term): Term =
      member("init").appliedToType(t).appliedTo(c)
  }  

  object Binder extends ImplicitUnwrapper[Computation]

  object ComputationTR extends Unwrap[Computation]

  object MaybeComputationTR {
    def unapply(tpe: TypeRepr): Option[(TypeRepr, Boolean)] = 
      ComputationTR.unapply(tpe).map(t => (t, true))
        .orElse(Some((tpe, false)))
  }

  object BangApplication {
    def unapply(t: Term): Option[Term] = t match
      case Apply(TypeApply(fun, _), c :: Nil) if fun.symbol.name == "unary_!" =>
        val ftype = c.tpe
        val ttype = t.tpe
        if hasBaseType(ftype, TypeRepr.of[Computation]) && ftype <:< TypeRepr.of[Computation].appliedTo(ttype) then
          Some(c)
        else 
          None
      case _ => None
  }

  object Transform extends TreeMap {

    override def transformTree(t: Tree)(owner: Symbol): Tree = t match
      case t: Term => transformTerm(t)(owner)
      case _ => super.transformTree(t)(owner)

    override def transformStats(s: List[Statement])(owner: Symbol) =
      report.errorAndAbort("Transform (stats) should not have been called.", s.head.pos)

    def transformToStatements(s: Statement)(owner: Symbol)(right: List[Statement], last: Term): (List[Statement], Term) = {
      def flagWarning(flag: Flags, name: String, ctx: String): Unit = 
        if s.symbol.flags.is(flag) then 
          report.warning(s"${name.capitalize} is ignored for ${ctx}.", s.pos)

      def flagError(flag: Flags, name: String, ctx: String): Unit = 
        if s.symbol.flags.is(flag) then 
          report.errorAndAbort(s"${name.capitalize} is invalid for ${ctx}.", s.pos)

      s match 
        case ValDef(name, tt, Some(m @ (Binder(_) | BangApplication(_)))) => 
          val initializer = m match 
            case Binder(m) => transformTerm(m)(owner)
            case BangApplication(m) => transformTerm(m)(owner)

          flagWarning(Flags.Lazy, "lazy", "bind")
          flagWarning(Flags.Inline, "inline", "bind")
          flagError(Flags.Mutable, "var", "bind")

          val tType = tt.tpe
          val MaybeComputationTR(sType, valid) = last.tpe

          if !valid then
            report.errorAndAbort(
              s"Bind only allowed when ${TypeRepr.of[Computation].typeSymbol.fullName}[_] is expected.",
              s.pos
            )

          val lmbd = Lambda(
            owner, 
            MethodType(List(name))(
              _ => List(tType),
              _ => builder.computation(sType)
            ), 
            (sym, args) => {
              val arg :: Nil = args
              SubstituteRef(s.symbol, arg.asInstanceOf[Term]).transformTerm(Block(right, last))(sym)
            }
          )

          (List.empty, builder.bind(tType, sType)(initializer, lmbd))

        case _ => (super.transformStatement(s)(owner) +: right, last)

    }.ensuring(res => last.tpe =:= res._2.tpe)

    def transformToStats(ls: List[Statement], last: Term)(owner: Symbol): (List[Statement], Term) =
      ls.foldRight((List.empty[Statement], transformTerm(last)(owner))){ case (l, (right, last)) => transformToStatements(l)(owner)(right, last) }

    override def transformTerm(t: Term)(owner: Symbol): Term = t match 
      case b@Binder(_) =>
        report.errorAndAbort(
          s"Invalid conversion to ${Printer.TypeReprCode.show(b.tpe)}",
          t.pos
        )

      case b@BangApplication(_) =>
        report.errorAndAbort(
          s"Invalid use of bang (!): it can only be used at the top-level of a value definition.",
          t.pos
        )

      case Block(statements, term) =>  
        val (sts, last) = transformToStats(statements, term)(owner)  
        Block.copy(t)(
          sts,
          last
        )


      case _ => super.transformTerm(t)(owner)
  }

  val owner = Symbol.spliceOwner
  val transformed = Transform.transformTerm(computation.asTerm)(owner)
  val ComputationTR(retType) = transformed.tpe
  val lmbd = Lambda(
      owner, 
      MethodType(List())(
        _ => List(),
        _ => builder.computation(retType)
      ), 
      (sym, _) => transformed.changeOwner(sym)
    )
  val r = builder.init(retType)(lmbd)
  // println("=======")
  // println(s"Got: ${Printer.TreeCode.show(computation.asTerm)}")
  // println(s"Built: ${Printer.TreeCode.show(r)}")
  r.asExprOf[Computation[T]]