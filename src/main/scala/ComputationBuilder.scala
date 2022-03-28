package exproc

import scala.quoted.*


trait ComputationBuilder[Computation[_]] extends Builder[Computation] {
  private final def conversionToBeErased(element: String): Nothing = 
    throw ExpressionProcessorImplementationError(s"The given ${element} should have been erased during processing;.")


  def bind[T, S](m: Computation[T], f: T => Computation[S]): Computation[S]

  def ret[T](t: T): Computation[T]

  given binder[T]: Conversion[Computation[T], T] =
    conversionToBeErased("Conversion[Processed[T], T]")

  final inline def apply[T](inline c: Computation[T])(using config: BuilderConfig): Computation[T] = buildComputation(this)(c)
}


private inline def buildComputation[Computation[_], T]
(builder: ComputationBuilder[Computation])
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


  object Binder {
    def unapply(t: Term): Option[Term] =
      t match 
        case ImplicitConversion(_, converted, from, to)
          if from <:< TypeRepr.of[Computation].appliedTo(to) => Some(converted)
        case _ => None
  }

  object Run {
    def unapply(tpe: TypeRepr): Option[TypeRepr] = 
      val bt = tpe.baseType(TypeRepr.of[Computation].typeSymbol)
      bt match
        case AppliedType(cstr, arg :: Nil) => 
          assert(cstr =:= TypeRepr.of[Computation])
          Some(arg)
        case _ => None
  }

  object RunIf {
    def unapply(tpe: TypeRepr): Option[(TypeRepr, Boolean)] = 
      Run.unapply(tpe).map(t => (t, true))
        .orElse(Some((tpe, false)))
  }

  final class SubstituteRef(rem: Symbol, add: Term) extends TreeMap {
    override def transformTerm(t: Term)(owner: Symbol): Term = t match 
      case _ if t.symbol == rem => add.changeOwner(owner)
      case _ => super.transformTerm(t)(owner)
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
        case ValDef(name, tt, Some(Binder(m))) => 
          val init = transformTerm(m)(owner)

          flagWarning(Flags.Lazy, "lazy", "bind")
          flagWarning(Flags.Inline, "inline", "bind")
          flagError(Flags.Mutable, "var", "bind")

          val tType = tt.tpe
          val RunIf(sType, run) = last.tpe

          if !run then
            report.errorAndAbort(
              s"Bind only allowed when ${TypeRepr.of[Computation].typeSymbol.fullName}[_] is expected.",
              s.pos
            )

          val lmbd = Lambda(
            owner, 
            MethodType(List("x"))(
              _ => List(tType),
              _ => builder.computation(sType)
            ), 
            (_, args) => {
              val arg :: Nil = args
              SubstituteRef(s.symbol, arg.asInstanceOf[Term]).transformTerm(Block(right, last))(owner)
            }
          )

          (List.empty, builder.bind(tType, sType)(init, lmbd))

        case _ => (super.transformStatement(s)(owner) +: right, last)

    }.ensuring(res => last.tpe =:= res._2.tpe)

    def transformToStats(ls: List[Statement], last: Term)(owner: Symbol): (List[Statement], Term) =
      ls.foldRight((List.empty[Statement], last)){ case (l, (right, last)) => transformToStatements(l)(owner)(right, last) }

    override def transformTerm(t: Term)(owner: Symbol): Term = t match 
      case b@Binder(monad) =>
        report.errorAndAbort(
          s"Invalid conversion to ${Printer.TypeReprCode.show(b.tpe)}",
          t.pos
        )

      case Block(statements, term) =>  
        val (sts, t) = transformToStats(statements, term)(owner)  
        Block.copy(t)(
          sts,
          t
        )


      case _ => super.transformTerm(t)(owner)
  }

  val r = Transform.transformTerm(computation.asTerm)(Symbol.spliceOwner).asExprOf[Computation[T]]
  //println(s"Built: ${Printer.TreeCode.show(r.asTerm)}")
  r