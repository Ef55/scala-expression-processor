package exproc

import exproc.utils.*
import scala.quoted.*
import scala.annotation.targetName


final case class BuilderImplementationError protected[exproc] (msg: String) extends Error {
  override def toString = msg
}

object ComputationBuilder {
  private final def toBeErased(element: String): Nothing = 
    throw BuilderImplementationError(s"`${element}` should have been erased during processing.")
}
trait ComputationBuilder[Computation[_]] {
  import ComputationBuilder.*

  type Bound[T]
  type Sequence[T] = Seq[T] // Could (should?) be generalized (i.e. abstracted)

  inline def bind[T, S](inline m: Computation[T], inline f: Bound[T] => Computation[S]): Computation[S]

  inline def combine[T, S](inline l: Computation[T], inline r: Computation[S]): Computation[S]

  inline def sequence[T](inline seq: Sequence[Computation[T]]): Computation[T] = seq.reduceLeft(combine(_, _))

  inline def unit[T](inline t: => T): Computation[T]

  inline def init[T](inline c: () => Computation[T]): Computation[T]

  inline def assign[T](inline b: Bound[T], inline v: Computation[T]): Computation[Unit]

  extension [T](c: Computation[T]) {
    @targetName("binder")
    def unary_! : Bound[T] = toBeErased("Bang !")
  }

  extension [T](b: Bound[T]) {
    @targetName("reassign")
    def =! (c: Computation[T]): Computation[Unit] = toBeErased("Reassign =!")
  }

  given binder[T]: Conversion[Computation[T], Bound[T]] =
    toBeErased("Binder")

  final inline def undefined(reason: String): Nothing = scala.compiletime.error(reason)
  final inline def apply[T](inline c: Computation[T]): Computation[T] = buildComputation(this, c)
}

trait DefaultInit[Computation[_]] { self: ComputationBuilder[Computation] =>
  inline def init[T](inline c: () => Computation[T]): Computation[T] = c()
}

trait DefaultSequence[Computation[_]] { self: ComputationBuilder[Computation] =>
  //inline def sequence[T](inline seq: Sequence[Computation[T]]): Computation[T] = seq.reduceLeft(combine(_, _))
  inline def combine[T, S](inline l: Computation[T], inline r: Computation[S]): Computation[S] = { l; r }
}

trait NoAssign[Computation[_]] { self: ComputationBuilder[Computation] => 
  inline def assign[T](inline b: Bound[T], inline v: Computation[T]): Computation[Unit] = undefined("Assignations are not supported.")
}

private inline def buildComputation[Computation[_], T]
(inline builder: ComputationBuilder[Computation], inline computation: Computation[T]): Computation[T] =
  ${buildComputationImpl('{builder}, '{computation})}

private def buildComputationImpl[Computation[_], T]
(builderExpr: Expr[ComputationBuilder[Computation]], computation: Expr[Computation[T]])
(using Type[Computation], Type[T], Quotes): Expr[Computation[T]] = 
  import quotes.reflect.*

  object builder {
    inline def member(name: String): Term = 
      val methds = computationBuilderSymbol.declaredMethod(name)
      assert(methds.length == 1)
      term.select(methds.head)

    val computationBuilderSymbol = TypeRepr.of[ComputationBuilder].typeSymbol

    val bangSymbol = unique(computationBuilderSymbol.declaredMethod("unary_!"))
    val reassignSymbol = unique(computationBuilderSymbol.declaredMethod("=!"))

    val binderSymbol = unique(computationBuilderSymbol.declaredMethod("binder"))

    val term = builderExpr.asTerm

    val bound = selectUniqueType(term, computationBuilderSymbol, "Bound")
    val seq = selectUniqueType(term, computationBuilderSymbol, "Sequence")

    def computation(tpe: TypeRepr): TypeRepr = TypeRepr.of[Computation].appliedTo(tpe)

    def bind(t: TypeRepr, s: TypeRepr)(m: Term, f: Term): Term =
      member("bind").appliedToTypes(List(t, s)).appliedToArgs(List(m, f))

    def combine(t: TypeRepr, s: TypeRepr)(l: Term, r: Term): Term = 
      member("combine").appliedToTypes(List(t, s)).appliedToArgs(List(l, r))

    def sequence(t: TypeRepr)(ls: Term): Term =
      member("sequence").appliedToType(t).appliedTo(ls)

    def init(t: TypeRepr)(c: Term): Term =
      member("init").appliedToType(t).appliedTo(c)

    def assign(t: TypeRepr)(b: Term, v: Term): Term = 
      member("assign").appliedToType(t).appliedToArgs(List(b, v))
  }  

  object BangApplication {
    def unapply(t: Term): Option[Term] = t match
      case Apply(TypeApply(fun, _), c :: Nil) if fun.symbol == builder.bangSymbol =>
        assert(hasBaseType(t.tpe, builder.bound))
        assert(hasBaseType(c.tpe, TypeRepr.of[Computation]))
        Some(c)
      case _ => None
  }

  extension (t: Term) {
    def isComputation: Boolean = hasBaseType(t.tpe, TypeRepr.of[Computation])

    def isComputationSeq: Boolean = 
      unwrap(builder.seq)(t.tpe)
        .map(t => hasBaseType(t, TypeRepr.of[Computation]))
        .getOrElse(false)

    def computationType: TypeRepr =
      assert(isComputation)
      unwrap(TypeRepr.of[Computation])(t.tpe).get

    def computationSeqType: TypeRepr =
      assert(isComputationSeq)
      unwrap(TypeRepr.of[Computation])(unwrap(builder.seq)(t.tpe).get).get
  }

  def buildBlock(sts: List[Statement], last: Term, owner: Symbol) = {
    val (tstatements, refs) = sts.foldRight[(List[Statement], List[Term])]((Nil, Nil)){ 
        case (statement, (statements, exprs)) => statement match
          case t: Term if t.isComputation => 
            val vs = Symbol.newVal(owner, s"computation", t.tpe, Flags.EmptyFlags, Symbol.noSymbol)
            val vd = ValDef(vs, Some(t.changeOwner(vs)))
            val ref = Ref(vs)
            (vd :: statements, ref :: exprs)
          case t: Term if t.isComputationSeq => 
            val vs = Symbol.newVal(owner, s"computationSeq", unwrap(TypeRepr.of[Seq])(t.tpe).get, Flags.EmptyFlags, Symbol.noSymbol)
            val typ = t.computationSeqType
            val vd = ValDef(vs, Some(
              builder.sequence(typ)(t.changeOwner(vs))
            ))
            val ref = Ref(vs)
            (vd :: statements, ref :: exprs)
          case _ => (statement :: statements, exprs)
      }

    Block(
      tstatements,
      (refs :+ last).reduceLeft( (l, r) => builder.combine(l.computationType, r.computationType)(l, r) )
    )
  }

  object TypeBinderApplication {
    def unapply(t: Term): Option[Term] = t match 
      case Apply(Select(conv, "apply"), arg :: Nil) if conv.symbol == builder.binderSymbol => 
        assert(arg.isComputation)
        Some(arg)
      case _ => None
  }

  object Desugar extends TreeMap {
    override def transformTerm(t: Term)(owner: Symbol): Term = t match 
      case TypeBinderApplication(arg) =>
        Ref(builder.bangSymbol)
          .appliedToType(arg.computationType)
          .appliedTo(transformTerm(arg)(owner))
      case _ => 
        if t.symbol == builder.binderSymbol then
          report.warning(
            s"Use of type-binder as value.",
            t.pos
          )
        super.transformTerm(t)(owner)
  }

  object Transform extends TreeMap {

    override def transformTree(t: Tree)(owner: Symbol): Tree = t match
      case t: Term => transformTerm(t)(owner)
      case _ => super.transformTree(t)(owner)

    override def transformStats(s: List[Statement])(owner: Symbol) =
      report.errorAndAbort("Transform (stats) should not have been called.", s.head.pos)

    def transformToStatements(s: Statement)(owner: Symbol)(right: List[Statement], last: Term): (List[Statement], Term) = {
      def flagWarning(flag: Flags, name: String, ctx: String): Unit = 
        if hasFlag(s, flag) then 
          report.warning(s"${name.capitalize} is ignored for ${ctx}.", s.pos)

      s match 
        case ValDef(name, tt, Some(BangApplication(m))) => 
          val initializer = transformTerm(m)(owner)
          
          flagWarning(Flags.Lazy, "lazy", "bind")
          flagWarning(Flags.Inline, "inline", "bind")

          val tType = initializer.computationType

          if !last.isComputation then 
            report.errorAndAbort(
              s"Bind only allows a computation to be returned; got `${Printer.TypeReprCode.show(last.tpe)}`.",
              s.pos
            )
          val sType = last.computationType

          val lmbd = Lambda(
            owner, 
            MethodType(List(name))(
              _ => List(tt.tpe),
              _ => builder.computation(sType)
            ), 
            (sym, args) => {
              val arg :: Nil = args
              SubstituteRef(s.symbol, arg.asInstanceOf[Term]).transformTerm(buildBlock(right, last, owner))(sym)
            }
          )

          val r = builder.bind(tType, sType)(initializer, lmbd)

          (List.empty, r)

        case _ => (super.transformStatement(s)(owner) +: right, last)

    }//.ensuring(res => last.tpe =:= res._2.tpe)

    def transformToStats(ls: List[Statement], last: Term)(owner: Symbol): (List[Statement], Term) =
      ls.foldRight((List.empty[Statement], transformTerm(last)(owner))){ case (l, (right, last)) => transformToStatements(l)(owner)(right, last) }

    override def transformTerm(t: Term)(owner: Symbol): Term = t match 

      case b@BangApplication(_) =>
        report.errorAndAbort(
          s"Invalid use of bang (!): it can only be used at the top-level of a value definition.",
          t.pos
        )

      case Assign(assignee, BangApplication(v)) => 
        report.errorAndAbort(
          s"Invalid use of bang (!): it can only be used at the top-level of a value definition. Did you insert an extra space ? (`= !` ==> `=!`)",
          t.pos
        )

      case Apply(Apply(TypeApply(f, typArgs), args1), args2) if f.symbol == builder.reassignSymbol =>
        assert(typArgs.length == 1)
        val vr :: Nil = args1
        val vl :: Nil = args2
        if !hasFlag(vr, Flags.Mutable) then 
          report.errorAndAbort(
            s"Assignation can only be done on variables.",
            vr.pos
          )
        val typ = vl.computationType
        builder.assign(typ)(
          transformTerm(vr)(owner),
          transformTerm(vl)(owner)
        )

      case Block(statements, term) =>  
        val (sts, last) = transformToStats(statements, term)(owner)  

        buildBlock(sts, last, owner)

      case _ => super.transformTerm(t)(owner)
  }

  val owner = Symbol.spliceOwner
  val transformed = chain(Desugar, Transform)(computation.asTerm, owner)
  val retType = transformed.computationType
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