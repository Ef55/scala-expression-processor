package exproc

import scala.quoted.*

def hasBaseType(using Quotes)(tpe: quotes.reflect.TypeRepr, base: quotes.reflect.TypeRepr): Boolean =
  tpe.baseType(base.typeSymbol).typeSymbol.isType

object ImplicitConversion {
  def unapply(using Quotes)(t: quotes.reflect.Term): Option[(quotes.reflect.Term, quotes.reflect.Term, quotes.reflect.TypeRepr, quotes.reflect.TypeRepr)] =
    import quotes.reflect.*
    t match 
      case a@Apply(Select(conversion, "apply"), arg :: Nil) => 
        val convKind = TypeRepr.of[Conversion]
        val conv = convKind.appliedTo(List(arg.tpe, a.tpe))
        if conversion.tpe <:< conv && conversion.symbol.flags.is(Flags.Given) then
          Some((conversion, arg, arg.tpe, a.tpe))
        else 
        None
      case _ => None
}

trait SpecificImplicitConversion[F, T] {
  def unapply(using Type[F], Type[T], Quotes)(t: quotes.reflect.Term): Option[(quotes.reflect.Term, quotes.reflect.Term)] =
    import quotes.reflect.*
    t match 
      case ImplicitConversion(conversion, converted, from, to)
        if from <:< TypeRepr.of[F] && to <:< TypeRepr.of[T] => Some((conversion, converted))
      case _ => None
}

trait ImplicitUnwrapper[W[_]] {
  def unapply(using Type[W], Quotes)(t: quotes.reflect.Term): Option[quotes.reflect.Term] =
    import quotes.reflect.*
    t match 
      case ImplicitConversion(_, converted, from, to)
        if from <:< TypeRepr.of[W].appliedTo(to) => Some(converted)
      case _ => None
}

trait ImplicitWrapper[W[_]] {
  def unapply(using Type[W], Quotes)(t: quotes.reflect.Term): Option[quotes.reflect.Term] =
    import quotes.reflect.*
    t match 
      case ImplicitConversion(_, converted, from, to)
        if to <:< TypeRepr.of[W].appliedTo(from) => Some(converted)
      case _ => None
}

trait Unwrap[W[_]] { self =>
  def unapply(using Type[W], Quotes)(tt: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] = 
    import quotes.reflect.*
    val bt = tt.baseType(TypeRepr.of[W].typeSymbol)
    bt match
      case AppliedType(cstr, arg :: Nil) => 
        assert(cstr =:= TypeRepr.of[W])
        Some(arg)
      case _ => None

  object TypeTree {
    def unapply(using Type[W], Quotes)(tt: quotes.reflect.TypeTree): Option[quotes.reflect.TypeRepr] = 
      self.unapply(tt.tpe)
  }
}

def SubstituteRef(using Quotes)(ref: quotes.reflect.Symbol, replacement: quotes.reflect.Term): quotes.reflect.TreeMap =
  new quotes.reflect.TreeMap {
    import quotes.reflect.*

    override def transformTerm(t: Term)(owner: Symbol): Term = t match 
      case id@Ident(_) if id.symbol == ref => replacement.changeOwner(owner)
      case _ => super.transformTerm(t)(owner)
  }