package exproc

import scala.quoted.*


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

trait Unwrap[W[_]] {
  def unapply(using Type[W], Quotes)(tt: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] = 
    import quotes.reflect.*
    val bt = tt.baseType(TypeRepr.of[W].typeSymbol)
    bt match
      case AppliedType(cstr, arg :: Nil) => 
        assert(cstr =:= TypeRepr.of[W])
        Some(arg)
      case _ => None
}

def SubstituteRef(using Quotes)(ref: quotes.reflect.Symbol, replacement: quotes.reflect.Term): quotes.reflect.TreeMap =
  new quotes.reflect.TreeMap {
    import quotes.reflect.*

    override def transformTerm(t: Term)(owner: Symbol): Term = t match 
      case id@Ident(_) if id.symbol == ref => replacement.changeOwner(owner)
      case _ => super.transformTerm(t)(owner)
  }