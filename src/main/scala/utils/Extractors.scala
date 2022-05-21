package exproc.utils

import scala.quoted.*

def hasBaseType(using Quotes)(tpe: quotes.reflect.TypeRepr, base: quotes.reflect.TypeRepr): Boolean =
  tpe.baseType(base.typeSymbol).typeSymbol.isType

def unwrap(using Quotes)(wrapper: quotes.reflect.TypeRepr)(tt: quotes.reflect.TypeRepr): Option[quotes.reflect.TypeRepr] = {
  import quotes.reflect.*
  val bt = tt.baseType(wrapper.typeSymbol)
  bt match
    case AppliedType(cstr, arg :: Nil) => 
      assert(cstr =:= wrapper)
      Some(arg)
    case _ => None
}

object Conversion {
  def unapply(using Quotes)(t: quotes.reflect.Term): Option[(quotes.reflect.Term, quotes.reflect.Term)] =
    import quotes.reflect.*
    t match 
      case Apply(f@Select(c, "apply"), t :: Nil) if hasBaseType(c.tpe, TypeRepr.of[scala.Conversion]) =>
        Some((f, t))
      case _ => None
}