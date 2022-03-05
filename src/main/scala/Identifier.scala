package exproc

/** Wraps a string in a uniquely-identified object.
  *
  * {{{
  * scala> import exproc.*
  * scala> import Identifier("id") == Identifier("id")
  * val res0: Boolean = false
  * scala> val id1 = Identifier("id")
  * val id1: exproc.Identifier = [id]
  * scala> id1 == id1
  * val res1: Boolean = true
  * }}}
  */ 
final class Identifier(val name: String) {
  override def toString: String = s"[${name}]" 

  override def equals(other: Any) = 
    other.isInstanceOf[Identifier] &&
    this.eq(other.asInstanceOf[Identifier])
}

object Identifier {
  /** Retrieves the string inside an identifier */
  def unapply(id: Identifier): Option[String] = Some(id.name)
}