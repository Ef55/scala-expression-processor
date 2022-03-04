package exproc

final class Identifier(val name: String) {
  override def toString: String = s"[${name}]" 

  override def equals(other: Any) = 
    other.isInstanceOf[Identifier] &&
    this.eq(other.asInstanceOf[Identifier])
}

object Identifier {
  def unapply(id: Identifier): Option[String] = Some(id.name)
}