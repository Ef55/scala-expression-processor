package exproc

import exproc.utils.*

trait BuilderConfig {
  val printCode: Boolean
}

object BuilderConfig {
  given default: BuilderConfig with
    val printCode = false

  object WithPrint extends BuilderConfig {
    override val printCode = true
  }
}

final case class BuilderImplementationError protected (msg: String) extends Error {
  override def toString = msg
}

trait Builder[Result[_]] {
  inline def apply[T](inline expr: Result[T])(using config: BuilderConfig): Result[T]
}