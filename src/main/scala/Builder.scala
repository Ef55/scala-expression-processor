package exproc

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

trait Builder[Result[_]] {
  inline def apply[T](inline expr: Result[T])(using config: BuilderConfig): Result[T]
}