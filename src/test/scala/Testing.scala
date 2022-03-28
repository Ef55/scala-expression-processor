import scala.compiletime.*
import exproc.*
import utest.*
import scala.collection.mutable.ArrayBuffer

class Logger {
  val buffer = ArrayBuffer.empty[String]

  def log(s: Any): Unit = buffer.addOne(s.toString)
  def get: String = buffer.mkString("\n")
}

object builderAssertions {
  inline def buildMatchAssert[Result[_], T]
    (using builder: Builder[Result])
    (inline expr: Result[T])(inline expected: PartialFunction[Any, Unit]): Unit =
      val result = builder(expr)
      assertMatch( result )( expected )

  inline def buildOutAssert[Result[_], T]
    (using builder: Builder[Result])
    (inline expr: (Any => Unit) => Result[T])(expected: String): Unit =
      given logger: Logger = Logger()
      builder(expr(logger.log))
      val result = logger.get
      assert(expected == result)

  inline def buildCompileError[Result[_]]
    (inline expr: String)(cont: String => Unit): Unit =
      val errs = testing.typeCheckErrors(expr)
      assert(!errs.isEmpty)
      errs.foreach(err => cont(err.message))
}