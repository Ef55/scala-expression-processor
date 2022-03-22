import scala.compiletime.*
import exproc.*
import utest.*
import scala.collection.mutable.ArrayBuffer

class Logger {
  val buffer = ArrayBuffer.empty[String]

  def log(s: Any): Unit = buffer.addOne(s.toString)
  def get: String = buffer.mkString("\n")
}

object processAssertions {
  inline def processMatchAssert[Processed[+_], Variable[+t] <: Processed[t]]
    (using processor: Processor[Processed, Variable])
    (inline expr: Processed[Any])(inline expected: PartialFunction[Any, Unit]): Unit =
      val result = processor(expr)
      assertMatch( result )( expected )

  inline def processOutAssert[Processed[+_], Variable[+t] <: Processed[t]]
    (using processor: Processor[Processed, Variable])
    (inline expr: (Any => Unit) => Processed[Any])(expected: String): Unit =
      given logger: Logger = Logger()
      processor(expr(logger.log))
      val result = logger.get
      assert(expected == result)

  inline def processCompileError[Processed[+_], Variable[+t] <: Processed[t]]
    (inline expr: String)(cont: String => Unit): Unit =
      val errs = testing.typeCheckErrors(expr)
      assert(!errs.isEmpty)
      errs.foreach(err => cont(err.message))
}