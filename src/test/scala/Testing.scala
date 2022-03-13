import scala.compiletime.*
import exproc.*
import utest.*


object processAssertions {
  inline def processMatchAssert[Processed[+_], Variable[+t] <: Processed[t]]
    (using processor: Processor[Processed, Variable])
    (inline expr: Processed[Any])(inline expected: PartialFunction[Any, Unit]): Unit =
      val result = processor(expr)
      assertMatch( result )( expected )

  inline def processCompileError[Processed[+_], Variable[+t] <: Processed[t]]
    (inline expr: String)(cont: String => Unit): Unit =
      testing.typeCheckErrors(expr).foreach(err => cont(err.message))
}