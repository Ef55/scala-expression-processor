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
  (using processor: Processor[Processed, Variable])
  (inline expr: Any)(cont: String => Unit): Unit =
    inline val code = "math{" + codeOf(expr) + "}"
    assertMatch(compileError(code)){ 
      case CompileError.Type(_, msg) => cont(msg)
    }
}