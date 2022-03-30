package computations

import exproc.*
import utest.*
import testing.*

case class Wrapper[T](t: T)

object wrap extends ComputationBuilder[Wrapper] with DefaultRun[Wrapper] {
  transparent inline given ComputationBuilder[Wrapper] = this

  override inline def bind[T, S](inline w: Wrapper[T], inline f: T => Wrapper[S]) = {
    val v = w.t
    f(v)
  }
  override inline def ret[T](inline t: => T) = Wrapper(t)
}

object BangDetection extends TestSuite {
  import wrap.{*,given}
  import BuilderAssertions.{
    buildMatchAssert => wrapAssert,
  }

  case class WithBang[T](t: T)

  extension [T](w: WithBang[T]) {
    def unary_! : String = w.t.toString
  }

  val tests = Tests {
    test("bang-mixup") {
      wrapAssert{
        val x = ! ret(1)
        val y = ! WithBang(42)
        ret(x + y.size)
      }{case 
        Wrapper(3)
      =>}
      
    }
  }
}