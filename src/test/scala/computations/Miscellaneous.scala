package computations

import exproc.*
import utest.*
import testing.*

case class Wrapper[T](t: T)

object wrap extends ComputationBuilder[Wrapper] with DefaultInit[Wrapper] with DefaultCombine[Wrapper] {
  transparent inline given ComputationBuilder[Wrapper] = this

  override type Bound = [T] =>> T

  override inline def bind[T, S](inline w: Wrapper[T], inline f: T => Wrapper[S]) = {
    val v = w.t
    f(v)
  }
  override inline def unit[T](inline t: => T) = undefined("wrap does not define unit")

  // Non-sense; just here to do some tests
  override inline def assign[T](inline t: T, inline v: Wrapper[T]) = Wrapper(())
}

object Miscellaneous extends TestSuite {
  import wrap.{*,given}
  import BuilderAssertions.{
    buildMatchAssert => wrapAssert,
    buildCompileError => compileError
  }

  case class WithBang[T](t: T)

  extension [T](w: WithBang[T]) {
    def unary_! : String = w.t.toString
  }

  val tests = Tests {
    test("bang-binder") {
      test("bang-mixup") {
        wrapAssert{
          val x = ! Wrapper(1)
          val y = ! WithBang(42)
          Wrapper(x + y.size)
        }{case 
          Wrapper(3)
        =>}
      }
    }
    test("extra-space") {
      compileError{"""wrap{
        var x = ! Wrapper(0)
        x = ! Wrapper(1)
        Wrapper(x)
      }"""}{ msg =>
        assert(msg.contains("bang (!)"))
        assert(msg.contains("extra space"))  
      }
    }
    test("assign-val") {
      compileError{"""wrap{
        val x = ! Wrapper(0)
        x =! Wrapper(1)
        Wrapper(x)
      }"""}{ msg =>
        assert(msg.contains("Assignation"))
        assert(msg.contains("variable"))
      }
    }
    test("missing-method") {
      test("can-be-ignored") {
        wrapAssert{
          val x = ! Wrapper(0)
          Wrapper(x)
        }{case
          Wrapper(0)
        =>}
      }
      test("cannot-be-used") {
        compileError{"""wrap{
          val x = ! unit(0)
          unit(x)
        }"""}{ msg =>
          assert(msg.contains("not define"))
          assert(msg.contains("unit"))
        }
      }
    }
  }
}