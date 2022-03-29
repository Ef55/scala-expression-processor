package computations

import exproc.*
import utest.*
import testing.*

object maybe extends ComputationBuilder[Option] with DefaultRun[Option] {
  transparent inline given ComputationBuilder[Option] = this

  override inline def bind[T, S](inline opt: Option[T], inline f: T => Option[S]) = opt.flatMap(f)
  override inline def ret[T](inline t: => T) = Some(t)
}

object OptionFlow extends TestSuite {
  import maybe.{*,given}
  import BuilderAssertions.{
    buildMatchAssert => maybeAssert,
    buildCompileError => maybeError
  }

  val tests = Tests {
    test("basic-let") {
      maybeAssert{
        val x: Option[Int] = ret(0)
        x
      }{case 
        Some(0)
      =>}
    }
    test("bind") {
      test("type-indication") {
        test("single") {
          maybeAssert{
            val my: Option[Int] = ret(0)
            val y: Int = my
            ret(y + 1)
          }{case 
            Some(1)
          =>}
        }
        test("chain") {
          maybeAssert{
            val x: Int = ret(0)
            val y: Int = ret(x + 1)
            val z: Int = ret(y + 2)
            ret(z)
          }{case 
            Some(3)
          =>}
        }
        test("none") {
          maybeAssert{
            val x: Int = None
            ret(x)
          }{case 
            None
          =>}
        }
        test("chain-none") {
          maybeAssert{
            val x: Int = ret(0)
            val y: Int = None
            val z: Int = ret(y + 2)
            ret(z)
          }{case 
            None
          =>}
        }
      }
      test("operator-indication") {
        test("single") {
          maybeAssert{
            val x = ! ret(0)
            ret(x + 1)
          }{case
            Some(1)
          =>}
        }
        test("chain") {
          maybeAssert{
            val x = ! ret(0)
            val y = ! ret(x + 1)
            val z = ! ret(y + 2)
            ret(z)
          }{case 
            Some(3)
          =>}
        }
        test("none") {
          maybeAssert{
            val x = !Option.empty[Int]
            ret(x)
          }{case 
            None
          =>}
        }
        test("chain-none") {
          maybeAssert{
            val x = !ret(0)
            val y = !Option.empty[Int]
            val z = !ret(y + 2)
            ret(z)
          }{case 
            None
          =>}
        }
      }
    }
    test("errors") {
      test("bind-ret") {
        maybeError{"""maybe{
          def f(o: Option[Int]): Int =
            val x: Int = o
            x + 1

          ret(f(None))
        }"""}{msg =>
          assert(msg.contains("Bind"))
          assert(msg.contains("scala.Option"))
        }
      }
      test("invalid-bang") {
        maybeError{"""maybe{
          ret( !ret(0) )
        }"""}{msg =>
          assert(msg.contains("Invalid"))
          assert(msg.contains("bang (!)"))
        }
      }
    }
  }

}