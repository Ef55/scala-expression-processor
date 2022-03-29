package computations

import exproc.*
import utest.*
import testing.*

object maybe extends ComputationBuilder[Option] {
  given ComputationBuilder[Option] = this

  override def bind[T, S](opt: Option[T], f: T => Option[S]) = opt.flatMap(f)
  override def ret[T](t: => T) = Some(t)
}

object OptionFlow extends TestSuite {
  import maybe.{*,given}
  import builderAssertions.{
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
    }
  }

}