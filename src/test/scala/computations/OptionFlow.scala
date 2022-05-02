package computations

import exproc.*
import utest.*
import testing.*

object maybe extends ComputationBuilder[Option] with DefaultInit[Option] with DefaultSequence[Option] with NoAssign[Option] {
  transparent inline given ComputationBuilder[Option] = this

  override type Bound = [T] =>> T

  override inline def bind[T, S](inline opt: Option[T], inline f: T => Option[S]) = opt.flatMap(f)
  override inline def unit[T](inline t: => T) = Some(t)
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
        val x: Option[Int] = unit(0)
        x
      }{case 
        Some(0)
      =>}
    }
    test("bind") {
      test("single") {
        maybeAssert{
          val x = ! unit(0)
          unit(x + 1)
        }{case
          Some(1)
        =>}
      }
      test("chain") {
        maybeAssert{
          val x = ! unit(0)
          val y = ! unit(x + 1)
          val z = ! unit(y + 2)
          unit(z)
        }{case 
          Some(3)
        =>}
      }
      test("none") {
        maybeAssert{
          val x = !Option.empty[Int]
          unit(x)
        }{case 
          None
        =>}
      }
      test("chain-none") {
        maybeAssert{
          val x = !unit(0)
          val y = !Option.empty[Int]
          val z = !unit(y + 2)
          unit(z)
        }{case 
          None
        =>}
      }
    }
    test("errors") {
      test("invalid-bang") {
        maybeError{"""maybe{
          unit( !unit(0) )
        }"""}{msg =>
          assert(msg.contains("Invalid"))
          assert(msg.contains("bang (!)"))
        }
      }
    }
  }

}