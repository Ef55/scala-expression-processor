package computations

import exproc.*
import utest.*
import testing.*

class Lazy[T](t: => T) {

  def kick: T = t

  def flatMap[S](f: T => Lazy[S]): Lazy[S] =
    Lazy(f(t).kick)
}

object delayed extends ComputationBuilder[Lazy] {
  given ComputationBuilder[Lazy] = this

  override def bind[T, S](d: Lazy[T], f: T => Lazy[S]) = d.flatMap(f)
  override def ret[T](t: => T) = Lazy(t)

  override def run[T](c: () => Lazy[T]) = Lazy(()).flatMap(_ => c())
}

inline def delayedAssert[T](inline expr: (Any => Unit) => Lazy[T])(computeTest: String => Unit): Unit =
  val logger: Logger = Logger()
  val computation = delayed(expr(logger.log))
  assert(logger.get.isEmpty)
  logger.clear
  val r = computation.kick
  computeTest(logger.get)

object DelayedFlow extends TestSuite {
  import delayed.{*,given}
  
  val tests = Tests {
    test("run-delay") {
      delayedAssert{ log =>
        log(0)
        ret(0)
      }{ msg =>
        assert(msg == "0")
      }
    }
    test("delayed-log") {
      delayedAssert{ log =>
        ret(log(0))
      }{ msg =>
        assert(msg == "0")
      }
    }
  }
}