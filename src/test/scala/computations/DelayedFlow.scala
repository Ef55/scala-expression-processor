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

  override type Bound = [T] =>> T

  override inline def bind[T, S](inline d: Lazy[T], inline f: T => Lazy[S]) = d.flatMap(f)
  override inline def sequence[T, S](inline l: Lazy[T], inline r: Lazy[S]) = l.flatMap(_ => r)
  override inline def unit[T](inline t: => T) = Lazy(t)
  override inline def init[T](inline c: () => Lazy[T]) = Lazy(()).flatMap(_ => c())
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
    test("delay") {
      delayedAssert{ log =>
        unit(log(0))
      }{ msg =>
        assert(msg == "0")
      }
    }
    test("toplevel-delay") {
      delayedAssert{ log =>
        log(0)
        unit(0)
      }{ msg =>
        assert(msg == "0")
      }
    }
    test("sequencing") {
      delayedAssert{ log => 
        log(0)
        log(1)
        unit(0)
      }{ msg => 
        assert(msg == "0\n1")
      }
    }
  }
}