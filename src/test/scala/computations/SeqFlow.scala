package computations

import exproc.*
import utest.*
import testing.*

import scala.collection.immutable.Seq
import scala.compiletime.summonInline

object seq extends ComputationBuilder[Seq] with DefaultInit[Seq] with NoAssign[Seq] {
  transparent inline given ComputationBuilder[Seq] = this

  override type Bound = [T] =>> T

  override inline def bind[T, S](inline s: Seq[T], inline f: T => Seq[S]) = s.flatMap(f)
  override inline def sequence[T, S](inline l: Seq[T], inline r: Seq[S]) = {
    val conv = summonInline[<:<[T, S]].liftCo[Seq]
    conv(l) ++ r
  }
  override inline def unit[T](inline t: => T) = Seq(t)
}

object DumbHierarchy {
  class A 
  class B extends A

  object A {
    def unapply(a: A): Boolean = true
  }

  object B {
    def unapply(b: B): Boolean = true
  }
}

object SeqFlow extends TestSuite {
  import seq.{*,given}
  import BuilderAssertions.{
    buildMatchAssert => seqAssert,
    buildCompileError => seqError
  }

  import DumbHierarchy.*

  val tests = Tests {
    test("sequencing") {
      seqAssert{
        Seq(new B())
        Seq(new A())
      }{case 
        Seq(B(), A())
      =>}
    }
    test("invalid-sequencing") {
      seqError{"""seq{
        Seq(1)
        Seq(2.0)
      }"""}{msg =>
        assert(msg.contains("Int <:< Double"))  
      }
    }
  }

}