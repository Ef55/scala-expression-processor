import utest.*
import exproc.*

sealed trait MathExpr[+T]
case class Variable[+T](name: String) extends MathExpr[T]
case class Constant(i: Int) extends MathExpr[Int]
case class Assign[T](to: Variable[T], v: MathExpr[T]) extends MathExpr[Unit]
case class Plus(lhs: MathExpr[Int], rhs: MathExpr[Int]) extends MathExpr[Int]
case class Minus(lhs: MathExpr[Int], rhs: MathExpr[Int]) extends MathExpr[Int]
case class Sequence[+T](first: MathExpr[?], second: MathExpr[T]) extends MathExpr[T]
case class If[+T](cond: MathExpr[Boolean], thenn: MathExpr[T], elze: MathExpr[T]) extends MathExpr[T]
case class While(cond: MathExpr[Boolean], body: MathExpr[Any]) extends MathExpr[Unit]
case class Eq[+T](lhs: MathExpr[T], rhs: MathExpr[T]) extends MathExpr[Boolean]

extension [T](v: Variable[T]) {
  def :=(m: MathExpr[T]) = Assign(v, m)
}

extension (m: MathExpr[Int]) {
  def +(n: MathExpr[Int]) = Plus(m, n)
  def -(n: MathExpr[Int]) = Minus(m, n)
}

extension [T](m: MathExpr[T]) {
  def ===(n: MathExpr[T]) = Eq(m, n)
}

object MathProcessor extends Processor[MathExpr, Variable] {

  override def variable[T](name: String): Variable[T] = Variable[T](name)
  override def initializer[T](va: Variable[T], init: MathExpr[T]) = va := init
  override def constant[T](t: T) = t match {
    case i: Int => Constant(i).asInstanceOf[MathExpr[T]]
  }
  override def sequence[T](ls: Seq[MathExpr[Any]], last: MathExpr[T]) = ls.foldRight(last)(Sequence(_, _))
  override def ifThenElse[T](cond: MathExpr[Boolean], thenn: MathExpr[T], elze: MathExpr[T]) = If(cond, thenn, elze)
  override def whileLoop(cond: MathExpr[Boolean], body: MathExpr[Any]) = While(cond, body)

  given Conversion[Boolean, MathExpr[Int]] with
    def apply(b: Boolean) = if b then Constant(1) else Constant(0)
}

inline def math[T](inline expr: MathExpr[T]): MathExpr[T] = MathProcessor(expr)

inline def mathAssert[T](inline expr: MathExpr[T])(expected: MathExpr[T]): Unit = 
  val result = math(expr)
  assert( result == expected )

object MathProcessorTests extends TestSuite {
  import MathProcessor.{*,given}

  val tests = Tests {
    test("DSL") {
      test("equality") {
        mathAssert{
          val x: Variable[Int] = 0
          x === Constant(0)
        }(Sequence(
          Assign(Variable("x"), Constant(0)),
          Eq(Variable("x"), Constant(0))
        ))
      }
      test("assignation") {
        mathAssert{
          Variable("x") := Variable("y") + Constant(5)
        }(Assign(Variable("x"), Plus(Variable("y"), Constant(5))))
      }
      test("assignation-with-val") {
        mathAssert{
          val x: Variable[Int] = -1
          val y: Variable[Int] = 0
          Variable("x") := Variable("y") + Constant(5)
        }(Sequence(
          Assign(Variable("x"), Constant(-1)), Sequence(
          Assign(Variable("y"), Constant(0)),
          Assign(Variable("x"), Plus(Variable("y"), Constant(5)))
        )))
      }
      test("bool-to-constant") {
        mathAssert{
          val x: Variable[Int] = 0
          x := true
        }(
          Sequence(
            Assign(Variable("x"), Constant(0)),
            Assign(Variable("x"), Constant(1))
          )
        )
      }
    }
    test("sequencing") {
      mathAssert{
        val x: Variable[Int] = 0
        Variable("x") := Constant(1)
      }(
        Sequence(
          Assign(Variable("x"), Constant(0)),
          Assign(Variable("x"), Constant(1))
        )
      )
    }
    test("proto-control-flow") {
      test("if-then-else") {
        mathAssert{
          val x: Variable[Int] = 0
          if x === Constant(-1) then
            Constant(0)
          else
            Constant(1)
        }(Sequence(
          Assign(Variable("x"), Constant(0)),
          If(
            Eq(Variable("x"), Constant(-1)),
            Constant(0),
            Constant(1)
          )
        ))
      }
      test("while") {
        mathAssert{
          val x: Variable[Int] = 10
          while x === Constant(0) do
            x := x  - Constant(3)
          x
        }(Sequence(
            Assign(Variable("x"), Constant(10)), Sequence(
            While(
              Eq(Variable("x"), Constant(0)),
              Assign(Variable("x"), Minus(Variable("x"), Constant(3)))
            ),
            Variable("x")
        )))
      }
    }
    test("runtime-errors") {
      test("bool-unwrap-abuse") {
        val e = intercept[FeatureMisuseException](math{
          val x: Variable[Int] = 0
          if x === Constant(0) then Constant(0) else Constant(1)
          val b: Boolean = x === Constant(0)
        })
        assert(e.msg.contains("should have been erased"))
      }
      test("initializer-abuse") {
        val e = intercept[FeatureMisuseException](math{
          val x: Variable[Int] = 0
          x := 1
        })
        assert(e.msg.contains("should have been erased"))
      }
      test("top-level-initializer") {
        val e = intercept[FeatureMisuseException](math{
          ()
        })
        assert(e.msg.contains("should have been erased"))
      }
    }
  }

}