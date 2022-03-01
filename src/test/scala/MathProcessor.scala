import utest.*

import exproc.*

type MathType = Int | Boolean

sealed trait MathExpr[+T]
case class Variable(name: String) extends MathExpr[Int]
case class Constant(i: Int) extends MathExpr[Int]
case class Assign(to: Variable, v: MathExpr[Int]) extends MathExpr[Int]
case class Plus(lhs: MathExpr[Int], rhs: MathExpr[Int]) extends MathExpr[Int]
case class Minus(lhs: MathExpr[Int], rhs: MathExpr[Int]) extends MathExpr[Int]
case class Sequence[T](first: MathExpr[?], second: MathExpr[T]) extends MathExpr[T]
case class If[T](cond: MathExpr[Boolean], thenn: MathExpr[T], elze: MathExpr[T]) extends MathExpr[T]
case class While(cond: MathExpr[Boolean], body: MathExpr[Any]) extends MathExpr[Unit]
case class Eq[T](lhs: MathExpr[T], rhs: MathExpr[T]) extends MathExpr[Boolean]

extension (v: Variable) {
  def :=(m: MathExpr[Int]) = Assign(v, m)
}

extension (m: MathExpr[Int]) {
  def +(n: MathExpr[Int]) = Plus(m, n)
  def -(n: MathExpr[Int]) = Minus(m, n)
}

extension [T](m: MathExpr[T]) {
  def ===(n: MathExpr[T]) = Eq(m, n)
}

object MathProcessor extends Processor[MathExpr] {
  override def sequence[T](ls: Seq[MathExpr[Any]], last: MathExpr[T]) = ls.foldRight(last)(Sequence(_, _))
  override def ifThenElse[T](cond: MathExpr[Boolean], thenn: MathExpr[T], elze: MathExpr[T]) = If(cond, thenn, elze)
  override def whileLoop(cond: MathExpr[Boolean], body: MathExpr[Any]) = While(cond, body)
}

inline def math[T](inline expr: MathExpr[T]): MathExpr[T] = MathProcessor(expr)

inline def mathAssert[T](inline expr: MathExpr[T])(expected: MathExpr[T]): Unit = 
  val result = math(expr)
  assert( result == expected )

object MathProcessorTests extends TestSuite {
  import MathProcessor.given

  val tests = Tests {
    test("DSL") {
      test("equality") {
        mathAssert{
          Variable("x") === Constant(0)
        }(Eq(Variable("x"), Constant(0)))
      }
      test("assignation") {
        mathAssert{
          Variable("x") := Variable("y") + Constant(5)
        }(Assign(Variable("x"), Plus(Variable("y"), Constant(5))))
      }
      test("assignation-with-val") {
        mathAssert{
          val v = Variable("x")
          v := Variable("y") + Constant(5)
        }(Assign(Variable("x"), Plus(Variable("y"), Constant(5))))
      }
    }
    test("sequencing") {
      mathAssert{
        Variable("x") := Constant(0)
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
          if Variable("x") === Constant(-1) then
            Constant(0)
          else
            Constant(1)
        }(
          If(
            Eq(Variable("x"), Constant(-1)),
            Constant(0),
            Constant(1)
          )
        )
      }
      test("while") {
        mathAssert{
          while Variable("x") === Constant(0) do
            Variable("x") := Variable("x")  - Constant(3)
          Variable("x")
        }(
          Sequence(
            While(
              Eq(Variable("x"), Constant(0)),
              Assign(Variable("x"), Minus(Variable("x"), Constant(3)))
            ),
            Variable("x")
          )
        )
      }
    }
  }

}