import utest.*

import exproc.*

sealed trait MathExpr
case class Variable(name: String) extends MathExpr
case class Constant(i: Int) extends MathExpr
case class Assign(to: Variable, v: MathExpr) extends MathExpr
case class Plus(lhs: MathExpr, rhs: MathExpr) extends MathExpr
case class Minus(lhs: MathExpr, rhs: MathExpr) extends MathExpr
case class Sequence(first: MathExpr, second: MathExpr) extends MathExpr
case class If(cond: MathExpr, thenn: MathExpr, elze: MathExpr) extends MathExpr
case class While(cond: MathExpr, body: MathExpr) extends MathExpr

extension (m: MathExpr) {
  def +(n: MathExpr) = Plus(m, n)
  def -(n: MathExpr) = Minus(m, n)
}

extension (v: Variable) {
  def :=(m: MathExpr) = Assign(v, m)
}

object MathProcessor extends Processor[MathExpr] {
  given Conversion[MathExpr, Boolean] = ???

  override def sequence(ls: Seq[MathExpr]) = ls.reduceRight(Sequence(_, _))
  override def ifThenElse(cond: MathExpr, thenn: MathExpr, elze: MathExpr) = If(cond, thenn, elze)
  override def whileLoop(cond: MathExpr, body: MathExpr) = While(cond, body)
}

inline def math(inline expr: Any): MathExpr = process(MathProcessor)(expr)

inline def mathAssert(inline expr: Any)(res: MathExpr): Unit = assert( math(expr) == res )

object MathProcessorTests extends TestSuite {
  import MathProcessor.given
  val tests = Tests {
    test("DSL") {
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
          if Variable("x") := Constant(-1) then
            Constant(0)
          else
            Constant(1)
        }(
          If(
            Assign(Variable("x"), Constant(-1)),
            Constant(0),
            Constant(1)
          )
        )
      }
    }
  }

}