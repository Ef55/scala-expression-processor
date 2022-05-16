package asts

import exproc.*
import exproc.{Identifier => Id}
import utest.*
import testing.*
import scala.compiletime.summonInline

object AST {
  sealed trait MathExpr[+T]
  class Variable[+T] extends MathExpr[T]
  case class Constant(i: Int) extends MathExpr[Int]
  case class Initialize[T](to: Variable[T], v: MathExpr[T]) extends MathExpr[Unit]
  case class Assign[T](to: Variable[T], v: MathExpr[T]) extends MathExpr[Unit]
  case class Plus(lhs: MathExpr[Int], rhs: MathExpr[Int]) extends MathExpr[Int]
  case class Minus(lhs: MathExpr[Int], rhs: MathExpr[Int]) extends MathExpr[Int]
  case class Sequence[+T](first: MathExpr[?], second: MathExpr[T]) extends MathExpr[T]
  case class IfT[+T](cond: MathExpr[Boolean], thenn: MathExpr[T], elze: MathExpr[T]) extends MathExpr[T]
  case class WhileT(cond: MathExpr[Boolean], body: MathExpr[Any]) extends MathExpr[Unit]
  case class Eq[+T](lhs: MathExpr[T], rhs: MathExpr[T]) extends MathExpr[Boolean]
}

object math 
extends AstBuilder[AST.MathExpr] 
with ControlFlow[AST.MathExpr] 
with IrrelevantImplicitElse[AST.MathExpr] {
  import AST.*
  transparent inline given AstBuilder[AST.MathExpr] = this

  /* DSL */

  extension (m: MathExpr[Int]) {
    def +(n: MathExpr[Int]) = Plus(m, n)
    def -(n: MathExpr[Int]) = Minus(m, n)
  }

  extension [T](m: MathExpr[T]) {
    def ===(n: MathExpr[T]) = Eq(m, n)
  }

  given boolToInt: Conversion[Boolean, MathExpr[Int]] with
    def apply(b: Boolean) = if b then Constant(1) else Constant(0)

  given autoConst: Conversion[Int, MathExpr[Int]] with 
    def apply(t: Int) = constant(t)

  override type Variable[T] = AST.Variable[T]

  override inline def freshVariable[T]: Variable[T] = new Variable[T]()
  override inline def initialize[T](inline va: Variable[T], inline init: MathExpr[T]) = Initialize(va, init)
  override inline def assign[T](inline va: Variable[T], inline init: MathExpr[T]) = Assign(va, init)

  override inline def constant[T](inline t: T) = 
    val conv = summonInline[=:=[T, Int]]
    val conv2 = conv.liftCo[MathExpr].flip
    conv2(Constant(conv(t)): MathExpr[Int])
  
  override inline def combine[T, S](inline first: MathExpr[T], inline second: MathExpr[S]) = 
    Sequence(first, second)

  override inline def ifThenElse[T](inline cond: MathExpr[Boolean], inline thenn: MathExpr[T], inline elze: MathExpr[T]) = IfT(cond, thenn, elze)
  override inline def whileLoop[T](inline cond: MathExpr[Boolean], inline body: MathExpr[T]) = WhileT(cond, body)
}


object MathAst extends TestSuite {
  import AST.*
  import math.{*,given}
  import scala.language.implicitConversions

  import BuilderAssertions.{
    buildMatchAssert => mathAssert,
    buildOutAssert => mathOut, 
    buildCompileError => mathError
  }

  inline def mathAutoOut[T](inline expr: (Any => Unit) => MathExpr[T])(count: Int): Unit =
    mathOut(expr)((0 until count).mkString("\n"))

  val tests = Tests {
    test("DSL") {
      test("constant") {
        mathAssert{
          0
        }{case 
          Constant(0)
        =>}
      }
      test("equality") {
        mathAssert{
          val x: Variable[Int] = ! 0
          x === 0
        }{ case 
          Sequence(
            Initialize(x1, Constant(0)),
            Eq(x2, Constant(0))
          )
          if x1 == x2
        =>}
      }
      test("assignation-with-val") {
        mathAssert{
          var x: Variable[Int] = ! (-1)
          val y: Variable[Int] = ! 0
          x =! y + Constant(5)
        }{ case 
          Sequence(
            Initialize(x1, Constant(-1)), 
            Sequence(
              Initialize(y1, Constant(0)),
              Assign(x2, Plus(y2, Constant(5)))
            )
          )
        if x1 == x2 && y1 == y2 && x1 != y1 =>}
      }
      test("bool-to-constant") {
        mathAssert{
          var x: Variable[Int] = ! 0
          x =! true
        }{ case
          Sequence(
            Initialize(x1, Constant(0)),
            Assign(x2, Constant(1))
          )
          if x1 == x2
        =>}
      }
      test("re-assignation") {
        mathAssert{
          var x: Variable[Int] = ! 0
          x =! true
          x =! 2
        }{ case
          Sequence(
            Initialize(x1, Constant(0)),
            Sequence(
              Assign(x2, Constant(1)),
              Assign(x3, Constant(2))
            )
          )
          if x1 == x2 && x2 == x3
        =>}
      }
    }
    test("variables") {
      test("constant-init") {
        mathAssert{
          val x: Variable[Int] = ! 0
          x
        }{case 
          Sequence(
            Initialize(x1, Constant(0)),
            x2
          )
        if x1 == x2 =>}
      }
      test("expr-init") {
        mathAssert{
          val x: Variable[Int] = ! 0
          val y: Variable[Int] = !( x + Constant(1) )
          val z: Variable[Int] = ! y
          z
        }{case 
          Sequence(
            Initialize(x1, Constant(0)),
            Sequence(
              Initialize(y1, Plus(x2, Constant(1))),
              Sequence(
                Initialize(z1, y2),
                z2
              )
            )
          )
        => assert(x1 == x2 && y1 == y2 && z1 == z2 
            && x1 != y1 && y1 != z1 && z1 != x1
        )}
      }
    }
    test("sequencing") {
      mathAssert{
        var x: Variable[Int] = ! 0
        x =! Constant(1)
      }{ case
        Sequence(
          Initialize(x1, Constant(0)),
          Assign(x2, Constant(1))
        )
        if x1 == x2
      =>}
    }
    test("proto-control-flow") {
      test("if-then-else") {
        mathAssert{
          val x: Variable[Int] = ! 0
          If(x === Constant(-1)){
            Constant(0)
          }{
            Constant(1)
          }
        }{ case 
          Sequence(
            Initialize(x1, Constant(0)),
            IfT(
              Eq(x2, Constant(-1)),
              Constant(0),
              Constant(1)
            )
          )
          if x1 == x2
        =>}
      }
      test("statement-if-then-else") {
        mathAssert{
          If(Constant(0) === Constant(1)){
            If(Constant(0) === Constant(1)){
              1
            }{
              0
            }
          }{
            0
          }
          1
        }{case
          Sequence(
            IfT(
              Eq(Constant(0), Constant(1)),
              IfT(
                Eq(Constant(0), Constant(1)),
                Constant(1),
                Constant(0)
              ),
              Constant(0)
            ),
            Constant(1)
          )
        =>}
      }
      test("while") {
        mathAssert{
          var x: Variable[Int] = ! 10
          While(x === Constant(0)){
            x =! x  - Constant(3)
          }
          x
        }{ case
          Sequence(
            Initialize(x1, Constant(10)), Sequence(
              WhileT(
                Eq(x2, Constant(0)),
                Assign(x3, Minus(x4, Constant(3)))
              ),
              x5
          ))
          if x1 == x2 && x2 == x3 && x3 == x4 && x4 == x5
        =>}
      }
    }
    test("meta-control-flow") {
      test("if-then-else") {
        mathAssert{
          val b = false
          if (if b then false else true) then 
            Constant(1) 
          else 
            Constant(0)
        }{case 
          Constant(1)
        =>}
      }
      test("while") {
        mathAssert{
          var x = 0
          while x < 10 do
            x += 1
          Constant(x)
        }{case
          Constant(10)
        =>}
      }
    }
    test("extra-features") {
      test("variable-shadowing") {
        mathAssert{
          val x: Variable[Int] = ! 0
          x === Constant(0)
          {
            val x: Variable[Int] = ! 1
            x === Constant(1)
          }
        }{
          case Sequence(
            Initialize(x1, Constant(0)), 
            Sequence(
              Eq(x2, Constant(0)),
              Sequence(
                Initialize(y1, Constant(1)),
                Eq(y2, Constant(1))
              )
            )
          ) => 
            assert(x1 == x2, y1 == y2)
            assert(x1 != y1, x1 != y2)
            assert(x2 != y1, x2 != y2)
        }
      }
      test("for-generator") {
        mathAssert{
          var x: Variable[Int] = ! 0
          for i <- 1 until 3 yield
            x =! Constant(i)
          x =! Constant(3)
        }{case 
          Sequence(
            Initialize(x1, Constant(0)),
            Sequence(
              Sequence(
                Assign(x2, Constant(1)),
                Assign(x3, Constant(2))
              ),
              Assign(x4, Constant(3))
            )
          )
          if x1 == x2 && x2 == x3 && x3 == x4
        =>}
      }
      test("block-init") {
        mathAssert{
          val x: Variable[Int] = !{
            val y: Variable[Int] = ! 0
            y
          }
          x
        }{case 
          Sequence(
            Initialize(x1, 
              Sequence(
                Initialize(y1, Constant(0)),
                y2
              )
            ),
            x2
          )
        if x1 == x2 && y1 == y2 && x1 != y1
        =>}
      }
    }
    test("side-effects") {
      test("vals") {
        mathAutoOut(log => {
          val x: Variable[Int] = ! Constant{ log(0); 0}
          var y: Variable[Int] = ! Constant{ log(1); 1}
          val z: Variable[Int] = ! Constant{ log(2); 1}
          x
        })(3)
      }
      test("block") {
        mathAutoOut(log => {
          val x: Variable[Int] = ! Constant{ log(0); 0}
          log(1)
          x
        })(2)
      }
    }
  }
}