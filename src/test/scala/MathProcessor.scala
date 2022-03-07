import utest.*
import exproc.*
import exproc.{Identifier => Id}

object MathAST {
  sealed trait MathExpr[+T]
  case class Variable[+T](id: Identifier) extends MathExpr[T]
  case class Constant(i: Int) extends MathExpr[Int]
  case class Assign[T](to: Variable[T], v: MathExpr[T]) extends MathExpr[Unit]
  case class Plus(lhs: MathExpr[Int], rhs: MathExpr[Int]) extends MathExpr[Int]
  case class Minus(lhs: MathExpr[Int], rhs: MathExpr[Int]) extends MathExpr[Int]
  case class Sequence[+T](first: MathExpr[?], second: MathExpr[T]) extends MathExpr[T]
  case class If[+T](cond: MathExpr[Boolean], thenn: MathExpr[T], elze: MathExpr[T]) extends MathExpr[T]
  case class While(cond: MathExpr[Boolean], body: MathExpr[Any]) extends MathExpr[Unit]
  case class Eq[+T](lhs: MathExpr[T], rhs: MathExpr[T]) extends MathExpr[Boolean]

}

object math extends Processor[MathAST.MathExpr, MathAST.Variable] {
  import MathAST.*

  /*
   * DSL
   */
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

  given Conversion[Boolean, MathExpr[Int]] with
    def apply(b: Boolean) = if b then Constant(1) else Constant(0)

  /*
   * Processor
   */

  override def variable[T](id: Identifier): Variable[T] = Variable[T](id)
  override def initializer[T](va: Variable[T], init: MathExpr[T]) = va := init
  override def constant[T](t: T) = t match {
    case i: Int => Constant(i).asInstanceOf[MathExpr[T]]
  }
  override def sequence[T](ls: Seq[MathExpr[Any]], last: MathExpr[T]) = ls.foldRight(last)(Sequence(_, _))
  override def ifThenElse[T](cond: MathExpr[Boolean], thenn: MathExpr[T], elze: MathExpr[T]) = If(cond, thenn, elze)
  override def whileLoop(cond: MathExpr[Boolean], body: MathExpr[Any]) = While(cond, body)
}

object VariableName {
  def unapply[T](v: MathAST.Variable[T]): Option[String] = Identifier.unapply(v.id)
}

inline def mathAssert[T](inline expr: MathAST.MathExpr[T])(inline expected: PartialFunction[Any, Unit]): Unit = 
  val result = math(expr)
  assertMatch( result )( expected )

object MathProcessorTests extends TestSuite {
  import MathAST.*
  import math.{*,given}


  val tests = Tests {
    test("DSL") {
      test("equality") {
        mathAssert{
          val x: Variable[Int] = 0
          x === Constant(0)
        }{ case 
          Sequence(
            Assign(VariableName("x"), Constant(0)),
            Eq(VariableName("x"), Constant(0))
          )
        =>}
      }
      test("assignation-with-val") {
        mathAssert{
          val x: Variable[Int] = -1
          val y: Variable[Int] = 0
          x := y + Constant(5)
        }{ case 
          Sequence(
            Assign(VariableName("x"), Constant(-1)), Sequence(
            Assign(VariableName("y"), Constant(0)),
            Assign(VariableName("x"), Plus(VariableName("y"), Constant(5)))
          ))
        =>}
      }
      test("bool-to-constant") {
        mathAssert{
          val x: Variable[Int] = 0
          x := true
        }{ case
          Sequence(
            Assign(VariableName("x"), Constant(0)),
            Assign(VariableName("x"), Constant(1))
          )
        =>}
      }
    }
    test("variables") {
      test("constant-init") {
        mathAssert{
          val x: Variable[Int] = 0
          x
        }{case 
          Sequence(
            Assign(VariableName("x"), Constant(0)),
            VariableName("x")
          )
        =>}
      }
      test("expr-init") {
        mathAssert{
          val x: Variable[Int] = 0
          val y: Variable[Int] = x + Constant(1)
          val z: Variable[Int] = y
          z
        }{case 
          Sequence(
            Assign(VariableName("x"), Constant(0)),
            Sequence(
              Assign(VariableName("y"), Plus(VariableName("x"), Constant(1))),
              Sequence(
                Assign(VariableName("z"), VariableName("y")),
                VariableName("z")
              )
            )
          )
        =>}
      }
    }
    test("sequencing") {
      mathAssert{
        val x: Variable[Int] = 0
        x := Constant(1)
      }{ case
        Sequence(
          Assign(VariableName("x"), Constant(0)),
          Assign(VariableName("x"), Constant(1))
        )
      =>}
    }
    test("optional-features") {
      test("empty") {
        val err = intercept[UnsupportedFeature](math{
          () // MathProcessor doesn't define empty
        })
        assert(err.toString.contains("Missing feature"))
        assert(err.toString.contains("empty"))
      }
    }
    test("proto-control-flow") {
      test("if-then-else") {
        mathAssert{
          val x: Variable[Int] = 0
          if x === Constant(-1) then
            Constant(0)
          else
            Constant(1)
        }{ case 
          Sequence(
            Assign(VariableName("x"), Constant(0)),
            If(
              Eq(VariableName("x"), Constant(-1)),
              Constant(0),
              Constant(1)
            )
          )
        =>}
      }
      test("while") {
        mathAssert{
          val x: Variable[Int] = 10
          while x === Constant(0) do
            x := x  - Constant(3)
          x
        }{ case
          Sequence(
            Assign(VariableName("x"), Constant(10)), Sequence(
              While(
                Eq(VariableName("x"), Constant(0)),
                Assign(VariableName("x"), Minus(VariableName("x"), Constant(3)))
              ),
              VariableName("x")
          ))
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
    }
    test("errors") {
      test("features-abuse") {
        test("bool-unwrap") {
          val err = compileError("""math{
            val x: Variable[Int] = 0
            if x === Constant(0) then Constant(0) else Constant(1)
            val b: Boolean = x === Constant(0)
          }""")
          assert(err.msg.contains("being converted"))
          assert(err.msg.contains("if/while"))
        }
        test("initializer-as-conversion") {
          val err = compileError("""math{
            val x: Variable[Int] = 0
            x := true   // Ok: an additional conversion was defined as part of the DSL
            x := 1      // Error: default conversion used, which is reserved for variables initialization
          }""")
          assert(err.msg.contains("1"))
          assert(err.msg.contains("not of type MathExpr"))
        }
        test("top-level-initializer") {
          val err = compileError("""math{
            1
          }""")
          assert(err.msg.contains("1"))
          assert(err.msg.contains("not of type MathExpr"))
        }
      }
      test("invalid-proto-flow") {
        // Constructs of the form `if <proto/> then <meta/> else <meta/>`
        test("if-then-else") {
          val err = compileError("""math{
            if Constant(0) === Constant(0) then
              1
            else
              0
            Constant(0)
          }""")
          assert(err.msg.contains("proto-if-then-else"))
          assert(err.msg.contains("meta-value"))
        }
      }
    }
    test("variable-shadowing") {
      val r = math {
        val x: Variable[Int] = 0
        x === Constant(0)
        {
          val x: Variable[Int] = 1
          x === Constant(1)
        }
      }
      assertMatch(r){
        case Sequence(
          Assign(Variable(x1: Identifier), Constant(0)), 
          Sequence(
            Eq(Variable(x2: Identifier), Constant(0)),
            Sequence(
              Assign(Variable(y1: Identifier), Constant(1)),
              Eq(Variable(y2: Identifier), Constant(1))
            )
          )
        ) => 
          assert(x1.name == y1.name)
          assert(x1 == x2, y1 == y2)
          assert(x1 != y1, x1 != y2)
          assert(x2 != y1, x2 != y2)
      }
    }

    test("WIP") {
      // test("for") {
      //   mathAssert{
      //     for i <- 0 until 3 do
      //       Constant(i)
      //   }{
      //     PartialFunction.empty
      //   }
      // }
      // test("variable-assignation") {
      //   math{
      //     var x = Constant(0)
      //     x = Constant(1)
      //     x
      //   }
      // }
    }
  }

}