package exproc

import scala.quoted.*
import exproc.utils.*

/** Leverage metaprogramming to provide nice to use AST builders.
  *
  * The DSL implementer has to implement the abstract methods
  * to define the behavior of the different construct.
  * Mixins are also provided which can be used to quickly make the DSL richer. 
  */ 
trait AstBuilder[Tree[_]] extends ComputationBuilder[Tree] {

  // API

  /** Type of variables in the produces AST. */
  type Variable[T] <: Tree[T]

  /** Generate a fresh variable. */
  inline def freshVariable[T]: Variable[T]

  /** Initialize a variable with its initial value. */
  inline def initialize[T](inline va: Variable[T], inline init: Tree[T]): Tree[Unit]

  /** Define a constant AST from a Scala value. */
  inline def constant[T](inline t: T): Tree[T]

  inline def combine[T, S](inline l: Tree[T], inline r: Tree[S]): Tree[S]

  // Computation expression implementation

  override type Bound[T] = Variable[T]

  override inline def bind[T, S](inline m: Tree[T], inline f: Variable[T] => Tree[S]): Tree[S] =
    val v = freshVariable[T]
    combine(initialize(v, m), f(v))

  override inline def unit[T](inline t: => T): Tree[T] =
    constant[T](t)

  override inline def init[T](inline c: () => Tree[T]): Tree[T] =
    c()
}

/** Adds common control-flow (if-then-else, while) elements to an AST builder. */
trait ControlFlow[Tree[_]] { self: AstBuilder[Tree] =>
  inline def ifThenImplicitElse[T](inline cond: Tree[Boolean], inline thenn: Tree[T], inline elze: Tree[T]): Tree[T]
  inline def ifThenElse[T](inline cond: Tree[Boolean], inline thenn: Tree[T], inline elze: Tree[T]): Tree[T]
  inline def whileLoop[T](inline cond: Tree[Boolean], inline body: Tree[T]): Tree[Unit]

  case class ImplicitElse[T](tree: Tree[T])

  /** If-then-else block. */ 
  inline def If[T](inline cond: Tree[Boolean])(inline thenn: Tree[T])(inline elze: Tree[T]): Tree[T] =
    ifThenElse(cond, thenn, elze)

  /** If-then block.
   * 
   * A given `ImplicitElse` must be available for this block to be usable.
   */
  inline def If[T](inline cond: Tree[Boolean])(inline thenn: Tree[T])(using elze: ImplicitElse[T]): Tree[T] =
    ifThenImplicitElse(cond, thenn, elze.tree)

  /** While block. */
  inline def While[T](inline cond: Tree[Boolean])(inline body: Tree[T]): Tree[Unit] =
    whileLoop(cond, body)
}

/** Adds empty (i.e. without body) while to an AST builder. */
trait EmptyWhile[Tree[_]] { self: AstBuilder[Tree] =>
  inline def emptyWhileLoop[T](inline cond: Tree[Boolean]): Tree[Unit]

  /** While block with empty body. */
  inline def While[T](inline cond: Tree[Boolean])(): Tree[Unit] =
    emptyWhileLoop(cond)
}

/** Mixin disabling if-then blocks.  */
trait NoImplicitElse[Tree[_]] { self: ControlFlow[Tree] with AstBuilder[Tree] =>
  inline def ifThenImplicitElse[T](inline cond: Tree[Boolean], inline thenn: Tree[T], inline elze: Tree[T]): Tree[T] =
    scala.compiletime.error("Implicit elses are disabled.")
}

/** Mixin reducing if-then blocks into if-then-else blocks. */
trait IrrelevantImplicitElse[Tree[_]] { self: ControlFlow[Tree] with AstBuilder[Tree] =>
  inline def ifThenImplicitElse[T](inline cond: Tree[Boolean], inline thenn: Tree[T], inline elze: Tree[T]): Tree[T] =
    self.ifThenElse(cond, thenn, elze)
}

/** Mixin providing if-then when the expression returns unit. */
trait DefaultUnitElse[Tree[_]] { self: ControlFlow[Tree] with AstBuilder[Tree] =>
  inline given ImplicitElse[Unit] = ImplicitElse(self.constant(()))
}