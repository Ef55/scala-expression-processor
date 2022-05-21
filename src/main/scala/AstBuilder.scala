package exproc

import scala.quoted.*
import exproc.utils.*

trait AstBuilder[Tree[_]] extends ComputationBuilder[Tree] {

  // API

  type Variable[T] <: Tree[T]

  inline def freshVariable[T]: Variable[T]
  inline def initialize[T](inline va: Variable[T], inline init: Tree[T]): Tree[Unit]
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

trait ControlFlow[Tree[_]] { self: AstBuilder[Tree] =>
  inline def ifThenImplicitElse[T](inline cond: Tree[Boolean], inline thenn: Tree[T], inline elze: Tree[T]): Tree[T]
  inline def ifThenElse[T](inline cond: Tree[Boolean], inline thenn: Tree[T], inline elze: Tree[T]): Tree[T]
  inline def whileLoop[T](inline cond: Tree[Boolean], inline body: Tree[T]): Tree[Unit]

  case class ImplicitElse[T](tree: Tree[T])

  inline def If[T](inline cond: Tree[Boolean])(inline thenn: Tree[T])(inline elze: Tree[T]): Tree[T] =
    ifThenElse(cond, thenn, elze)

  inline def If[T](inline cond: Tree[Boolean])(inline thenn: Tree[T])(using elze: ImplicitElse[T]): Tree[T] =
    ifThenImplicitElse(cond, thenn, elze.tree)

  inline def While[T](inline cond: Tree[Boolean])(inline body: Tree[T]): Tree[Unit] =
    whileLoop(cond, body)
}

trait EmptyWhile[Tree[_]] { self: AstBuilder[Tree] =>
  inline def emptyWhileLoop[T](inline cond: Tree[Boolean]): Tree[Unit]

  inline def While[T](inline cond: Tree[Boolean])(): Tree[Unit] =
    emptyWhileLoop(cond)
}

trait NoImplicitElse[Tree[_]] { self: ControlFlow[Tree] with AstBuilder[Tree] =>
  inline def ifThenImplicitElse[T](inline cond: Tree[Boolean], inline thenn: Tree[T], inline elze: Tree[T]): Tree[T] =
    scala.compiletime.error("Implicit elses are disabled.")
}

trait IrrelevantImplicitElse[Tree[_]] { self: ControlFlow[Tree] with AstBuilder[Tree] =>
  inline def ifThenImplicitElse[T](inline cond: Tree[Boolean], inline thenn: Tree[T], inline elze: Tree[T]): Tree[T] =
    self.ifThenElse(cond, thenn, elze)
}

trait DefaultUnitElse[Tree[_]] { self: ControlFlow[Tree] with AstBuilder[Tree] =>
  inline given ImplicitElse[Unit] = ImplicitElse(self.constant(()))
}