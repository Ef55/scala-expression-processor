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
  inline def ifThenElse[T](inline cond: Tree[Boolean], inline thenn: Tree[T], inline elze: Tree[T]): Tree[T]
  inline def whileLoop[T](inline cond: Tree[Boolean], inline body: Tree[T]): Tree[Unit]

  inline def If[T](inline cond: Tree[Boolean])(inline thenn: Tree[T])(inline elze: Tree[T]): Tree[T] =
    ifThenElse(cond, thenn, elze)

  inline def If[T](inline cond: Tree[Boolean])(inline thenn: Tree[T])(using c: Tree[T] =:= Tree[Unit]): Tree[Unit] =
    ifThenElse(cond, c(thenn), self.constant(()))

  inline def While[T](inline cond: Tree[Boolean])(inline body: Tree[T]): Tree[Unit] =
    whileLoop(cond, body)
}