package exproc

import scala.quoted.*
import exproc.utils.*

trait AstBuilder[Tree[_]] extends ComputationBuilder[Tree] {

  // API

  type Variable[T] <: Tree[T]

  inline def freshVariable[T]: Variable[T]
  inline def initialize[T](inline va: Variable[T], inline init: Tree[T]): Tree[Unit]
  inline def constant[T](inline t: T): Tree[T]
  inline def sequence[T, S](inline l: Tree[T], inline r: Tree[S]): Tree[S]

  // Computation expression implementation

  override type Bound[T] = Variable[T]

  override inline def bind[T, S](inline m: Tree[T], inline f: Variable[T] => Tree[S]): Tree[S] =
    val v = freshVariable[T]
    sequence(initialize(v, m), f(v))

  override inline def unit[T](inline t: => T): Tree[T] =
    constant[T](t)

  override inline def init[T](inline c: () => Tree[T]): Tree[T] =
    c()
}