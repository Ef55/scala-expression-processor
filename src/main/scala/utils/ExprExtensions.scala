package exproc.utils

import scala.quoted.*

extension (using Quotes)(t: quotes.reflect.Term) {
  def isExprOf(tpe: quotes.reflect.TypeRepr): Boolean = t.tpe <:< tpe

}