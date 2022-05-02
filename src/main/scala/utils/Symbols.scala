package exproc.utils

import scala.quoted.*

@scala.annotation.tailrec
def hasOwner(using Quotes)(owner: quotes.reflect.Symbol)(symbol: quotes.reflect.Symbol): Boolean = {
  import quotes.reflect.*
  require(owner != Symbol.noSymbol)

  if(owner == symbol) then true 
  else if (symbol == Symbol.noSymbol) then false
  else hasOwner(owner)(symbol.owner)
}