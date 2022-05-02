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

def hasFlag(using Quotes)(s: quotes.reflect.Term | quotes.reflect.Statement, flag: quotes.reflect.Flags): Boolean = 
  s.symbol.flags.is(flag)


def selectUniqueType(using Quotes)(instance: quotes.reflect.Term, base: quotes.reflect.Symbol, name: String): quotes.reflect.TypeRepr = 
    val s = base.declaredType(name)
    assert(s.length == 1)
    instance.select(s.head).tpe