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

def unique[T](seq: Seq[T]): T = 
  assert(seq.length == 1, "Sequence was expected to contain one single element.")
  seq.head

def selectUniqueType(using Quotes)(instance: quotes.reflect.Term, base: quotes.reflect.Symbol, name: String): quotes.reflect.TypeRepr = 
    val s = unique(base.declaredType(name))
    instance.select(s).tpe