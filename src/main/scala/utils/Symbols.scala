package exproc.utils

import scala.quoted.*
import scala.annotation.targetName

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

def overrides(using Quotes)(s: quotes.reflect.Symbol, base: quotes.reflect.Symbol): Boolean = 
  s == base || s.allOverriddenSymbols.exists(_ == base)

def unique[T](seq: Seq[T]): T = 
  assert(seq.length == 1, "Sequence was expected to contain one single element.")
  seq.head

def selectUniqueMethod(using Quotes)(instance: quotes.reflect.Term, name: String): quotes.reflect.Term = 
  val s = unique(instance.tpe.typeSymbol.declaredMethod(name))
  instance.select(s)

def selectUniqueType(using Quotes)(instance: quotes.reflect.Term, base: quotes.reflect.Symbol, name: String): quotes.reflect.TypeRepr = 
  val s = unique(base.declaredType(name))
  instance.select(s).tpe