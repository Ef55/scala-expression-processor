package exproc.utils

import scala.quoted.*

def SubstituteRef(using Quotes)(ref: quotes.reflect.Symbol, replacement: quotes.reflect.Term): quotes.reflect.TreeMap =
  new quotes.reflect.TreeMap {
    import quotes.reflect.*

    override def transformTerm(t: Term)(owner: Symbol): Term = t match 
      case id@Ident(_) if id.symbol == ref => replacement.changeOwner(owner)
      case _ => super.transformTerm(t)(owner)
  }

def chain(using Quotes)(tms: quotes.reflect.TreeMap*)(term: quotes.reflect.Term, owner: quotes.reflect.Symbol): quotes.reflect.Term =
  tms.foldLeft(term)( (t, tm) => tm.transformTerm(t)(owner) )