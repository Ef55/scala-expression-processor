import scala.quoted.*

inline def process(inline expr: Any): Any =
  ${processImpl('expr)}

def processImpl(expr: Expr[Any])(using Quotes): Expr[Any] = 
  import quotes.reflect.*

  object Transform extends TreeMap {}

  Transform.transformTerm(expr.asTerm)(Symbol.spliceOwner).asExpr