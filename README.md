# Exproc (**Ex**pression **proc**essor) library

Tired of having to write a full-blown compiler for your [XML-based plane control language](https://snowflake0s.github.io/funkyguide/) ?
Tired of writing monadic-style computations no-one understands ?
```scala
val willInline = maybeName
    .flatMap(name => {
        env(name).map(fun => {
            (fun, size(fun.body))
        })
    })
    .filter(_._2 <= funLimit) 
    .map(_._1)
    .filter(fun => sameLen(fun.args, args))
```

Look no further, Exproc will ease your life.
```scala
val willInline = maybeFlow {
    val name = ! maybeName
    val fun = ! env(name)

    if (size(fun.body) <= funLimit && sameLen(fun.args, args)) then
        Some(fun)
    else
        None
}
```

# Examples
## Computation expressions
Inspired from F#'s computation expressions, some possible monadic-flows are available in the [tests](src/test/scala/computations/).

## AST builders
Two examples are available, both based upon tiny languages used in videogames:
- [less-funky-trees](https://github.com/Ef55/less-funky-trees) A compiler for [simple planes](https://www.simpleplanes.com/);
- [Human Resource Scala Machine](https://github.com/Ef55/hrm-scala-compiler) A compiler for [Human Resource Machine](https://tomorrowcorporation.com/humanresourcemachine).