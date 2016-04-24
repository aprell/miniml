miniml /mɪnəməl/
================

A minimal applicative programming language

Usage
-----

Compile with `ocamlbuild -use-menhir miniml.[byte|native]`.

miniml can generate Lua code:

```
./miniml.byte -c fact.miniml
print((function (fact_) fact = fact_ return (fact)(10) end)(function (x)
return ((x < 1) and 1 or (x * (fact)((x - 1)))) end))

./miniml.byte -c fact.miniml | lua
3628800
```

Oh dear.

TODO
----

- Study [TAPL](https://www.cis.upenn.edu/~bcpierce/tapl)
- Add types, type checking, and type inference

References
----------

- [CS 3110 - Data Structures and Functional Programming](http://www.cs.cornell.edu/courses/cs3110/2015fa)
- [Compilers as lazy denotational interpreters](http://researchblogs.cs.bham.ac.uk/thelablunch/2016/01/compilers-as-lazy-denotational-interpreters)
- [The Programming Languages Zoo](https://github.com/andrejbauer/plzoo)
- [Compiling Mini-ML to JavaScript](http://www.lexicallyscoped.com/2015/06/28/miniml-compiler-to-js.html)
