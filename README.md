miniml /mɪnəməl/
================

A minimal applicative programming language

Usage
-----

Build miniml by typing `make`, or simply run `./miniml`.

miniml can generate Lua code:

```
./miniml -c examples/fact.miniml
print((function (fact_) fact = fact_ return (fact)(10) end)(function (x)
return ((x < 1) and 1 or (x * (fact)((x - 1)))) end))

./miniml -c examples/fact.miniml | lua
3628800
```

Oh dear.

References
----------

- [CS 3110 - Data Structures and Functional Programming](http://www.cs.cornell.edu/courses/cs3110/2015fa)
- [Compilers as lazy denotational interpreters](http://researchblogs.cs.bham.ac.uk/thelablunch/2016/01/compilers-as-lazy-denotational-interpreters)
- [The Programming Languages Zoo](https://github.com/andrejbauer/plzoo)
- [Compiling Mini-ML to JavaScript](http://www.lexicallyscoped.com/2015/06/28/miniml-compiler-to-js.html)
- [An Intro to Type Checking](https://mukulrathi.netlify.app/create-your-own-programming-language/intro-to-type-checking)
- [Parser generators and function application](https://ptival.github.io/2017/05/16/parser-generators-and-function-application)
- [Automatic Compiler Pass Fusion](https://keleshev.com/automatic-compiler-pass-fusion)
