
LR(1)-parser generator (with PGM-compression)
==============
Generate yourself an LR1-parser.

Installation
------------
```
$ cabal v2-install exe:gen-lr1-parser
```

Usage
-----
```sh
$ gen-lr1-parser --help
```
```
LR parser generator

Usage: gen-lr1-parser [--src PATH-TO-SRC] --module MODULE-NAME
                      --grammar PATH-TO-GRAMMAR

  Generate LR parser

Available options:
  --src PATH-TO-SRC        path to root of haskell sources
  --module MODULE-NAME     name of the module to generate
  --grammar PATH-TO-GRAMMAR
                           path to grammar
  -h,--help                Show this help text
```

Example grammar
---------------
```
imports
  "import Expr.AST"

start
  Expr

Expr
  = a:Add "=" b:Add => "ExprBinary pos a Equals b"
  | a:Add           => "a"

Add: "Expr"
  = a:Add  "+" b:Mult => "ExprBinary pos a Add b"
  | a:Mult            => "a"

Mult : "Expr"
  = a:Mult "*" b:Term => "ExprBinary pos a Mult b"
  | a:Term            => "a"

Term : "Expr"
  = "(" e:Expr ")" => "e"
  | n:"<Name>"     => "ExprVar   pos n"
  | n:Const        => "ExprConst pos n"
```

Larger example
--------------
[grammar](lr1-parser-example/src/language.grammar)
[generated parser](lr1-parser-example/src/Parser.hs)
[usage](lr1-parser-example/src/Main.hs)
[example1](lr1-parser-example/src/test.dlog)
[example2](lr1-parser-example/src/test.test)

To install the example
```sh
$ cabal v2-install exe:lr1-parser-example
```

To run the example
```sh
$ lr1-parser-example "lr1-parser-example/src/test.dlog" lr1-parser-example/src/test.test
```
```
edge(1, 2).
edge(2, 3).
edge(3, 4).
edge(4, 5).
has-link(A, B, A, B) <- edge(A, B).
has-link(A, B, C, D) <- has-link(A, C), edge(C, D), has-link(D, B).
unlink(A, B) -> has-link(A, B, C, D) => -edge(C, D).

expect edge(2, 3)
notify unlink(1, 4)
guard  (1 = (2 + 1))
```

License
-------
BSD 3-Clause for all the components
