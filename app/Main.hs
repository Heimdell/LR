
import Prelude hiding (lex)

import S
import LR
import Lex
import Exts

test :: ETable S
test = ETable
  [ ERule  Start      ["Struct"]                                     "Start"
  , ERule "Struct"    ["\\\\", "Name", "->", "Struct"]               "Lam"              -- \s -> s s
  , ERule "Struct"    ["Add", "<\\|", "Struct"]                      "Pipe"              -- \s -> s s
  , ERule "Struct"    ["let", "Name", "=", "Struct", ";", "Struct"]  "Let"              -- let id = \x -> x; id 42
  , ERule "Struct"    ["Add"]                                       "-LetExpr"
  , ERule "Add"       ["Add", "\\-", "Unary"]                        "Minus"            -- foo x - b
  , ERule "Add"       ["Add", "\\+", "Unary"]                        "Plus"             -- x + sum ys
  , ERule "Add"       ["Unary"]                                     "-Add"
  , ERule "Unary"     ["\\-", "Unary"]                               "Negate"           -- -x
  , ERule "Unary"     ["\\+", "Unary"]                               "Noop"             -- +y
  , ERule "Unary"     ["Expr"]                                      "-U"
  , ERule "Expr"      ["Expr", "Term"]                               "Call"             -- sum xs
  , ERule "Expr"      ["Term"]                                      "-ExprTerm"
  , ERule "Term"      ["Name"]                                       "Var"
  , ERule "Term"      ["\\(", "Struct", "\\)"]                       "Group"            -- (let x = 1; x)
  , ERule "Name"      ["?[A-Za-z][A-Za-z_$0-9-]*"]                   "Name"             -- foo-bar
  ]

main :: IO ()
main = do
  line <- readFile "test.ml"
  let test' = compileETable test
  case tokenize test' line of
    Right input -> do
      print (parse test' input)

    Left err -> do
      error (show err)
