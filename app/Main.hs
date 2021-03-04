
import Prelude hiding (lex)

import S
import LR
import Lex

test :: Table S
test = Table
  [ Rule  Start      ["Struct"]                                     "Start"
  , Rule "Struct"    ["\\\\", "Name", "->", "Struct"]               "Lam"              -- \s -> s s
  , Rule "Struct"    ["Expr", "<\\|", "Struct"]                     "Feed"              -- \s -> s s
  , Rule "Struct"    ["let", "Name", "=", "Struct", ";", "Struct"]  "Let"              -- let id = \x -> x; id 42
  , Rule "Struct"    ["Add"]                                       "-LetExpr"
  , Rule "Add"       ["Add", "\\-", "Unary"]                        "Minus"            -- foo x - b
  , Rule "Add"       ["Add", "\\+", "Unary"]                        "Plus"             -- x + sum ys
  , Rule "Add"       ["Unary"]                                     "-Add"
  , Rule "Unary"     ["\\-", "Unary"]                               "Negate"           -- -x
  , Rule "Unary"     ["\\+", "Unary"]                               "Noop"             -- +y
  , Rule "Unary"     ["Expr"]                                      "-U"
  , Rule "Expr"      ["Expr", "Term"]                               "Call"             -- sum xs
  , Rule "Expr"      ["Term"]                                      "-ExprTerm"
  , Rule "Term"      ["Name"]                                       "Var"
  , Rule "Term"      ["\\(", "Struct", "\\)"]                       "Group"            -- (let x = 1; x)
  , Rule "Name"      ["?[A-Za-z][A-Za-z_$0-9-]*"]                   "Name"             -- foo-bar
  ]

main :: IO ()
main = do
  line <- readFile "test.ml"
  case tokenize test line of
    Right input -> do
      print (parse test input)

    Left err -> do
      error (show err)
