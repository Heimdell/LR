
import Prelude hiding (lex)

import Pretty
import S
import LR
import Lex

test :: Table S
test = Table
  [ Rule  Start      ["Let"]                                  "-Start"
  , Rule "Let"       ["let", "Name", "=", "Let", ";", "Let"]   "Let"
  , Rule "Let"       ["Expr"]                                 "-LetExpr"
  , Rule "Expr"      ["Expr", "Term"]                          "Expr"
  , Rule "Expr"      ["Term"]                                 "-ExprTerm"
  , Rule "Term"      ["Name"]                                 "-Var"
  , Rule "Term"      ["fun", "Name", "->", "Let", "end"]       "Lam"
  , Rule "Term"      ["\\(", "Expr", "\\)"]                    "Group"
  , Rule "Name"      ["?[A-Za-z][A-Za-z_$0-9]*"]              "-Name"
  ]

main :: IO ()
main = do
  line <- readFile "test.ml"
  case tokenize test line of
    Right input -> do
      print $ parse test input

    Left err -> do
      error $ show err
