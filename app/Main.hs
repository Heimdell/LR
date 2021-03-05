
import Prelude hiding (lex)

import S
import LR
import Lex
import Exts
import Pretty

test :: ETable S
test = ETable
  [ ERule  Start      ["Struct"]                                     "Start"
  , ERule "Struct"    ["\\\\", "Name", "->", "Struct"]               "Lam"              -- \s -> s s
  , ERule "Struct"    ["fixity", EOpt $ EOr ["left", "right", "none"], "Number", EPlus "Op", ";", "Struct"] "fixity"
  , ERule "Struct"    ["let", "Name", "=", "Struct", ";", "Struct"]  "Let"              -- let id = \x -> x; id 42
  , ERule "Struct"    ["Unary", "Op", "Struct"]                      "Soup"              -- \s -> s s
  , ERule "Struct"    ["Unary"]                                     "-LetExpr"
  , ERule "Unary"     ["Op", "Unary"]                                "Noop"             -- +y
  , ERule "Unary"     ["Expr"]                                      "-U"
  , ERule "Expr"      ["Expr", "Term"]                               "Call"             -- sum xs
  , ERule "Expr"      ["Term"]                                      "-ExprTerm"
  , ERule "Term"      ["Name"]                                       "Var"
  , ERule "Term"      ["Number"]                                     "Var"
  , ERule "Term"      ["\\(", "Struct", "\\)"]                       "Group"            -- (let x = 1; x)
  , ERule "Term"      [  "{", EOpt $ ESeq ["Field",  EMult $ ESeq [",", "Field"]],    "}"]                       "Record"            -- (let x = 1; x)
  , ERule "Term"      ["\\[", EOpt $ ESeq ["Struct", EMult $ ESeq [",", "Struct"]], "\\]"]                       "Record"            -- (let x = 1; x)
  , ERule "Field"     ["Name", "=", "Struct"]                        "Field"
  , ERule "Name"      ["?[A-Za-z][A-Za-z_$0-9-]*"]                   "Name"             -- foo-bar
  , ERule "Op"        ["?[-+\\|><!~@#$%^&*=\\\\/\\?.,]+"]            "Op"             -- foo-bar
  , ERule "Number"    ["?[-+]?([0-9]*[.])?[0-9]+([eE][-+]?\\d+)?"]    "Num"             -- foo-bar
  ]

main :: IO ()
main = do
  line <- readFile "test.ml"
  let test' = compileETable test
  case tokenize test' line of
    Right input -> do
      print $ either id pretty $ parse test' input

    Left err -> do
      error (show err)
