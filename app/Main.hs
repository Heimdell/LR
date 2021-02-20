
import Prelude hiding (lex)

import Pretty
import S
import Tree
import LR
import Lex
import Point
import Set qualified

data Lexeme
  = Keyword S
  | Name
  | Num
  | Str
  deriving stock (Eq, Ord, Show)

instance Pretty Lexeme where
  pretty = text . show

test :: Table Lexeme
test = Table
  [ Rule  Start      ["Let"]                                           "-Start"
  , Rule "Let"       [kw "let", "Name", kw "=", "Let", kw ";", "Let"]  "Let"
  , Rule "Let"       ["Expr"]                                          "-LetExpr"
  , Rule "Expr"      ["Expr", "Term"]                                  "Expr"
  , Rule "Expr"      ["Term"]                                          "-ExprTerm"
  , Rule "Term"      ["Name"]                                          "-Var"
  , Rule "Term"      [kw "fun", "Name", kw "->", "Let", kw "end"]      "Lam"
  , Rule "Term"      [kw "\\(", "Expr", kw "\\)"]                      "Group"
  , Rule "Name"      [name]                                            "-Name"
  ]

kw :: String -> Point Lexeme
kw s = Term (Keyword $ S s)

name :: Point Lexeme
name = Term Name

reserved = "\\[ \\] \\{ \\} : , ; fun -> \\( \\) let = in end"

keywords :: String -> [(Lexeme, Regex)]
keywords = map (\s -> (Keyword (S s), token s)) . words

main :: IO ()
main = do
  line <- readFile "test.ml"
  let
    Right input = tokenizer
      (    keywords reserved
      ++ [(Name, token "[A-Za-z][A-Za-z_$0-9]*")]
      )
      line
  print $ parse test (Keyword "") (input ++ [(Keyword "", "")])
