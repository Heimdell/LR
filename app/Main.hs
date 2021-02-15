
import S
import Tree
import LR

test :: Table S (Tree S)
test = Table
  [ Rule Start    ["If"]                                    head
  , Rule "If"     ["if", "If", "then", "If", "else", "If"] (Join "ite")
  , Rule "If"     ["Expr"]                                  head
  , Rule "Expr"   ["Expr", "+", "Factor"]                  (Join "+")
  , Rule "Expr"   ["Factor"]                                head
  , Rule "Factor" ["Factor", "*", "Unary"]                 (Join "*")
  , Rule "Factor" ["Unary"]                                 head
  , Rule "Unary"  ["-", "Unary"]                           (Join "-")
  , Rule "Unary"  ["Term"]                                  head
  , Rule "Term"   ["(", "If", ")"]                         (!! 1)
  , Rule "Term"   ["id"]                                    head
  ]

main :: IO ()
main = print $ parse test Leaf id "$" ["id", "*", "id", "+", "-", "id", "*", "id", "$"]
