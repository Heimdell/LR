
import Prelude hiding (lex)

import Pretty
import S
import Tree
import LR

data CST
  = If   CST CST CST
  | Add  CST CST
  | Mult CST CST
  | Neg  CST
  | Id   S
  | Kw   S
  deriving stock Show

test :: Table S CST
-- test = Table
  -- [ Rule  Start ["S"]           (Join "Start")
  -- , Rule "S"    ["A", "a"]      (Join "S1")
  -- , Rule "S"    ["b", "A", "c"] (Join "S2")
  -- , Rule "S"    ["B", "c"]      (Join "S3")
  -- , Rule "S"    ["b", "B", "a"] (Join "S4")
  -- , Rule "A"    ["d"]           (Join "A")
  -- , Rule "B"    ["d"]           (Join "B")
  -- ]
test = Table
  [ Rule Start    ["If"]                                    head
  , Rule "If"     ["if", "If", "then", "If", "else", "If"] (\s -> If   (s !! 1) (s !! 3) (s !! 5))
  , Rule "If"     ["Expr"]                                  head
  , Rule "Expr"   ["Expr", "+", "Factor"]                  (\s -> Add  (s !! 0) (s !! 2))
  , Rule "Expr"   ["Factor"]                                head
  , Rule "Factor" ["Factor", "*", "Unary"]                 (\s -> Mult (s !! 0) (s !! 2))
  , Rule "Factor" ["Unary"]                                 head
  , Rule "Unary"  ["-", "Unary"]                           (Neg . (!! 1))
  , Rule "Unary"  ["Term"]                                  head
  , Rule "Term"   ["(", "If", ")"]                         (!! 1)
  , Rule "Term"   ["id"]                                    head
  ]

wrap :: T -> CST
wrap (TID s) = Id s
wrap (TKW s) = Kw s

reserved = [] :  words "if then else ( ) + * -"

data T
  = TKW S
  | TID S

instance Pretty T where
  pretty = \case
    TKW s -> color 2 $ pretty s
    TID s -> color 3 $ pretty s

classify :: T -> S
classify (TKW s) =  s
classify (TID _) = "id"

toT :: String -> T
toT s | elem s reserved = TKW (S s)
toT s                   = TID (S s)

main :: IO ()
main = do
  line <- getLine
  print $ parse test wrap classify "" $ map toT $ words line ++ [""]
