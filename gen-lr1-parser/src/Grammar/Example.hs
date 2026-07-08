module Grammar.Example where

import Grammar.Structure (Grammar, makeGrammar)
import Rule              (mkRule)
import Term              (Point(..))

-- grammarTest :: Grammar
-- grammarTest = makeGrammar "Expr"
--   [ mkRule "Expr" [E (Just "expr") "Expr", T Nothing "+", E (Just "factor") "Factor"]
--       "expr :+ factor"

--   , mkRule "Expr" [E (Just "factor") "Factor"]
--       "Factor factor"

--   , mkRule "Factor" [E (Just "factor") "Factor", T Nothing "*", E (Just "term") "Term"]
--       "factor :* term"

--   , mkRule "Factor" [E (Just "term") "Term"]
--       "Term term"

--   , mkRule "Term" [T (Just "n") "<num>"]
--       "Number n"

--   , mkRule "Term" [T Nothing "(", E (Just "expr") "Expr", T Nothing ")"]
--       "Group expr"
--   ]
-- grammarTest :: Grammar
-- grammarTest = makeGrammar
--   [ mkRule "S" [E Nothing "Expr"] "S = E"
--   , mkRule "Expr" [T Nothing "a", E Nothing "X", T Nothing "d"] "E = a X d"
--   , mkRule "Expr" [T Nothing "b", E Nothing "X", T Nothing "c"] "E = b X c"
--   , mkRule "Expr" [T Nothing "b", E Nothing "Y", T Nothing "d"] "E = b y d"
--   , mkRule "X" [T Nothing "expr", E Nothing "X"] "X = expr X"
--   , mkRule "X" [T Nothing "expr"] "X = expr"
--   , mkRule "Y" [T Nothing "expr", E Nothing "Y"] "Y = expr Y"
--   , mkRule "Y" [T Nothing "expr"] "Y = expr"
--   ]


{-
E = a X d
  | b X c
  | b Y d

X = expr X
  | expr

Y = expr Y
  | expr
-}
