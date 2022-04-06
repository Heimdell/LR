{-# OPTIONS_GHC -Wno-orphans #-}

{- |
  Representation for grammar.
-}
module LR1.Grammar where

import Data.Foldable qualified as Set
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)

import LR1.Map     qualified as Map
import LR1.Rule    qualified as Rule
import LR1.Utils (one, (==>), Get ((?)))

{- |
  A map `NonTerm.T` -> `Set` `Rule.T` and a list of rules in reverse order of adding
  (for pretty-printing).
-}
data T = Grammar
  { rules :: Map.T Text (Set Rule.T)
  , order :: [] Rule.T
  , s     :: Text
  }

instance Show LR1.Grammar.T where
  show (Grammar _ order s) =
    show s <> "\n" <>
    (order
      & reverse
      & fmap show
      & unlines)

{- |
  Add rule to the grammar.
-}
add :: Rule.T -> LR1.Grammar.T -> LR1.Grammar.T
add rule grammar@Grammar {rules, order} =
  grammar
    { rules = rules <> Rule.entity rule ==> one rule
    , order = rule : order
    }

{- |
  An empty grammar.
-}
empty :: Text -> LR1.Grammar.T
empty = Grammar mempty []

instance Get LR1.Grammar.T Text (Set Rule.T) where
  Grammar {rules} ? entity
    = Map.lookup entity rules
    & fromMaybe (error $ "undefined " <> show entity)

{- |
  Get a rule for `NonTerm.Start` from the grammar.

  Grammar must have one and only one rule for `NonTerm.Start`.
-}
firstRule :: LR1.Grammar.T -> Rule.T
firstRule grammar = head $ Set.toList $ grammar ? s grammar

{- |
  Make grammar from a list of rules.
-}
fromRules :: Text -> [] Rule.T -> LR1.Grammar.T
fromRules = foldr add . LR1.Grammar.empty
