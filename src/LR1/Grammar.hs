{-# OPTIONS_GHC -Wno-orphans #-}
module LR1.Grammar where

import LR1.Fixpoint (Map, one, (==>), Get ((?)))
import LR1.NonTerm qualified as NonTerm
import LR1.Rule qualified as Rule
import Data.Set (Set)
import Data.Map.Monoidal qualified as Map
import Data.Function ((&))
import Data.Text (Text)
import LR1.Point qualified as Point
import Data.Maybe (fromMaybe)
import qualified Data.Foldable as Set

data T = Grammar
  { rules :: Map NonTerm.T (Set Rule.T)
  , order :: [] Rule.T
  }

instance Show LR1.Grammar.T where
  show (Grammar _ order) =
    order
      & reverse
      & fmap show
      & unlines

addInternal :: Rule.T -> LR1.Grammar.T -> LR1.Grammar.T
addInternal rule grammar@Grammar {rules, order} =
  grammar
    { rules = rules <> Rule.entity rule ==> one rule
    , order = rule : order
    }

empty :: LR1.Grammar.T
empty = Grammar mempty []

add :: NonTerm.T -> Text -> [] Point.T -> LR1.Grammar.T -> LR1.Grammar.T
add entity label points = addInternal $ Rule.Rule {entity, label, points}

instance Get LR1.Grammar.T NonTerm.T (Set Rule.T) where
  Grammar {rules} ? entity
    = Map.lookup entity rules
    & fromMaybe (error $ "undefined " <> show entity)

firstRule :: LR1.Grammar.T -> Rule.T
firstRule grammar = head $ Set.toList $ grammar ? NonTerm.Start
