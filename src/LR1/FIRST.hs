{-# OPTIONS_GHC -Wno-orphans #-}
module LR1.FIRST where

import LR1.Fixpoint (Get ((?)), fixpoint, (==>), one)
import LR1.NonTerm qualified as NonTerm
import Data.Set (Set)
import LR1.Term qualified as Term
import LR1.Map qualified as Map
import Data.Function ((&))
import Data.Foldable (toList)
import LR1.Grammar qualified as Grammar
import Data.Set qualified as Set
import LR1.Rule qualified as Rule
import LR1.Point qualified as Point

newtype T = FIRST
  { unwrap :: Map.T NonTerm.T (Set Term.T)
  }

instance Show LR1.FIRST.T where
  show (FIRST firsts) =
    firsts
      & Map.toList
      & fmap do \(entity, terms) -> show entity <> "\t= {" <> show (terms & toList & fmap show & unwords) <> "}"
      & unlines

instance Get LR1.FIRST.T NonTerm.T (Set Term.T) where
  FIRST fs ? entity = fs ? entity

make :: Grammar.T -> LR1.FIRST.T
make Grammar.Grammar {rules}
  = FIRST
  $ fixpoint do NonTerm.Start ==> Set.empty
  $ \firsts -> rules
      & (foldMap.foldMap) \Rule.Rule {points, entity} ->
        case head points of
          Point.Term    term    -> entity ==> one term
          Point.NonTerm entity1 -> entity ==> firsts ? entity1
