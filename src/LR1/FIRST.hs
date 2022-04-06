{-# OPTIONS_GHC -Wno-orphans #-}

{- |
  FIRST table.

  Table of first terms for all nonterminals in the grammar.
-}

module LR1.FIRST where

import Data.Foldable (toList)
import Data.Function ((&))
import Data.Set (Set)
import Data.Set qualified as Set

import LR1.Grammar qualified as Grammar
import LR1.Map     qualified as Map
import LR1.Point   qualified as Point
import LR1.Rule    qualified as Rule
import LR1.Term    qualified as Term
import LR1.Utils (Get ((?)), fixpoint, (==>), one)
import Data.Text (Text)

{- |
  A map `NonTerm.T` -> (`Set` `Term.T`).
-}
newtype T = FIRST
  { unwrap :: Map.T Text (Set Term.T)
  }

instance Show LR1.FIRST.T where
  show (FIRST firsts) =
    firsts
      & Map.toList
      & fmap do \(entity, terms) -> show entity <> "\t= {" <> (terms & toList & fmap show & unwords) <> "}"
      & unlines

instance Get LR1.FIRST.T Text  (Set Term.T) where
  FIRST fs ? entity = fs ? entity

{- |
  Generate FIRST table from the grammar.
-}
make :: Grammar.T -> LR1.FIRST.T
make Grammar.Grammar {rules, s}
  = FIRST
  $ fixpoint do s ==> Set.empty
  $ \firsts -> rules
      & (foldMap.foldMap) \Rule.Rule {points, entity} ->
        case head points of
          Point.Term    term    -> entity ==> one term
          Point.NonTerm entity1 -> entity ==> firsts ? entity1
