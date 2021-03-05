
-- | A GOTO function - second component of the LR(1)-table.
--
module Goto where

import Map (Map, (==>))
import Map qualified as Map
import Set (Set)
import Set qualified as Set

import Item
import Point
import Pretty
import Table
import Util
import Term

-- | GOTO function.
--
type Goto term
  =  State term
  -> Point term
  -> State term

-- | GOTO function, memoized.
--
type Goto' term
  = Map (State term)
  ( Map (Point term)
  ( State term))

-- | Generate GOTO function from table, FIRSTS and FOLLOWS.
--
--   For each `next` of all productions in the set, we generate a
--   @GOTO (state, prod.locus) = CLOSURE (prod.next)@
--
getGoto
  :: forall term
  .  (Ord term, Pretty term)
  => Table term  -- ^ parsing table
  -> Firsts (Term term)        -- ^ FIRSTS function (set of first terminals of
                        -- ^ a production)
  -> Follows (Term term)       -- ^ FOLLOWS function (set of terminals that can go
                        --   after a production)
  -> Goto' (Term term)
getGoto table firsts follows = close collect (getFirstStateOfTable table firsts follows ==> mempty)
  where
    collect :: Goto' (Term term) -> Goto' (Term term)
    collect = foldMap move . Map.keySet

    move :: State (Term term) -> Goto' (Term term)
    move states = states ==> foldMap step states
      where
        step :: Item1 (Term term) -> Map (Point (Term term)) (State (Term term))
        step item =
          case locus item of
            Just term -> do
              let following = next item
              term ==>
                ( getClosure table firsts follows
                $ possible Set.ofOne following
                )
            Nothing -> do
              mempty

-- | Generate a set of all possible states for a grammar.
--
getItems
  :: forall term
  .  (Ord term, Pretty term)
  => Goto' term        -- ^ GOTO function
  -> Set (State term)
getItems = Map.keySet
