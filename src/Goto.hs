
-- | A GOTO function - second component of the LR(1)-table.
--
module Goto where

import Map (Map)
import Set (Set)
import Set qualified as Set

import Item
import Point
import Pretty
import Table
import Util

-- | GOTO function.
--
type Goto term result
  =  State term result
  -> Point term
  -> State term result

-- | GOTO function, memoized.
--
type Goto' term result
  = Map (State term result)
  ( Map (Point term)
  ( State term result))

-- | Generate GOTO function from table, FIRSTS and FOLLOWS.
--
--   For each `next` of all productions in the set, we generate a
--   @GOTO (state, prod.locus) = CLOSURE (prod.next)@
--
getGoto
  :: (Ord term, Pretty term)
  => Table term result  -- ^ parsing table
  -> Firsts term        -- ^ FIRSTS function (set of first terminals of
                        -- ^ a production)
  -> Follows term       -- ^ FOLLOWS function (set of terminals that can go
                        --   after a production)
  -> Goto term result
getGoto rules firsts follows (Set.toList -> items) term =
  mconcat
    [ getClosure rules firsts follows
    $ possible Set.ofOne following
    | item <- items
    , locus item == Just term
    , let following = next item
    ]

-- | Generate a set of all possible states for a grammar.
--
getItems
  :: forall term result
  .  (Ord term, Pretty term)
  => Set (Point term)        -- ^ set of terminals and non-terminals
  -> Goto term result        -- ^ GOTO function
  -> State term result       -- ^ first state of a parser
  -> Set (State term result)
getItems terminals goto firstState
  = close (foldMap collect)
  $ Set.ofOne firstState
  where
    collect :: State term result -> Set (State term result)
    collect items = foldMap (Set.ofOne . goto items) terminals
