
module Goto where

import Data.Function (on)
import Data.List (partition, sortBy, groupBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ord (comparing)

import Map (Map, (==>))
import Map qualified as Map
import Set (Set)
import Set qualified as Set

import Item
import Name
import Point
import Pretty
import Rule
import Table
import Util

type Goto term result
  =  Set (Item1 term result)
  -> Point term
  -> Set (Item1 term result)

type Goto' term result
  = Map (Set (Item1 term result))
  ( Map (Point term)
  ( Set (Item1 term result)))

getGoto
  :: (Ord term, Pretty term)
  => Table term result
  -> Firsts term
  -> Follows term
  -> Goto term result
getGoto rules firsts follows (Set.toList -> items) term =
  mconcat
    [ getClosure rules firsts follows
    $ possible Set.ofOne following
    | item <- items
    , locus item == Just term
    , let following = next item
    ]

getItems
  :: forall term result
  .  (Ord term, Pretty term)
  => Set (Point term)
  -> Goto term result
  -> Set (Item1 term result)
  -> Set (Set (Item1 term result))
getItems terminals goto firstState
  = close (foldMap collect)
  $ Set.ofOne firstState
  where
    collect :: Set (Item1 term result) -> Set (Set (Item1 term result))
    collect items = foldMap (Set.ofOne . goto items) terminals
