
module First where

import Data.Set qualified as Set
import Data.Map.Monoidal qualified as Map
import Data.Function

import Monoidal
import Grammar
import Point
import Rule

newtype First = MkFirst { table :: Map.Map Point (Set.Set Term) }

mkFirst :: Grammar -> First
mkFirst grammar = MkFirst (fixpoint step mempty)
  where
    step :: Map.Map Point (Set.Set Term) -> Map.Map Point (Set.Set Term)
    step first = allPoints grammar & foldMap \e ->
      Map.singleton e case e of
        Term   t -> Set.singleton t
        Entity e -> grammar.rules Map.! e
          & foldMap \rule ->
              first Map.! firstPoint rule

instance Show First where
  show first = show' first.table