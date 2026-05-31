module Fixpoint where

import Data.Function ((&))
import Data.Set      (Set)

import Data.Set qualified as Set

{- |
  Calculate a simple fixpoint over a monoidal object.
-}
fixpoint :: (Monoid vertex, Eq vertex) => (vertex -> vertex) -> vertex -> vertex
fixpoint f = go
  where
    go vertex = do
      let vertex'  = f vertex
      let vertex'' = vertex <> vertex'
      if vertex == vertex''
        then    vertex
        else go vertex''

infix 3 >>-

{-# INLINE (>>-) #-}
(>>-) :: (Foldable f, Monoid m) => f vertex -> (vertex -> m) -> m
(>>-) = flip foldMap

{- |
  Construct a graph from adjacency relation.
-}
graphClosure ::
     forall vertex graph
  .  ( Ord vertex
     , Monoid graph
     )
  => (vertex ->  graph)    -- ^ edges, adjacent to a given vertex
  -> (graph  -> [vertex])  -- ^ get all endpoint vertices
  ->  vertex               -- ^ vertex to start from
  ->  graph

graphClosure adjacent endpoints origin = go Set.empty mempty [origin]
  where
    go :: Set vertex -> graph -> [vertex] -> graph
    go visitedSet accum stack = case stack of
      []            -> accum
      vertex : rest -> do
        let graph    = adjacent vertex
        let vertices = endpoints graph & filter (`Set.notMember` visitedSet)
        go (Set.insert vertex visitedSet) (accum <> graph) (vertices ++ rest)
