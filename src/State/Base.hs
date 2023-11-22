
module State.Base where

import Control.Monad.State
import Control.Monad.Reader
import Data.Set qualified as Set
import Data.Hashable
import Data.Function
import Data.List

import Grammar
import First
import Point
import Position
import Data.Map.Monoidal qualified as Map
import Monoidal (show', search)

data Kernel = MkKernel
  { set  :: Set.Set Position
  , hash :: Int
  }

mkKernel :: Set.Set Position -> Kernel
mkKernel set = MkKernel
  { set
  , hash = hash (Set.map (.hash) set)
  }

instance Eq  Kernel where (==)    = (==)    `on` (.hash)
instance Ord Kernel where compare = compare `on` (.hash)

instance Semigroup Kernel where
  (<>) = error "Kernel.<>"

instance Monoid Kernel where
  mempty = error "Kernel.mempty"

data Vertex = MkVertex
  { kernel  :: Kernel
  , closure :: Set.Set Position
  }

instance Semigroup Vertex where
  l <> r = error "Vertex.<>"

instance Monoid Vertex where
  mempty = MkVertex mempty mempty

instance Eq  Vertex where (==)    = (==)    `on` (.kernel)
instance Ord Vertex where compare = compare `on` (.kernel)

mkVertex :: Kernel -> Set.Set Position -> Vertex
mkVertex kernel closure = MkVertex
  { kernel
  , closure
  }

type CacheM = ReaderT (Grammar, First) (State (Map.Map Kernel Vertex))

evalCacheM :: Grammar -> CacheM a -> a
evalCacheM grammar ma = evalState (runReaderT ma (grammar, mkFirst grammar)) mempty

initial :: CacheM Vertex
initial = do
  (grammar, _) <- ask
  closure do
    mkKernel (Set.map (`start` "$") (grammar.rules Map.! "S"))

closure :: Kernel -> CacheM Vertex
closure kernel = do
  gets (Map.lookup kernel) >>= \case
    Just vertex -> return vertex
    Nothing -> do
      (grammar, first) <- ask
      let vertex = mkClosure grammar first kernel
      modify (Map.insert kernel vertex)
      return vertex

nextOn :: Vertex -> Point -> CacheM Vertex
nextOn vertex point = do
  (grammar, first) <- ask
  let kernel = mkNextOn grammar first vertex point
  closure kernel

mkClosure :: Grammar -> First -> Kernel -> Vertex
mkClosure grammar first kernel =
  mkVertex kernel (search simultaneous kernel.set)
  where
    simultaneous :: Position -> Set.Set Position
    simultaneous pos =
      case locus pos of
        Just (Entity e) -> do
          let
            lookaheads =
              case locus =<< next pos of
                Nothing -> Set.singleton pos.lookahead
                Just pt -> first.table Map.! pt

          startAll (grammar.rules Map.! e) lookaheads

        _ -> Set.empty

expected :: Vertex -> Set.Set Point
expected MkVertex {closure} = foldMap positionExpects closure
  where
    positionExpects pos = case locus pos of
      Nothing -> mempty
      Just pt -> Set.singleton pt

mkNextOn :: Grammar -> First -> Vertex -> Point -> Kernel
mkNextOn grammar first state point =
  mkKernel (foldMap nextPosition state.closure)
  where
    nextPosition :: Position -> Set.Set Position
    nextPosition pos
      | locus pos == Just point = maybe mempty Set.singleton (next pos)
      | otherwise               = mempty
