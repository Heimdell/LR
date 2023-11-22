
module Monoidal where

import Prelude hiding (map)

import Data.Map.Monoidal qualified as Map
import Data.Map.Monoidal (Map)
import Data.Set qualified as Set
import Data.Foldable
import Data.Coerce

import Colored

show' :: (Show k, Show v) => Map k (Set.Set v) -> String
show' = unlines . fmap (uncurry bind) . Map.toList . Map.map Set.toList
  where
    bind k vs = show k <> magenta " : " <> unwords (fmap show vs)

fixpoint :: (Eq a, Semigroup a) => (a -> a) -> a -> a
fixpoint step acc = do
  let delta = step acc
  let acc' = acc <> delta
  if acc' /= acc
  then fixpoint step acc'
  else acc

search :: forall k. (Ord k) => (k -> Set.Set k) -> Set.Set k -> Set.Set k
search nexts kernel = go kernel mempty
  where
    go :: Set.Set k -> Set.Set k -> Set.Set k
    go kernel acc = do
      case Set.minView kernel of
        Nothing -> acc
        Just (el, kernel') -> do
          if Set.member el acc
          then go              kernel'                 acc
          else go (nexts el <> kernel') (Set.insert el acc)

search''
  :: forall k p m
  .  ( Ord k
     , Semigroup k
     , Ord p
     , Monad m
     )
  => (k -> m (Map p k))
  -> k
  -> m (Map k (Map p k))
search'' step start = loop (Set.singleton start) mempty
  where
    loop :: Set.Set k -> Map k (Map p k) -> m (Map k (Map p k))
    loop keys goto = do
      case Set.minView keys of
        Nothing -> return goto
        Just (el, keys') -> do
          if Map.member el goto
          then loop keys' goto
          else do
            new <- step el
            loop (keys' <> foldMap Set.singleton new) (Map.insert el new goto)
