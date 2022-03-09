{-# OPTIONS_GHC -Wno-orphans #-}
module LR1.Fixpoint where

import Control.Applicative (Applicative(liftA2))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)

import LR1.Map qualified as Map

(==>) :: k -> v -> Map.T k v
(==>) = (Map.==>)

infixr 7 ==>

mmap :: (Ord k) => [(k, a)] -> Map.T k a
mmap = Map.fromList

set :: (Ord k) => [k] -> Set k
set = Set.fromList

one :: (Ord k) => k -> Set k
one = Set.singleton

class (Monoid d, Eq d) => Diff d where
  diff :: d -> d -> d

instance Ord k => Diff (Set k) where
  diff = Set.difference

instance (Ord k, Diff v) => Diff (Map.T k v) where
  diff part known
    = part
    & Map.mapWithKey do \k v -> v `diff` (known ? k)
    & Map.filter (/= mempty)

class (Ord k) => Get w k v | w -> k v where
  (?) :: w -> k -> v

infixl 8 ?

instance (Ord k, Monoid v) => Get (Map.T k v) k v where
  m ? k = Map.lookup k m & fromMaybe mempty

fixpoint :: (Diff a) => a -> (a -> a) -> a
fixpoint start step =
  let next = step start `diff` start
  in if next == mempty
     then start
     else fixpoint (next <> start) step

fixpointM :: (Diff a, Monad m) => (a -> m a) -> a -> m a
fixpointM step start = do
  next <- step start <&> (`diff` start)
  if next == mempty
  then return start
  else fixpointM step (next <> start)

foldMapM :: (Applicative m, Foldable f, Monoid v) => (a -> m v) -> f a -> m v
foldMapM f xs = unMonoM $ foldMap (MonoM . f) xs

newtype MonoM m v = MonoM { unMonoM :: m v }
  deriving newtype (Functor, Applicative)

instance (Applicative m, Semigroup v) => Semigroup (MonoM m v) where
  (<>) = liftA2 (<>)

instance (Applicative m, Monoid v) => Monoid (MonoM m v) where
  mempty = pure mempty

deriving stock instance (Generic a, Generic b) => Generic (Map.T a b)

newtype PMap k v = PMap { unPMap :: Map.T k v }

instance (Show k, Show v) => Show (PMap k v) where
  show m = m
    & unPMap
    & Map.toList
    & fmap do \(k, v) -> show k <> " => " <> show v
    & List.intercalate ", "
    & (<> "}")
    & ("{" <>)
