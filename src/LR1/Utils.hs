{-# OPTIONS_GHC -Wno-orphans #-}

{- |
  Various helpers and utilities.
-}
module LR1.Utils where

import Control.Applicative (Applicative(liftA2))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)

import LR1.Map qualified as Map

{- |
  `Map.singleton`, but for custom monoidal map.

  I don't use @monoidal-container@, because it depends on @lens@.
-}
(==>) :: k -> v -> Map.T k v
(==>) = (Map.==>)

infixr 7 ==>

{- |
  `Map.fromList`, for custom monoidal map.
-}
mmap :: (Ord k) => [(k, a)] -> Map.T k a
mmap = Map.fromList

{- |
  `Set.fromList`.
-}
set :: (Ord k) => [k] -> Set k
set = Set.fromList

{- |
  `Set.singleton`.
-}
one :: (Ord k) => k -> Set k
one = Set.singleton

{- |
  "Monoidal" difference, an inverse for `<>` (threated as "plus").
-}
class (Monoid d, Eq d) => Diff d where
  diff :: d -> d -> d

instance Ord k => Diff (Set k) where
  diff = Set.difference

instance (Ord k, Diff v) => Diff (Map.T k v) where
  diff part known
    = part
    & Map.mapWithKey do \k v -> v `diff` (known ? k)
    & Map.filter (/= mempty)

{- |
  Container access. Most of the containers implementing this are monoidal, thus
  no need for `Maybe`.
-}
class (Ord k) => Get w k v | w -> k v where
  (?) :: w -> k -> v

infixl 8 ?

instance (Ord k, Monoid v) => Get (Map.T k v) k v where
  m ? k = Map.lookup k m & fromMaybe mempty

{- |
  Apply step function to starting value repeatedly, until value stops changing.

  This is only used for FIRST table and CLOSURE, and both don't grow that much.
-}
fixpoint
  :: (Diff a)
  => a         -- ^ starting value
  -> (a -> a)  -- ^ step function
  -> a
fixpoint start step =
  let next = step start `diff` start
  in if next == mempty
     then start
     else fixpoint (next <> start) step

{- |
  Same as `fixpoint`, but monadic.
-}
fixpointM :: (Diff a, Monad m) => (a -> m a) -> a -> m a
fixpointM step start = do
  next <- step start <&> (`diff` start)
  if next == mempty
  then return start
  else fixpointM step (next <> start)

{- |
  Why isn't this in the @base@, anyway?
-}
foldMapM :: (Applicative m, Foldable f, Monoid v) => (a -> m v) -> f a -> m v
foldMapM f xs = unMonoM $ foldMap (MonoM . f) xs

{- |
  To be used in `foldMapM` so I can implement it with no `Traversable` on container.
-}
newtype MonoM m v = MonoM { unMonoM :: m v }
  deriving newtype (Functor, Applicative)

{- |
  Provides `Semigroup` on @m a@, if @a@ is a `Semigroup` itself.
-}
instance (Applicative m, Semigroup v) => Semigroup (MonoM m v) where
  (<>) = liftA2 (<>)

instance (Applicative m, Monoid v) => Monoid (MonoM m v) where
  mempty = pure mempty

{- |
  Probably, someone might want to generate tables in haskell and use them elsewhere.

  This makes it trivial: `derive anyclass (ToJSON)`.
-}
deriving stock instance (Generic a, Generic b) => Generic (Map.T a b)

{- |
  "Pretty Map". Has nice `Show` instance.
-}
newtype PMap k v = PMap { unPMap :: Map.T k v }

instance (Show k, Show v) => Show (PMap k v) where
  show m = m
    & unPMap
    & Map.toList
    & fmap do \(k, v) -> show k <> " => " <> show v
    & List.intercalate ", "
    & (<> "}")
    & ("{" <>)
