
-- | The wrapper around `Impl.Map`.
--
--   Standard haskell `Impl.Map` has `Monoid` instance that does not merge
--   values, it only merges keys - and my algorithms become so much cleaner
--   if it do.
--
module LR1.Map where

import Data.Map qualified as Impl

-- | A wrapper.
--
newtype T k v = Map { unMap :: Impl.Map k v }
  deriving newtype (Eq, Ord, Show, Functor, Foldable)
  deriving stock (Traversable)

instance (Ord k, Monoid v) => Monoid (LR1.Map.T k v) where
  mempty = Map mempty

instance (Ord k, Semigroup v) => Semigroup (LR1.Map.T k v) where
  Map l <> Map r = Map $ Impl.unionWith (<>) l r

-- | `Impl.fromList`.
--
fromList :: Ord k => [(k, v)] -> LR1.Map.T k v
fromList = Map . Impl.fromList

-- | `Impl.lookup`.
--
lookup :: Ord k => k -> LR1.Map.T k v -> Maybe v
lookup k (Map m) = Impl.lookup k m

-- | `Impl.toList`.
--
toList :: LR1.Map.T k v -> [(k, v)]
toList (Map m) = Impl.toList m

-- | `Impl.map`.
--
map :: (a -> b) -> LR1.Map.T k a -> LR1.Map.T k b
map f (Map m) = Map $ Impl.map f m

-- | `Impl.filter`.
--
filter :: (a -> Bool) -> LR1.Map.T k a -> LR1.Map.T k a
filter f (Map m) = Map $ Impl.filter f m

-- | `Impl.mapWithKey`.
--
mapWithKey :: (k -> a -> b) -> LR1.Map.T k a -> LR1.Map.T k b
mapWithKey f (Map m) = Map $ Impl.mapWithKey f m

-- | `Impl.insert`.
--
insert :: Ord k => k -> v -> LR1.Map.T k v -> LR1.Map.T k v
insert k v = Map . Impl.insert k v . unMap

-- | `Impl.keysSet` (sic!).
--
keys :: LR1.Map.T k v -> [k]
keys = Impl.keys . unMap

-- | `Impl.elems`.
--
values :: LR1.Map.T k v -> [v]
values = Impl.elems . unMap

-- | `Impl.foldrWithKey`.
--
foldrWithKey :: (k -> a -> b -> b) -> b -> LR1.Map.T k a -> b
foldrWithKey f z (Map m) = Impl.foldrWithKey f z m

-- | `Impl.null`.
--
null :: LR1.Map.T k v -> Bool
null (Map m) = Impl.null m

-- | `Impl.size`.
--
size :: LR1.Map.T k v -> Int
size (Map m) = Impl.size m

-- | `Impl.singleton`.
--
(==>) :: k -> v -> LR1.Map.T k v
(==>) = (Map .) . Impl.singleton

infixr 7 ==>

-- | `Impl.singleton`.
--
empty :: LR1.Map.T k v
empty = Map Impl.empty

-- | `(Impl.!)`.
--
(!) :: Ord k => LR1.Map.T k v -> k -> v
Map m ! k = m Impl.! k

-- | `Impl.member`.
--
member :: Ord k => k -> LR1.Map.T k v -> Bool
member k (Map m) = Impl.member k m

-- | `Impl.adjust`.
--
adjust :: Ord k => (v -> v) -> k -> LR1.Map.T k v -> LR1.Map.T k v
adjust f k (Map m) = Map (Impl.adjust f k m)
