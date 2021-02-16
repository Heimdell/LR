
-- | The wrapper around `Impl.Map`.
--
--   Standard haskell `Impl.Map` has `Monoid` instance that does not merge
--   values, it only merges keys - and my algorithms become so much cleaner
--   if it do.
--
module Map where

import qualified Data.Map as Impl

import Set qualified
import Set (Set)
import Pretty

-- | A wrapper.
--
newtype Map k v = Map { unMap :: Impl.Map k v }
  deriving stock   (Eq, Ord, Functor, Foldable, Traversable)
  deriving Show via PP (Map k v)

instance (Ord k, Monoid v) => Monoid (Map k v) where
  mempty = Map mempty

instance (Ord k, Semigroup v) => Semigroup (Map k v) where
  Map l <> Map r = Map $ Impl.unionWith (<>) l r

-- | `Impl.fromList`.
--
fromList :: Ord k => [(k, v)] -> Map k v
fromList = Map . Impl.fromList

-- | `Impl.singleton`.
--
(==>) :: k -> v -> Map k v
(==>) = (Map .) . Impl.singleton

-- | `Impl.lookup`.
--
lookup :: Ord k => k -> Map k v -> Maybe v
lookup k (Map m) = Impl.lookup k m

-- | `Impl.toList`.
--
toList :: Map k v -> [(k, v)]
toList (Map m) = Impl.toList m

-- | `Impl.map`.
--
map :: (a -> b) -> Map k a -> Map k b
map f (Map m) = Map $ Impl.map f m

-- | `Impl.insert`.
--
insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v = Map . Impl.insert k v . unMap

-- | Unwrap.
--
toMap :: Map k v -> Impl.Map k v
toMap (Map m) = m

-- | Wrap.
--
fromMap :: Impl.Map k v -> Map k v
fromMap = Map

-- | `Impl.keysSet` (sic!).
--
keySet :: Map k v -> Set k
keySet = Set.Set . Impl.keysSet . unMap

-- | `Impl.elems`.
--
values :: Map k v -> [v]
values = Impl.elems . unMap

-- | `Impl.foldrWithKey`.
--
foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey f z (Map m) = Impl.foldrWithKey f z m

-- | `Impl.null`.
--
null :: Map k v -> Bool
null (Map m) = Impl.null m

infixr 7 ==>

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty m =
    "{"
      `indent` (vcat (ppKV <$> Map.toList m))
      `above`  "}"
    where
      ppKV (k, v) = pretty k <.> ": " `indent` pretty v

instance (Pretty k, Pretty v) => Pretty (Impl.Map k v) where
  pretty = pretty . Map
