
module Map where

import qualified Data.Map as Impl

import Set qualified
import Set (Set)
import Pretty

newtype Map k v = Map { unMap :: Impl.Map k v }
  deriving stock   (Eq, Ord, Functor, Foldable, Traversable)
  deriving Show via PP (Map k v)

instance (Ord k, Monoid v) => Monoid (Map k v) where
  mempty = Map mempty

instance (Ord k, Semigroup v) => Semigroup (Map k v) where
  Map l <> Map r = Map $ Impl.unionWith (<>) l r

empty :: Map k v
empty = Map Impl.empty

fromList :: Ord k => [(k, v)] -> Map k v
fromList = Map . Impl.fromList

(==>) :: k -> v -> Map k v
(==>) = singleton

singleton :: k -> v -> Map k v
singleton = (Map .) . Impl.singleton

lookup :: Ord k => k -> Map k v -> Maybe v
lookup k (Map m) = Impl.lookup k m

toList :: Map k v -> [(k, v)]
toList (Map m) = Impl.toList m

map :: (a -> b) -> Map k a -> Map k b
map f (Map m) = Map $ Impl.map f m

insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v = Map . Impl.insert k v . unMap

toMap :: Map k v -> Impl.Map k v
toMap (Map m) = m

fromMap :: Impl.Map k v -> Map k v
fromMap = Map

keySet :: Map k v -> Set k
keySet = Set.Set . Impl.keysSet . unMap

values :: Map k v -> [v]
values = Impl.elems . unMap

foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey f z (Map m) = Impl.foldrWithKey f z m

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
