module Data.Map.Monoidal.Structure where

import Data.Coerce (coerce)

import Data.Map qualified as Map

infixr 1 ==>

newtype k ==> v = Monoidal { monoidal :: Map.Map k v }
  deriving newtype (Eq, Ord, Functor, Foldable)

empty :: forall k v. k ==> v
empty = coerce (Map.empty @k @v)

insert :: (Ord k, Semigroup v) => k -> v -> k ==> v -> k ==> v
insert k v = coerce $ Map.insertWith (<>) k v

infixl 4 !

(!) :: (Ord k, Monoid v) => k ==> v -> k -> v
Monoidal m ! k = Map.findWithDefault mempty k m

fromList :: forall k v. (Ord k, Semigroup v) => [(k, v)] -> k ==> v
fromList = coerce $ Map.fromListWith @k @v (<>)

(==>) :: forall k v. k -> v -> k ==> v
(==>) = coerce (Map.singleton @k @v)

instance (Ord k, Semigroup v) => Semigroup (k ==> v) where
  Monoidal a <> Monoidal b = Monoidal $ Map.unionWith (<>) a b

instance (Ord k, Semigroup v) => Monoid (k ==> v) where
  mempty = empty

assocs :: forall k v. k ==> v -> [(k, v)]
assocs = coerce (Map.assocs @k @v)

foldMapWithKey :: forall m k v. Monoid m => (k -> v -> m) -> k ==> v -> m
foldMapWithKey = coerce (Map.foldMapWithKey @m @k @v)

mapWithKey :: forall k v v'. Monoid v' => (k -> v -> v') -> k ==> v -> k ==> v'
mapWithKey = coerce (Map.mapWithKey @k @v @v')

selectKeys :: (Ord k', Semigroup v) => (k -> Maybe k') -> k ==> v -> k' ==> v
selectKeys choose = foldMapWithKey \k v -> foldMap (==> v) (choose k)

member :: forall k v. (Ord k) => k -> k ==> v -> Bool
member = coerce (Map.member @k @v)

keys :: forall k v. (Ord k) => k ==> v -> [k]
keys = coerce (Map.keys @k @v)
