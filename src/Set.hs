
-- | A wrapper for `Impl.Set`, so I can write proper `Pretty` instance.
--
--   I might probably not write it and make just the instance itself, but eh.
--
module Set where

import Data.Coerce (coerce)
import Data.Set qualified as Impl
import Data.Monoid
import Pretty

-- | Wrapper type.
newtype Set a = Set { unSet :: Impl.Set a }
  deriving newtype (Eq, Ord, Semigroup, Monoid, Foldable)
  deriving Show via PP (Set a)

-- | `Impl.singleton`.
--
ofOne :: Ord k => k -> Set k
ofOne = Set . Impl.singleton

-- | `Impl.fromList`.
--
fromList :: Ord a => [a] -> Set a
fromList = Set . Impl.fromList

-- | `Impl.toList`.
--
toList :: Set a -> [a]
toList (Set impl) = Impl.toList impl

-- | A `>>=` operator for `Set`s.
--
bind :: Ord b => Set a -> (a -> Set b) -> Set b
bind sa asb = Set . Impl.unions . Prelude.map (unSet . asb) $ toList sa

-- | `Impl.member`.
--
member :: Ord a => a -> Set a -> Bool
member a = Impl.member a . unSet

-- | `Impl.map`.
--
map :: Ord b => (a -> b) -> Set a -> Set b
map f (Set sa) = Set $ Impl.map f sa

-- | `Impl.unions`.
--
unions :: Ord a => [Set a] -> Set a
unions = Set . Impl.unions @[] . coerce

-- | Implementation of `Data.Foldable.any` for `Set`s.
--
any :: Ord a => (a -> Bool) -> Set a -> Bool
any p = getAny . foldMap (Any . p)

-- | `Impl.difference`.
--
diff :: Ord a => Set a -> Set a -> Set a
diff (Set a) (Set b) = Set $ Impl.difference a b

-- | `Impl.null`.
--
null :: Set a -> Bool
null (Set s) = Impl.null s

instance Pretty a => Pretty (Set a) where
  pretty sa = "{" `indent` vcat (Prelude.map pretty (reverse (toList sa))) `above` "}"

-- | A wrapper, making `Pretty` instance produce one-liner.
newtype ShortSet a = ShortSet (Set a)

instance Pretty a => Pretty (ShortSet a) where
  pretty (ShortSet sa) = "{" <.> fsep (Prelude.map pretty (reverse (toList sa))) <.> "}"
