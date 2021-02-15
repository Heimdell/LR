
module Set where

import Data.Set qualified as Impl
import Data.Monoid
import Pretty

newtype Set a = Set { unSet :: Impl.Set a }
  deriving newtype (Eq, Ord, Semigroup, Monoid, Foldable)
  deriving Show via PP (Set a)

ofOne :: Ord k => k -> Set k
ofOne = Set . Impl.singleton

fromList :: Ord a => [a] -> Set a
fromList = Set . Impl.fromList

toList :: Set a -> [a]
toList (Set impl) = Impl.toList impl

bind :: Ord b => Set a -> (a -> Set b) -> Set b
bind sa asb = Set . Impl.unions . Prelude.map (unSet . asb) $ toList sa

member :: Ord a => a -> Set a -> Bool
member a = Impl.member a . unSet

map :: Ord b => (a -> b) -> Set a -> Set b
map f (Set sa) = Set $ Impl.map f sa

unions :: Ord a => [Set a] -> Set a
unions = Set . Impl.unions . Prelude.map unSet

any :: Ord a => (a -> Bool) -> Set a -> Bool
any p = getAny . foldMap (Any . p)

diff :: Ord a => Set a -> Set a -> Set a
diff (Set a) (Set b) = Set $ Impl.difference a b

null :: Set a -> Bool
null (Set s) = Impl.null s

instance Pretty a => Pretty (Set a) where
  pretty sa = "{" `indent` vcat (Prelude.map pretty (reverse (toList sa))) `above` "}"

newtype ShortSet a = ShortSet (Set a)

instance Pretty a => Pretty (ShortSet a) where
  pretty (ShortSet sa) = "{" <.> fsep (Prelude.map pretty (reverse (toList sa))) <.> "}"
