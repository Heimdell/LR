
{- | A production with * (cursor point) inside of it.
-}
module Item where

import Control.Arrow ((&&&))

import Data.Function      (on)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe         (listToMaybe)

import Set (Set)
import Set qualified as Set

import Name
import Point
import Pretty
import Rule
import Term

{- | An LR(0) production that has been started.
-}
data Item term = Item
  { iName      ::  Name               -- ^ entity name
  , iBefore    :: [Point term]        -- ^ already parsed points
  , iAfter     :: [Point term]        -- ^ points to be parsed
  , iReduce    :: Name                -- ^ semantic "action"
  }
  deriving Show via PP (Item term)

instance Eq term => Eq (Item term) where
  (==) = (==) `on` (iName &&& iBefore &&& iAfter)

instance Ord term => Ord (Item term) where
  compare = compare `on`
    (length . iBefore &&& iName &&& reverse . iBefore &&& iAfter)

instance Pretty term => Pretty (Item term) where
  pretty (Item m (reverse -> before) after _) =
    pretty m
      <+> "->" <+> fsep (map pretty before)
      <+> "."  <+> fsep (map pretty after)

{- | An LR(1) production (with lookahead) that has been started.
-}
data Item1 term = Item1
  { i1Item      :: Item term  -- ^ an LR(0) production
  , i1Lookahead :: Set term   -- ^ terms expected after its reduction
  }
  deriving stock (Eq, Ord)
  deriving Show via PP (Item1 term)

instance Pretty term => Pretty (Item1 term) where
  pretty (Item1 item l) =
    pretty item
      <.> "," <+> pretty (Set.ShortSet l)

-- | A type for parser state (a closure of LR(1) productions).
--
type State term = Set (Item1 term)

{- | A toll for nominativity of haskell type system
     (a name of LR(1) production).
-}
i1Name :: Item1 term -> Name
i1Name = iName . i1Item

{- | A toll for nominativity of haskell type system
     (a semantic action of LR(1) production).
-}
i1Reduce :: Item1 term -> Name
i1Reduce = iReduce . i1Item

{- | Check if production has no mached parts yet.
-}
isStartingItem :: Item1 term -> Bool
isStartingItem = null . iBefore . i1Item

{- | Size of production in points (terminals + non-terminals).
-}
ruleLength :: Item1 term -> Int
ruleLength (i1Item -> item) = length (iBefore item <> iAfter item)

{- | "Shift" a production.
-}
next :: Item1 term -> Maybe (Item1 term)
next
    item1@Item1
      { i1Item = item@Item
        { iBefore, iAfter } }
  = case iAfter of
    []           -> Nothing
    point : rest -> Just item1
      { i1Item = item
        { iBefore = point : iBefore
        , iAfter = rest
        }
      }

{- | All possible shifts of a production with given lookahead.
-}
explode :: Rule term -> Set (Term term) -> NonEmpty (Item1 (Term term))
explode (Rule name points reduce) lookeahead =
  explode' (Item1 (Item name [] ((map.fmap) Next points) reduce) lookeahead)
  where
    explode' :: Item1 (Term term) -> NonEmpty (Item1 (Term term))
    explode' item = maybe (item :| []) (NonEmpty.cons item . explode') (next item)

{- | Current terminal or non-terminal to be parsed.
-}
locus :: Item1 term -> Maybe (Point term)
locus = listToMaybe . iAfter . i1Item

{- | Firts production of a rule with given lookahead.
-}
getFirstState :: Rule term -> Set (Term term) -> Item1 (Term term)
getFirstState = (NonEmpty.head .) . explode

{- | Check ig production is for `Start` non-terminal.
-}
isStart :: Ord term => State term -> Bool
isStart = Set.any \item -> i1Name item == Start
