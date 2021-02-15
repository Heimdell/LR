
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

data Item term result = Item
  { iName      ::  Name
  , iBefore    :: [Point term]
  , iAfter     :: [Point term]
  , iReduce    :: [result] -> result
  }
  deriving Show via PP (Item term result)

instance Eq term => Eq (Item term result) where
  (==) = (==) `on` (iName &&& iBefore &&& iAfter)

instance Ord term => Ord (Item term result) where
  compare = compare `on`
    (length . iBefore &&& iName &&& reverse . iBefore &&& iAfter)

instance Pretty term => Pretty (Item term result) where
  pretty (Item m (reverse -> before) after _) =
    pretty m
      <+> "->" <+> fsep (map pretty before)
      <+> "."  <+> fsep (map pretty after)

data Item1 term result = Item1
  { i1Item      :: Item term result
  , i1Lookahead :: Set term
  }
  deriving Show via PP (Item1 term result)

instance Eq term => Eq (Item1 term result) where
  (==) = (==) `on` (i1Item &&& i1Lookahead)
instance Ord term => Ord (Item1 term result) where
  compare = compare `on` (i1Item &&& i1Lookahead)

instance Pretty term => Pretty (Item1 term result) where
  pretty (Item1 item l) =
    pretty item
      <.> "," <+> pretty (Set.ShortSet l)

i1Name   :: Item1 term result -> Name
i1Reduce :: Item1 term result -> [result] -> result
i1Name   = iName   . i1Item
i1Reduce = iReduce . i1Item

ruleLength :: Item1 term result -> Int
ruleLength (i1Item -> item) = length (iBefore item <> iAfter item)

next :: Item1 term result -> Maybe (Item1 term result)
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

explode :: Rule term result -> Set term -> NonEmpty (Item1 term result)
explode (Rule name points reduce) lookeahead =
  explode' (Item1 (Item name [] points reduce) lookeahead)
  where
    explode' :: Item1 term result -> NonEmpty (Item1 term result)
    explode' item = maybe (item :| []) (NonEmpty.cons item . explode') (next item)

locus :: Item1 term result -> Maybe (Point term)
locus = listToMaybe . iAfter . i1Item

getFirstState :: Rule term result -> Set term -> Item1 term result
getFirstState = (NonEmpty.head .) . explode

isStart :: Ord term => Set (Item1 term result) -> Bool
isStart = Set.any \item -> i1Name item == Start
