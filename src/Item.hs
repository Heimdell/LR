
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

{- | An LR(0) production that has been started.
-}
data Item term result = Item
  { iName      ::  Name               -- ^ entity name
  , iBefore    :: [Point term]        -- ^ already parsed points
  , iAfter     :: [Point term]        -- ^ points to be parsed
  , iReduce    :: [result] -> result  -- ^ "semantic action"
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

{- | An LR(1) production (with lookahead) that has been started.
-}
data Item1 term result = Item1
  { i1Item      :: Item term result -- ^ an LR(0) production
  , i1Lookahead :: Set term         -- ^ terms expected after its reduction
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

-- | A type for parser state (a closure of LR(1) productions).
--
type State term result = Set (Item1 term result)

{- | A toll for nominativity of haskell type system
     (a name of LR(1) production).
-}
i1Name :: Item1 term result -> Name
i1Name = iName . i1Item

{- | A toll for nominativity of haskell type system
     (a semantic action of LR(1) production).
-}
i1Reduce :: Item1 term result -> [result] -> result
i1Reduce = iReduce . i1Item

{- | Check if production has no mached parts yet.
-}
isStartingItem :: Item1 term result -> Bool
isStartingItem = null . iBefore . i1Item

{- | Size of production in points (terminals + non-terminals).
-}
ruleLength :: Item1 term result -> Int
ruleLength (i1Item -> item) = length (iBefore item <> iAfter item)

{- | "Shift" a production.
-}
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

{- | All possible shifts of a production with given lookahead.
-}
explode :: Rule term result -> Set term -> NonEmpty (Item1 term result)
explode (Rule name points reduce) lookeahead =
  explode' (Item1 (Item name [] points reduce) lookeahead)
  where
    explode' :: Item1 term result -> NonEmpty (Item1 term result)
    explode' item = maybe (item :| []) (NonEmpty.cons item . explode') (next item)

{- | Current terminal or non-terminal to be parsed.
-}
locus :: Item1 term result -> Maybe (Point term)
locus = listToMaybe . iAfter . i1Item

{- | Firts production of a rule with given lookahead.
-}
getFirstState :: Rule term result -> Set term -> Item1 term result
getFirstState = (NonEmpty.head .) . explode

{- | Check ig production is for `Start` non-terminal.
-}
isStart :: Ord term => State term result -> Bool
isStart = Set.any \item -> i1Name item == Start
