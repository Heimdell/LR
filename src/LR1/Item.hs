
{- |
  An LR(1) Item.
-}
module LR1.Item where

import Data.Foldable (toList)
import Data.Function ((&))
import Data.Set (Set)

import LR1.Func     qualified as Func
import LR1.NonTerm  qualified as NonTerm
import LR1.Point    qualified as Point
import LR1.Position qualified as Position
import LR1.Rule     qualified as Rule
import LR1.Term     qualified as Term

{- |
  An LR(1) Item, as described in the Dragonbook.
-}
data T = Item
  { entity    :: NonTerm.T   -- ^ Nonterminal it reduces into.
  , pos       :: Position.T  -- ^ Rule points and current position.
  , lookahead :: Set Term.T  -- ^ The terminals expected after this rule is done.
  , label     :: Func.T      -- ^ The reducing function.
  }
  deriving stock (Eq, Ord)

instance Show LR1.Item.T where
  show Item {entity, pos, lookahead, label} =
      show entity
      <> " = " <> show pos
      <> "    {" <> (lookahead & toList & fmap show & unwords)
      <> "}    {" <> show label <> "}"

{- |
  Turn a `Rule.T` into and item in the starting position with given lookahead set.
-}
start :: Set Term.T -> Rule.T -> LR1.Item.T
start lookahead rule@Rule.Rule {entity, label} = Item
  { entity
  , pos = Position.start rule
  , label
  , lookahead
  }

{- |
  Get an item in the next position - if there is one.
-}
next :: LR1.Item.T -> Maybe LR1.Item.T
next item = do
  pos1 <- pos item & Position.next
  return item { pos = pos1 }

{- |
  Get current point of the rule - unless it is finished and ready to be reduced.
-}
locus :: LR1.Item.T -> Maybe Point.T
locus = Position.locus . pos

{- |
  Get how many items should be popped from stack to use in this item reduction.
-}
len :: LR1.Item.T -> Int
len = Position.len . pos

{- |
  Get current `locus` /and/ `next` item at once.
-}
locusAndNext :: LR1.Item.T -> Maybe (Point.T, LR1.Item.T)
locusAndNext item = do
  loc <- locus item
  nex <- next item
  return (loc, nex)
