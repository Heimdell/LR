module LR1.Item where

import LR1.NonTerm qualified as NonTerm
import Data.Text (Text)
import Data.Set (Set)
import LR1.Term qualified as Term
import LR1.Position qualified as Position
import qualified Data.Text as Text
import Data.Function ((&))
import Data.Foldable (toList)
import qualified LR1.Rule as Rule
import qualified LR1.Point as Point

data T = Item
  { entity    :: NonTerm.T
  , pos       :: Position.T
  , lookahead :: Set Term.T
  , label     :: Text
  }
  deriving stock (Eq, Ord)

instance Show LR1.Item.T where
  show Item {entity, pos, lookahead, label} =
      show entity
      <> " = " <> show pos
      <> "    {" <> (lookahead & toList & fmap show & unwords)
      <> "}    {" <> Text.unpack label <> "}"

start :: Set Term.T -> Rule.T -> LR1.Item.T
start lookahead rule@Rule.Rule {entity, label} = Item
  { entity
  , pos = Position.start rule
  , label
  , lookahead
  }

next :: LR1.Item.T -> Maybe LR1.Item.T
next item = do
  pos1 <- pos item & Position.next
  return item { pos = pos1 }

locus :: LR1.Item.T -> Maybe Point.T
locus = Position.locus . pos

len :: LR1.Item.T -> Int
len = Position.len . pos

uncons :: LR1.Item.T -> Maybe (Point.T, LR1.Item.T)
uncons item = do
  loc <- locus item
  nex <- next item
  return (loc, nex)
