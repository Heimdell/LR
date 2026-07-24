module LR1Item where

import Rule
import Symbol
import Data.Foldable

import LR0Item
import Pretty
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

{- |
  Position with `Lookahead`.

  > [F ← F • '*' T, '+']
  >        ~ ~~~    ~~~
  >        ^  ^      ^
  >        |  |      +-- lookahead
  >        |  +--------- locus
  >        +------------ current parsing position

-}
data LR1Item = LR1Item
  { lr0item   :: LR0Item
  , lookahead :: Lookahead
  }
  deriving stock (Eq, Ord)
  deriving (Show) via PP LR1Item

parsedSymbols :: LR1Item -> [Symbol]
parsedSymbols lr1 = (.symbol) <$> case lr1.lr0item of
  Shifts  shifting -> shifting.parsed
  Reduces rule     -> toList rule.symbols

instance Pretty LR1Item where
  pPrint lr1 = "[" <.> pPrint lr1.lr0item <.> "," <+> pPrint lr1.lookahead <.> "]"

data ShiftingLR1Item = ShiftingLR1Item
  { shifting  :: Shifting
  , lookahead :: Lookahead
  }
  deriving stock (Eq, Ord)
  deriving (Show) via PP ShiftingLR1Item

instance Pretty ShiftingLR1Item where
  pPrint lr1 = "[" <.> pPrint lr1.shifting <.> "," <+> pPrint lr1.lookahead <.> "]"

data ReducingLR1Item = ReducingLR1Item
  { rule      :: Rule
  , lookahead :: Lookahead
  }
  deriving stock (Eq, Ord)
  deriving (Show) via PP ReducingLR1Item

instance Pretty ReducingLR1Item where
  pPrint lr1 = "[" <.> pPrint lr1.rule <.> "," <+> pPrint lr1.lookahead <.> "]"

splitLR1Item :: LR1Item -> Either ShiftingLR1Item ReducingLR1Item
splitLR1Item lr1@LR1Item{lookahead} = case lr1.lr0item of
  Shifts  shifting -> Left  ShiftingLR1Item {shifting, lookahead}
  Reduces rule     -> Right ReducingLR1Item {rule,     lookahead}

{- |
  Start parsing a rule, expect given exact terminal after it finishes.
-}
startLR1 :: Rule -> Lookahead -> LR1Item
startLR1 rule lookahead = LR1Item
  { lr0item = startLR0 rule
  , lookahead
  }

{- |
  Advance an shifting item to its next position.
-}
next :: ShiftingLR1Item -> LR1Item
next lr1@ShiftingLR1Item{shifting = Shifting {entity, parsed, locus, ahead, reduce}} = case ahead of
  []              -> item $ Reduces Rule    {entity, symbols = parsed `snoc` locus,                                  reduce}
  locus' : ahead' -> item $ Shifts  Shifting{entity, parsed  = parsed   ++  [locus], locus = locus', ahead = ahead', reduce}
  where
    item :: LR0Item -> LR1Item
    item lr0item = LR1Item {lr0item, lookahead = lr1.lookahead}

snoc :: [a] -> a -> NonEmpty a
snoc xs x = foldr (NonEmpty.<|) [x] xs
