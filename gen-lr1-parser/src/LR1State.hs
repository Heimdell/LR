module LR1State where

import Data.Function                  ((&), on)

-- import Position        (groupPositionsByPrefices)
import Data.Set      (Set)

import Data.Set qualified as Set

import Data.Map.Monoidal (type (==>), (==>))
import Grammar
import LR1Item
import Rule
import Symbol
import LR0Item
import GHC.Generics
import Pretty
import Data.Foldable
import Data.Maybe (listToMaybe)
import Control.Arrow ((>>>))
import Data.Ord (comparing)


{- |
  Set of positions to build a parser state from.
-}
newtype Kernel = Kernel { items :: Set LR1Item }
  deriving newtype (Eq, Ord, Semigroup, Monoid, Pretty)

mapKernel :: (LR1Item -> LR1Item) -> Kernel -> Kernel
mapKernel f (Kernel items) = Kernel (Set.map f items)

startingKernel :: CachedGrammar -> Kernel
startingKernel cachedGrammar = startEntityRules cachedGrammar S [LookForEOF]

{- |
  For a set of starting positions for all rules for given `Entity` and
  for all lookaheads.

  >>> startEntityRules arithCachedGrammar "E" ["+", LookForEOF]

  > fromList
  >   [ [E = • E + F, ⊥]
  >   , [E = • E + F, +]
  >   , [E = • F,     ⊥]
  >   , [E = • F,     +]
  >   ]
-}
startEntityRules :: CachedGrammar -> Entity -> Set Lookahead -> Kernel
startEntityRules cache entity lookaheads =
  lookaheads            & foldMap \lookahead ->
  rulesFor cache entity & foldMap \rule ->
    Kernel [startLR1 rule lookahead]

{- |
  Parsing state.

  Is built from kernel via `closure`.
-}
data LR1State = LR1State
  { kernel   :: Kernel                     -- ^ initial set of simultaneous rules
  , items    :: Set LR1Item                -- ^ full set of simultaneous rules
  , lr0state :: Set LR0Item                -- ^ for PGM-compression
  }
  deriving stock (Generic)
  deriving (Show) via PP LR1State
  deriving (Semigroup, Monoid) via Generically LR1State

longestPrefix :: LR1State -> [Symbol]
longestPrefix state = maximumBy (comparing length) (Set.map parsedSymbols state.items)


{- |
  Kernel defines the `LR1State`.
-}
instance Eq LR1State where (==) = (==) `on` (.kernel)

{- |
  Kernel defines the `LR1State`.
-}
instance Ord LR1State where compare = compare `on` (.kernel)

instance Pretty LR1State where
  pPrint = pPrint . (.items)

instance Pretty a => Pretty (Set a) where
  pPrint st = vcat $ map pPrint (toList st)

{-|
  Build full state from some set of positions.

  Example:

  > 0 [S ← • E, ⊥]

  We cannot track progress with it - @E@ can have unlimited length. Lets add all
  its starting positions with appropriate lookaheads:

  > 0 [S ← • E,     ⊥]
  > 1 [E ← • E + F, ⊥] from 0
  > 2 [E ← • F,     ⊥] from 0

  Now we have @F@ and @E@ (with different lookahead @"+"@ this time). Still, we
  need to continue adding entities, until no unfolded entity remain.

  > [⊤ = • E,     ⊥]
  > [E = • E + F, ⊥]
  > [E = • E + F, +]
  > [E = • F,     ⊥]
  > [E = • F,     +]
  > [F = • F * T, ⊥]
  > [F = • F * T, *]
  > [F = • F * T, +]
  > [F = • T,     ⊥]
  > [F = • T,     *]
  > [F = • T,     +]
  > [T = • ( E ), ⊥]
  > [T = • ( E ), *]
  > [T = • ( E ), +]
  > [T = • n,     ⊥]
  > [T = • n,     *]
  > [T = • n,     +]

  That's quite a lot. We will use all of this in `transitionsFrom`.
-}
closure :: CachedGrammar -> Kernel -> LR1State
closure cache kernel = LR1State
  { kernel
  , items
  , lr0state = fold perTerm
  }
  where
    perTerm :: Lookahead ==> Set LR0Item
    perTerm = items & foldMap \item -> case item.lr0item of
      Reduces _ -> item.lookahead ==> [item.lr0item]
      Shifts shifting -> leadingTerminalsOf cache shifting.locus & foldMap \term ->
        term ==> [item.lr0item]

    items :: Set LR1Item
    items = go Set.empty (Set.toList kernel.items)
      where
        go :: Set LR1Item -> [LR1Item] -> Set LR1Item
        go visited = \case
          []          -> visited
          top : stack -> do
            let visited' = Set.insert top visited
            case top.lr0item of
              Shifts Shifting {locus = _ :@ NonTerm entity, ahead} | Set.notMember top visited -> do
                let lookaheads = maybe [top.lookahead] (leadingTerminalsOf cache) (listToMaybe ahead)
                let addedItems = startEntityRules cache (Named entity) lookaheads
                go visited' (toList addedItems.items ++ stack)

              _ -> go visited' stack

data SortedState = SortedState
  { expectTerminal    :: Terminal    ==> Set ShiftingLR1Item
  , expectNonTerminal :: NonTerminal ==> Set ShiftingLR1Item
  , canReduce         :: Lookahead   ==> Set Rule
  }

instance Pretty SortedState where
  pPrint SortedState {expectTerminal, expectNonTerminal, canReduce} = vcat
    [ "on non terminals"
    , nest 2 showGotos
    , "on terminals"
    , nest 2 showShifts
    , "can reduce"
    , nest 2 showReduces
    ]
    where
      showReduces, showShifts, showGotos :: Doc
      showReduces = pPrint canReduce
      showShifts  = pPrint expectTerminal
      showGotos   = pPrint expectNonTerminal

{- |
  Split state onto
  - items, expecting terminal;
  - items, expecting non-terminal;
  - items, able to be reduced.
-}
sortState :: LR1State -> SortedState
sortState LR1State {items} = SortedState
  { expectTerminal    = foldMap oneShift  items
  , expectNonTerminal = foldMap oneGoto   items
  , canReduce         = foldMap oneReduce items
  }
  where
    oneReduce :: LR1Item -> Lookahead ==> Set Rule
    oneReduce = splitLR1Item >>> \case
      Right lr1 -> lr1.lookahead ==> [lr1.rule]
      _         -> mempty

    oneGoto :: LR1Item -> NonTerminal ==> Set ShiftingLR1Item
    oneGoto = splitLR1Item >>> \case
      Left shifting -> case shifting.shifting.locus.symbol of
        Term    _      -> mempty
        NonTerm entity -> entity ==> [shifting]
      _ -> mempty

    oneShift :: LR1Item -> Terminal ==> Set ShiftingLR1Item
    oneShift = splitLR1Item >>> \case
      Left shifting -> case shifting.shifting.locus.symbol of
        Term    term -> term ==> [shifting]
        NonTerm _    -> mempty
      _ -> mempty
