module LR1Item where

import Data.Function                  ((&))
import Data.Set                       (Set)
import GHC.Records                    (HasField(..))
import Text.PrettyPrint.HughesPJClass (Pretty, pPrint, (<+>), fsep, braces)

import Data.Set qualified as Set

import Data.Map.Monoidal  ((==>))
import Rule

import Data.Map.Monoidal qualified as Map

import GHC.Generics      (Generically(..), Generic)

import Data.Array qualified as Array

import Data.Map.Monoidal (type (==>), (!))
import Control.Fixpoint          ((>>-))
import Grammar           (Grammar(first, Grammar))
import Symbol
import Data.Foldable
import Data.Text (Text)

import LR0Item

{- |
  LR1Item in a rule during parsing process.
-}
data LR1Item = LR1Item
  { lookahead :: Lookahead     -- ^ term expected right after rule is parsed
  , lr0item   :: LR0Item
  }
  deriving stock (Eq, Ord)

instance HasField "clause" LR1Item Clause where getField = (.lr0item.clause)
instance HasField "offset" LR1Item Int where getField = (.lr0item.offset)
instance HasField "entity" LR1Item NonTerminal where getField = (.lr0item.entity)
instance HasField "type_" LR1Item (Maybe Text) where getField = (.lr0item.type_)

{- |
  > E = E + T
  >       ^
  Current point of the position.
-}
instance HasField "locus" LR1Item (Maybe Symbol) where
  getField lr1item =
    if lr1item.offset >= length lr1item.clause.points
    then Nothing
    else Just (lr1item.clause.points Array.! lr1item.offset)

instance HasField "reducer" LR1Item Text where
  getField = (.clause.reducer)

{- |
  > E = E + T
  >       ^

  > E = E + T
  >         ^

  Next position.
-}
instance HasField "next" LR1Item (Maybe LR1Item) where
  getField lr1item =
    if lr1item.offset >= length lr1item.clause.points
    then Nothing
    else Just (lr1item :: LR1Item)
      { lr0item = lr1item.lr0item
        { offset = lr1item.offset + 1
        }
      }

instance HasField "parsed" LR1Item [Symbol] where
  getField lr1item = take lr1item.offset $ toList lr1item.clause.points

{- |
  Start parsing a rule, expecting given `lookahead` term.
-}
startRule :: NonTerminal -> Maybe Text -> Clause -> Lookahead -> LR1Item
startRule entity type_ clause lookahead = LR1Item
  { lookahead
  , lr0item = LR0Item
    { offset = 0
    , clause
    , entity
    , type_
    }
  }

{- |
  Terms, expected after current point is parsed.

  Either FIRST(pos.next) or the lookahead of the position.

  > E = E + T {)}
  >       ^
  > lacp = FIRST(T)

  > E = E + T {)}
  >         ^
  > lacp = {)}
-}
lookaheadAfterCurrentPoint :: Grammar -> LR1Item -> Set Lookahead
lookaheadAfterCurrentPoint Grammar {first} pos = case pos.next >>= (.locus) of
  Nothing           -> Set.singleton pos.lookahead
  Just (T _ term)   -> Set.singleton (LookForTerm term)
  Just (E _ entity) -> Set.map LookForTerm (first ! entity)

{- |
  Group a set of position by current point to be parsed.
-}
groupPositionsByCurrentPoints :: Set LR1Item -> Symbol ==> Set LR1Item
groupPositionsByCurrentPoints positions =
  positions >>- \pos ->
  pos.locus >>- \point ->
    point ==> Set.singleton pos

data SortedPositions = SortedPositions
  { expectsNonTerminal :: NonTerminal ==> Set LR1Item
  , expectsTerminal    :: Lookahead   ==> Set LR1Item
  , needsReduction     ::                 Set LR1Item
  }
  deriving stock (Generic)
  deriving       (Semigroup, Monoid) via Generically SortedPositions

splitPositionsByCategory :: Set LR1Item -> SortedPositions
splitPositionsByCategory = foldMap \pos -> do
  case pos.locus of
    Nothing           -> mempty { needsReduction     =                        Set.singleton pos }
    Just (T _ term)   -> mempty { expectsTerminal    = LookForTerm term   ==> Set.singleton pos }
    Just (E _ entity) -> mempty { expectsNonTerminal =             entity ==> Set.singleton pos }
      :: SortedPositions

-------------------------------------------------------------------------------

instance HasField "prefix" LR1Item (Clause, NonTerminal, Int) where
  getField pos = (pos.clause, pos.entity, pos.offset)

groupPositionsByPrefices :: Set LR1Item -> Set PrettyPosition
groupPositionsByPrefices set =
  foldMap (\a -> a.prefix ==> Set.singleton a.lookahead) set
    & Map.foldMapWithKey \(e, c, f) ts -> Set.singleton $ PrettyPosition e f c ts

data PrettyPosition = PrettyPosition
  { clause    :: Clause
  , offset    :: Int
  , entity    :: NonTerminal
  , lookahead :: Set Lookahead
  }
  deriving stock (Eq, Ord)

instance Pretty PrettyPosition where
  pPrint PrettyPosition {clause, entity, offset, lookahead} =
    pPrint entity <+> "=" <+> fsep (map pPrint front) <+> (case rest of
        [] -> "reducing on"
        locus : other ->
          ("[" <> pPrint locus <> "]")
            <+> fsep (map pPrint other)
      ) <+> braces (fsep (map pPrint (toList lookahead)))
    where
      (front, rest) = splitAt offset (toList clause.points)

instance Pretty LR1Item where
  pPrint lr1item =
    pPrint lr1item.entity <+> "=" <+> fsep (map pPrint front) <+> (case rest of
        [] -> "reducing on"
        locus : other ->
          ("[" <> pPrint locus <> "]")
            <+> fsep (map pPrint other)
      ) <+> braces (pPrint lr1item.lookahead)
    where
      (front, rest) = splitAt lr1item.offset (toList lr1item.clause.points)
