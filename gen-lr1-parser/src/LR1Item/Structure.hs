{- |
  Internal state of parsing a rule.
-}
module LR1Item.Structure where

import Data.Set          (Set)
import GHC.Generics      (Generically(..), Generic)
import GHC.Records       (HasField(..))

import Data.Array qualified as Array
import Data.Set   qualified as Set

import Data.Map.Monoidal (type (==>), (!), (==>))
import Fixpoint          ((>>-))
import Grammar           (Grammar(first, Grammar))
import Rule
import Symbol
import Data.Foldable
import Data.Text (Text)

{- |
  LR1Item in a rule during parsing process.
-}
data LR1Item = LR1Item
  { lookahead :: Lookahead     -- ^ term expected right after rule is parsed
  , offset    :: Int      -- ^ mark of the rule
  , clause    :: Clause   -- ^ reference to the rule
  , type_     :: Maybe Text
  , entity    :: Entity
  }
  deriving stock (Eq, Ord)

{- |
  > E = E + T
  >       ^
  Current point of the position.
-}
instance HasField "locus" LR1Item (Maybe Symbol) where
  getField LR1Item {clause, offset} =
    if offset >= length clause.points
    then Nothing
    else Just (clause.points Array.! offset)

instance HasField "reducer" LR1Item Text where
  getField LR1Item {clause} = clause.reducer

{- |
  > E = E + T
  >       ^

  > E = E + T
  >         ^

  Next position.
-}
instance HasField "next" LR1Item (Maybe LR1Item) where
  getField pos@LR1Item{clause, offset} =
    if offset >= length clause.points
    then Nothing
    else Just (pos :: LR1Item)
      { offset = offset + 1
      }

instance HasField "parsed" LR1Item [Symbol] where
  getField LR1Item {offset, clause} = take offset $ toList clause.points

{- |
  Start parsing a rule, expecting given `lookahead` term.
-}
startRule :: Entity -> Maybe Text -> Clause -> Lookahead -> LR1Item
startRule entity type_ clause lookahead = LR1Item
  { offset = 0
  , lookahead
  , clause
  , entity
  , type_
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
  { expectsEntity   :: Entity    ==> Set LR1Item
  , expectsTerminal :: Lookahead ==> Set LR1Item
  , needsReduction  ::               Set LR1Item
  }
  deriving stock (Generic)
  deriving       (Semigroup, Monoid) via Generically SortedPositions

splitPositionsByCategory :: Set LR1Item -> SortedPositions
splitPositionsByCategory = foldMap \pos -> do
  case pos.locus of
    Nothing           -> mempty { needsReduction  =                        Set.singleton pos }
    Just (T _ term)   -> mempty { expectsTerminal = LookForTerm term   ==> Set.singleton pos }
    Just (E _ entity) -> mempty { expectsEntity   =             entity ==> Set.singleton pos }
      :: SortedPositions

-- examplePosSet :: Set LR1Item
-- examplePosSet = Set.fromList
--   [ fromJust $ (mkRule "T" [T "(", E "E", T ")"] "" `start` "$").next
--   ,             mkRule "E" [E "E", T "+", E "F"] "" `start` "+"
--   ]
