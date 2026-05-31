{- |
  Internal state of parsing a rule.
-}
module Position.Structure where

import Data.Foldable                         (Foldable(toList))
import Data.Maybe                            (listToMaybe, fromJust)
import Data.Map.Monoidal                     (type (==>), (!), (==>))
import Data.Set                              (Set)
import Data.Set          qualified as Set
import GHC.Records                           (HasField(..))

import Fixpoint                              ((>>-))
import Rule                                  (Rule(Rule, entity, points, mark), mkRule)
import Term                                  (Point(..), Entity, Term(Term))
import Grammar                               (Grammar(first, Grammar))
import GHC.Generics (Generically(..), Generic)

{- |
  Position in a rule during parsing process.
-}
data Position = Position
  { entity    :: Entity   -- ^ entity to be parsed
  , lookahead :: Term     -- ^ term expected right after rule is parsed
  , front     :: [Point]  -- ^ already parsed points
  , rest      :: [Point]  -- ^ yet-to be parsed points
  , mark      :: Int      -- ^ mark of the rule
  , rule      :: Rule     -- ^ reference to the rule
  }
  deriving stock (Eq, Ord)

{- |
  > E = E + T
  >       ^
  Current point of the position.
-}
instance HasField "locus" Position (Maybe Point) where
  getField Position {rest} = listToMaybe rest

{- |
  > E = E + T
  >       ^

  > E = E + T
  >         ^

  Next position.
-}
instance HasField "next" Position (Maybe Position) where
  getField pos@Position{front, rest} = case rest of
    []            -> Nothing
    point : rest' -> Just (pos :: Position)
      { front = point : front
      , rest  = rest'
      }

{- |
  Start parsing a rule, expecting given `lookahead` term.
-}
start :: Rule -> Term -> Position
start rule@Rule {entity, points, mark} lookahead = Position
  { entity
  , front = []
  , rest  = toList points
  , mark
  , lookahead
  , rule
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
lookaheadAfterCurrentPoint :: Grammar -> Position -> Set Term
lookaheadAfterCurrentPoint Grammar {first} pos = case pos.next >>= (.locus) of
  Nothing         -> Set.singleton pos.lookahead
  Just (T term)   -> Set.singleton term
  Just (E entity) -> first ! entity

{- |
  Group a set of position by current point to be parsed.
-}
groupPositionsByCurrentPoints :: Set Position -> Point ==> Set Position
groupPositionsByCurrentPoints positions =
  positions >>- \pos ->
  pos.locus >>- \point ->
    point ==> Set.singleton pos

data SortedPositions = SortedPositions
  { expectsEntity   :: Entity ==> Set Position
  , expectsTerminal :: Term   ==> Set Position
  , needsReduction  ::            Set Position
  }
  deriving stock (Generic)
  deriving       (Semigroup, Monoid) via Generically SortedPositions

splitPositionsByCategory :: Set Position -> SortedPositions
splitPositionsByCategory = foldMap \pos -> do
  case pos.locus of
    Nothing         -> mempty { needsReduction  =            Set.singleton pos }
    Just (T term)   -> mempty { expectsTerminal = term   ==> Set.singleton pos }
    Just (E entity) -> mempty { expectsEntity   = entity ==> Set.singleton pos } :: SortedPositions

{- |
  Starting position for test grammar.
-}
startingPosition :: Position
startingPosition = start (mkRule "S" [E "E"] "") (Term "$")

examplePosSet :: Set Position
examplePosSet = Set.fromList
  [ fromJust $ (mkRule "T" [T "(", E "E", T ")"] "" `start` "$").next
  ,             mkRule "E" [E "E", T "+", E "F"] "" `start` "+"
  ]
