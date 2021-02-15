
module Action where

import Data.Function (on)
import Data.List (partition, sortBy, groupBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ord (comparing)

import Map (Map, (==>))
import Map qualified as Map
import Set (Set)
import Set qualified as Set

import Item
import Goto
import Name
import Point
import Pretty
import Rule
import Util

data Action term result
  = Shift    (Set (Item1 term result))
  | Reduce   (Item1 term result)
  | Accept
  | Conflict (Action term result) (Action term result)
  | Expected (Set term)
  deriving stock (Eq)

instance Pretty term => Pretty (Action term result) where
  pretty = \case
    Shift    state      -> color 2 "Shift"    `indent` pretty state
    Reduce   result     -> color 5 "Reduce"   `indent` pretty result
    Accept              -> color 3 "Accept"
    Expected expected   -> color 7 "Expected" `indent` pretty expected
    Conflict left right -> color 1 "Conflict" `indent`
          "("  <.>     pretty left
      <.> "," `indent` pretty right
      <.> ")"

instance Ord term => Semigroup (Action term result) where
  Expected left <> Expected right = Expected (left <> right)
  Expected _    <>          right = right
  left          <> Expected _     = left
  left          <>          right
    | left  ==               right  = left
    | left `eqUpToLookahead` right  = left `mergeLookahead` right
    | otherwise                     = Conflict left right

eqUpToLookahead :: Eq term => Action term result -> Action term result -> Bool
eqUpToLookahead (Reduce left) (Reduce right) = ((==) `on` i1Item) left right
eqUpToLookahead  _             _             = False

mergeLookahead :: Ord term => Action term result -> Action term result -> Action term result
mergeLookahead (Reduce left) (Reduce right) =
  Reduce left
    { i1Lookahead = i1Lookahead left <> i1Lookahead right
    }
mergeLookahead _ _ = error "mergeLookahead: can only merge Reduce actions"

instance Ord term => Monoid (Action term result) where
  mempty = Expected mempty

type Act term result
  =  Set (Item1 term result)
  -> term
  -> Action term result

type Act' term result
  = Map (Set (Item1 term result))
  ( Map term (Action term result)
  )

getAction
  :: (Ord term, Pretty term)
  => Goto term result
  -> term
  -> Act term result
getAction goto eof from term = foldMap decide from
  where
    decide item = case locus item of
      Just (Term term')
        | term == term' ->
          Shift (goto from (Term term))

      Nothing
        | i1Lookahead item ? term ->
          if isStart from && term == eof
          then Accept
          else Reduce item

      _ -> Expected mempty

populateExpectedTokens
  :: (Ord term, Pretty term)
  => Act' term result
  -> Act' term result
populateExpectedTokens = Map.map populate
  where
    populate (Map.toList -> mapping) = do
      let (fails, successes) = partition (isFailure . snd) mapping
      let expected = Set.fromList [term | (term, _act) <- successes]
      Map.fromList $ ((Expected expected <$) <$> fails) <> successes

    isFailure = \case
      Expected {} -> True
      _           -> False
