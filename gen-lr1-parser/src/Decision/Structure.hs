module Decision.Structure where

import Data.Set (Set)

import Data.Set qualified as Set

import State             (State)
import Rule
import Position
import Term
import Data.Map.Monoidal (type (==>), (==>))

data Decision state
  = Shift state
  | Reduce Entity Clause
  | Accept
  deriving stock (Eq, Ord)

doShift :: State -> Set (Decision State)
doShift = Set.singleton . Shift

doReduce :: Entity -> Clause -> Set (Decision state)
doReduce = (Set.singleton .) . Reduce

doAccept :: Set (Decision state)
doAccept = Set.singleton Accept

reducingDecision :: Position -> Maybe Term ==> Set (Decision state)
reducingDecision pos
  | pos.entity == "Start" = Nothing       ==> doAccept
  | otherwise             = pos.lookahead ==> doReduce pos.entity pos.clause

onlyShift :: (Decision State) -> [State]
onlyShift = \case
  Shift state -> [state]
  _           -> []

mapDecisionState :: (a -> b) -> Decision a -> Decision b
mapDecisionState f = \case
  Shift    state -> Shift   (f state)
  Reduce e rule  -> Reduce e rule
  Accept         -> Accept
