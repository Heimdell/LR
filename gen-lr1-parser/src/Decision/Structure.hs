module Decision.Structure where

import Data.Set (Set)

import Data.Set qualified as Set

import State             (State)
import Rule
import Position
import Term              (Term)
import Data.Map.Monoidal (type (==>), (==>))

data Decision state
  = Shift state
  | Reduce Clause
  | Accept
  deriving stock (Eq, Ord)

doShift :: State -> Set (Decision State)
doShift = Set.singleton . Shift

doReduce :: Clause -> Set (Decision state)
doReduce = Set.singleton . Reduce

doAccept :: Set (Decision state)
doAccept = Set.singleton Accept

reducingDecision :: Position -> Term ==> Set (Decision state)
reducingDecision pos
  | pos.entity == "S" = "$"           ==> doAccept
  | otherwise         = pos.lookahead ==> doReduce pos.clause

onlyShift :: (Decision State) -> [State]
onlyShift = \case
  Shift state -> [state]
  _           -> []

mapDecisionState :: (a -> b) -> Decision a -> Decision b
mapDecisionState f = \case
  Shift  state -> Shift (f state)
  Reduce rule  -> Reduce rule
  Accept       -> Accept
