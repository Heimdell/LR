module Decision.Structure where

import Data.Set (Set)

import Data.Set qualified as Set

import State             (State)
import Rule              (Rule(entity))
import Position          (Position(rule, lookahead))
import Term              (Term)
import Data.Map.Monoidal (type (==>), (==>))

data Decision
  = Shift State
  | Reduce Rule
  | Accept
  deriving stock (Eq, Ord)

doShift :: State -> Set Decision
doShift = Set.singleton . Shift

doReduce :: Rule -> Set Decision
doReduce = Set.singleton . Reduce

doAccept :: Set Decision
doAccept = Set.singleton Accept

reducingDecision :: Position -> Term ==> Set Decision
reducingDecision pos
  | pos.rule.entity == "S" = "$"           ==> doAccept
  | otherwise              = pos.lookahead ==> doReduce pos.rule

onlyShift :: Decision -> [State]
onlyShift = \case
  Shift state -> [state]
  _           -> []
