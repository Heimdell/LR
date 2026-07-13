module Decision.Structure where

import Data.Set (Set)

import Data.Set qualified as Set

import LR1State             (LR1State)
import Rule
import LR1Item
import Symbol
import Data.Map.Monoidal (type (==>), (==>))

data Decision state
  = Shift state
  | Reduce Entity Clause
  | Accept
  deriving stock (Eq, Ord)

doShift :: LR1State -> Set (Decision LR1State)
doShift = Set.singleton . Shift

doReduce :: Entity -> Clause -> Set (Decision state)
doReduce = (Set.singleton .) . Reduce

doAccept :: Set (Decision state)
doAccept = Set.singleton Accept

reducingDecision :: LR1Item -> Lookahead ==> Set (Decision state)
reducingDecision pos
  | pos.entity == "Start" = LookForEOF    ==> doAccept
  | otherwise             = pos.lookahead ==> doReduce pos.entity pos.clause

onlyShift :: (Decision LR1State) -> [LR1State]
onlyShift = \case
  Shift state -> [state]
  _           -> []

mapDecisionState :: (a -> b) -> Decision a -> Decision b
mapDecisionState f = \case
  Shift    state -> Shift   (f state)
  Reduce e rule  -> Reduce e rule
  Accept         -> Accept
