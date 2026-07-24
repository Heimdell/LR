module Transitions where
import Symbol
import Data.Map.Monoidal (type (==>))
import Data.Set (Set)
import Decision
import Pretty
import qualified Data.Set as Set
import Data.Foldable
import Data.Traversable
import LR1State
import LR1Item
import Rule
import qualified Data.Map.Monoidal as Monoidal

data TransitionsTo state = TransitionsTo
  { gotos   :: NonTerminal ==> state                 -- transitions on non-terminals
  , actions :: Lookahead   ==> Set (Decision state)  -- transitions on terminals
  }
  deriving stock (Eq)
  deriving (Show) via PP (TransitionsTo state)

{- |
  Merge two transition sets.

  It should be impossible to arrive at different GOTO() parts of the transitions
  in the merged state.

  This is because the states are checked to be equal with their LR(0)-cores,
  thus all LR(0)-items like [X <- A • B C] should be exactly equal in the both states.

  Due to that, I choose to ignore the left GOTO-part.

  Proper solution, I guess, would be to separate GOTO and ACTION parts and collecting them in
  different manner.
-}
instance Ord state => Semigroup (TransitionsTo state) where
  one <> another = TransitionsTo
    { gotos   = another.gotos
    , actions = one.actions <> another.actions
    }

{- |
  The parameter is behind `Set`, so I have to make `fmap` myself.
-}
fmapTransitionsTo :: Ord st' => (st -> st') -> TransitionsTo st -> TransitionsTo st'
fmapTransitionsTo f TransitionsTo {gotos, actions} = TransitionsTo
  { gotos = f <$> gotos
  , actions = Set.map (fmap f) <$> actions
  }

{- |
  The parameter is behind `Set`, so I have to make `traverse` myself.
-}
traverseTransitionsTo :: (Ord st', Applicative m) => (st -> m st') -> TransitionsTo st -> m (TransitionsTo st')
traverseTransitionsTo f TransitionsTo {gotos, actions} = TransitionsTo
  <$> traverse f gotos
  <*> (traverse . traverseSet . traverse) f actions

traverseSet :: (Ord st', Applicative m) => (st -> m st') -> Set st -> m (Set st')
traverseSet f set = do
  fold <$> do
    for (Set.toList set) \a -> do
      Set.singleton <$> f a

instance Pretty st => Pretty (TransitionsTo st) where
  pPrint TransitionsTo {gotos, actions} = vcat
    [ pPrint gotos
    , pPrint actions
    ]

transitionsFrom :: LR1State -> TransitionsTo Kernel
transitionsFrom st = TransitionsTo {gotos, actions = shifts <> reduces}
  where
    gotos :: NonTerminal ==> Kernel
    gotos = Kernel . Set.map next <$> sorted.expectNonTerminal

    reduces :: Lookahead ==> Set (Decision Kernel)
    reduces = Set.map reduce <$> sorted.canReduce

    shiftsHaveNoEOF :: Lookahead ==> Set ShiftingLR1Item
    shiftsHaveNoEOF = Monoidal.mapKeys LookForTerm sorted.expectTerminal

    shifts :: Lookahead ==> Set (Decision Kernel)
    shifts = Set.singleton . Shift . Kernel . Set.map next <$> shiftsHaveNoEOF

    sorted :: SortedState
    sorted = sortState st

    reduce :: Rule -> Decision Kernel
    reduce rule = if rule.entity == S then Accept else Reduce rule
