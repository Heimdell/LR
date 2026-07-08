{- |
  Parse tables (ACTION & GOTO)
-}
module Tables.Structure where

import Data.Foldable     (toList)
import Data.Map.Monoidal (type (==>) (Monoidal), (==>))
import Data.Set          (Set)
import GHC.Generics      (Generic, Generically (..))

import Data.Set qualified as Set

import Decision (Decision, doShift, reducingDecision, onlyShift, mapDecisionState)
import Fixpoint (graphClosure)
import Grammar  (Grammar())
import Position (Position(..))
import Position (splitPositionsByCategory, SortedPositions (..))
import State    (State(positions, State), closure)
import Term     (Term, Entity)
import qualified Data.Map.Monoidal as Monoidal

{- |
  In classic formulation, GOTO and ACTION are somewhat separate tables.

  I decided to join both tables by common dimension of parsing state.
-}
data Action state = Action
  { goto   :: Entity ==> state         -- ^ move after non-terminal is parsed
  , action :: Term   ==> Set (Decision state)  -- ^ action after terminal is parsed
  }
  deriving stock (Eq, Ord, Generic)
  deriving       (Semigroup, Monoid) via Generically (Action state)

mapActionState :: (Ord b, Semigroup b) => (a -> b) -> Action a -> Action b
mapActionState f Action {goto, action} = Action
  { goto   = fmap f goto
  , action = fmap (Set.map (mapDecisionState f)) action
  }

{- |
  Find nodes the `Action` subtable can lead into.
-}
endpointNodes :: Action State -> [State]
endpointNodes Action {goto, action}
  =  toList goto
  <> (toList action >>= foldMap onlyShift)

newtype Table state = Table
  { actions :: state ==> Action state
  }
  deriving stock (Eq, Ord, Generic)
  deriving       (Semigroup, Monoid) via Generically (Table state)

mapTableState :: forall a b. (Ord b, Semigroup b) => (a -> b) -> Table a -> Table b
mapTableState f = Table . Monoidal.foldMapWithKey aux . (.actions)
  where
    aux :: a -> Action a -> b ==> Action b
    aux a b = f a ==> mapActionState f b

{- |
  Collect targed nodes of subgraph.
-}
collectTargetStates :: Table State -> [State]
collectTargetStates Table {actions} = foldMap endpointNodes actions

{- |
  Advance a set of positions one point each and build a closure of them.

  It is assumed that /all/ positions are at the same point, for instance:

  > T = ( .E )
  > E = .E + F
-}
advanceOnePoint :: Grammar -> Set Position -> State
advanceOnePoint grammar
  = closure grammar
  . foldMap (foldMap Set.singleton . (.next))

{- |
  Construct part of parser transition graph, adjacentSubgraph to given state.

  There are 3 kinds of positions in state:
  1) Expects nonterminal: E = E  + .F
  2) Expects terminal:    E = E .+  F
  3) Requies reduction:   E = E  +  F .

  The (1) generates GOTO part of the table.
  The (2) generates SHIFTs.
  The (3) generates REDUCEs.
-}
adjacentSubgraph :: Grammar -> State -> Table State
adjacentSubgraph grammar state@State {positions} =
  Table do
    state ==> gotos <> shifts <> reduce
  where
    sorted = splitPositionsByCategory positions

    gotos, shifts, reduce :: Action State
    gotos  = mempty { goto   =           advanceOnePoint grammar <$> sorted.expectsEntity   }
    shifts = mempty { action = doShift . advanceOnePoint grammar <$> sorted.expectsTerminal }
    reduce = mempty { action =           foldMap reducingDecision    sorted.needsReduction  }

makeTables :: Grammar -> State -> Table State
makeTables grammar firstState =
  graphClosure (adjacentSubgraph grammar) collectTargetStates firstState
