{- |
  Parse tables (ACTION & GOTO)
-}
module Tables.Structure where

import Data.Map.Monoidal (type (==>), (==>))
import Data.Set          (Set)
import Data.Set          qualified as Set
import GHC.Generics      (Generic, Generically (..))

import Fixpoint          (graphClosure)
import Grammar           (Grammar())
import Position          (Position(..))
import Term              (Term, Entity)
import Data.Foldable     (toList)
import Position          (splitPositionsByCategory, SortedPositions (..))
import State             (State(positions, State), closure)
import Decision          (Decision, doShift, reducingDecision, onlyShift)

{- |
  In classic formulation, GOTO and ACTION are somewhat separate tables.

  I decided to join both tables by common dimension of parsing state.
-}
data Action = Action
  { goto   :: Entity ==> State         -- ^ move after non-terminal is parsed
  , action :: Term   ==> Set Decision  -- ^ action after terminal is parsed
  }
  deriving stock (Eq, Ord, Generic)
  deriving       (Semigroup, Monoid) via Generically Action

{- |
  Find nodes the `Action` subtable can lead into.
-}
endpointNodes :: Action -> [State]
endpointNodes Action {goto, action}
  =  toList goto
  <> (toList action >>= foldMap onlyShift)

data Table = Table
  { actions :: State ==> Action
  }
  deriving stock (Eq, Ord, Generic)
  deriving       (Semigroup, Monoid) via Generically Table

{- |
  Collect targed nodes of subgraph.
-}
collectTargetStates :: Table -> [State]
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
adjacentSubgraph :: Grammar -> State -> Table
adjacentSubgraph grammar state@State {positions} =
  Table do
    state ==> gotos <> shifts <> reduce
  where
    sorted = splitPositionsByCategory positions

    gotos, shifts, reduce :: Action
    gotos  = mempty { goto   =           advanceOnePoint grammar <$> sorted.expectsEntity   }
    shifts = mempty { action = doShift . advanceOnePoint grammar <$> sorted.expectsTerminal }
    reduce = mempty { action =           foldMap reducingDecision    sorted.needsReduction  }

makeTables :: Grammar -> State -> Table
makeTables grammar firstState =
  graphClosure (adjacentSubgraph grammar) collectTargetStates firstState
