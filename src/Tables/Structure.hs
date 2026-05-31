{- |
  Parse tables (ACTION & GOTO)
-}
module Tables.Structure where

import Data.Map.Monoidal (type (==>), (!), (==>))
import Data.Set          (Set)
import Data.Set          qualified as Set
import GHC.Generics      (Generic, Generically (..))

import Fixpoint          ((>>-), graphClosure)
import Grammar           (Grammar(rules))
import Position          (Position(..), start, lookaheadAfterCurrentPoint)
import Term              (Point(..), Term, Entity)
import Data.Foldable     (toList)
import Rule
import Position.Structure (splitPositionsByCategory, SortedPositions (..))

{- |
  Parser state.

  State if formed from a set of positions named "kernel", by
  calculating the `closure` of the set.

  For instance, kernel:

  > T = ( .E )  {$}
  > E = .E + F  {+}

  Can lead to a closure of:

  > E = .E + F    { ) + - }
  > E = .E - F    { ) + - }
  > E = .F        { ) + - }
  > F = .F * T    { ) * + - / }
  > F = .F / T    { ) * + - / }
  > F = .T        { ) * + - / }
  > T = .( E )    { ) * + - / }
  > T = .number   { ) * + - / }
  > T = ( .E )    {$}
-}
data State = State
  { positions :: Set Position
  , kernel    :: Set Position
  }
  deriving stock (Eq, Ord, Generic)
  deriving       (Semigroup, Monoid) via Generically State

{- |
  Calculate a closure of a kernel, using grammar and included FIRST table.

  For each entity to be parsed, start all its generating rules recursively.
  Add appropriate lookaheads.

  > T = ( .E )  {$}
  > E = .E + F  {+}

  > T = ( .E )          {$}
  >   E = .E - F        { ) + - }
  > E = .E + F          { ) + - }
  >   E = .F            { ) + - }
  >     F = .F * T      { ) * + - / }
  >     F = .F / T      { ) * + - / }
  >     F = .T          { ) * + - / }
  >       T = .( E )    { ) * + - / }
  >       T = .number   { ) * + - / }
-}
closure :: Grammar -> Set Position -> State
closure grammar kernel = State {kernel, positions}
  where
    {- Grow kernel set, until no new positions can be added.
    -}
    positions :: Set Position
    positions
      =  kernel  -- have to be included manually,
                 -- graphClosure considers it a "vertex"
      <> graphClosure (foldMap starts) pure kernel
                    -- ^ collate result of each position

    {- Find which positions are implied by current one.

       In terms of `graphClosure` those positions are "adjacentSubgraph".
    -}
    starts :: Position -> Set Position
    starts pos = case pos.locus of
      {- If position expects entity,
         for each rule producing such entity
         and each term that can happen right after it,
         start that tule with that term as a lookahead.
      -}
      Just (E entity) ->
        grammar.rules ! entity                 >>- \rule ->
        lookaheadAfterCurrentPoint grammar pos >>- \term ->
          Set.singleton (start rule term)

      _ -> mempty

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

data Decision
  = Shift State
  | Reduce Rule
  -- | Conflict (Set Decision)
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
