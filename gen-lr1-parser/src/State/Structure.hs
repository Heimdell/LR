module State.Structure where

import Data.Set      (Set)
import Data.Function (on, (&))
import GHC.Generics  (Generic, Generically(..))

import Data.Set qualified as Set

import Data.Map.Monoidal ((!))
import Fixpoint          ((>>-), graphClosure)
import Grammar           (Grammar(rules))
import Position
import Rule
import Term              (Point(E))

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
  deriving stock (Generic)
  deriving       (Semigroup, Monoid) via Generically State

instance Eq  State where (==)    = (==)    `on` (.kernel)
instance Ord State where compare = compare `on` (.kernel)

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
closure grammar kernel = do
    State {kernel, positions}
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
      Just (E _ entity) ->
          grammar.rules ! entity                 >>- \rule ->
          lookaheadAfterCurrentPoint grammar pos >>- \term ->
          (rule.clauses :: [Clause])             >>- \clause ->
            Set.singleton (start rule.entity rule.type_ clause term)

      _ -> mempty

{- |
  Starting position for test grammar.
-}
startingState :: Grammar -> State
startingState grammar =
  closure grammar do
    (grammar.rules ! "Start") & foldMap \rule -> do
      rule.clauses >>- \clause -> do
        Set.singleton (start rule.entity rule.type_ clause Nothing)
