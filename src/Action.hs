
-- | An ACTION function - first component of the LR(1)-table.
--
module Action where

import Data.Function (on)
import Data.List (partition)

import Map (Map)
import Map qualified as Map
import Set (Set)
import Set qualified as Set

import Item
import Goto
import Point
import Pretty
import Util
import Term

{- | An action for automata.
-}
data Action state term
  = -- | Push a terminal, move to state.
    Shift    state

    -- | Reduce with rule, move to @GOTO (rule.name, state)@.
  | Reduce   (Item1 term)

    -- | Exit the automata with a result.
  | Accept

    -- | Not really an action, but possible outcome of automata builder.
    --
    --   Represents a conflict between 2 actions.
  | Conflict (Action state term) (Action state term)

    -- | An error node, reports what tokens were expected.
  | Expected (Set term)
  deriving stock (Eq)

instance (Pretty term, Pretty state) => Pretty (Action state term) where
  pretty = \case
    Shift    state      -> color 2 "Shift"    `indent` pretty state
    Reduce   result     -> color 5 "Reduce"   `indent` pretty result
    Accept              -> color 3 "Accept"
    Expected expected   -> color 7 "Expected" `indent` pretty expected
    Conflict left right -> color 1 "Conflict" `indent`
          "("  <.>     pretty left
      <.> "," `indent` pretty right
      <.> ")"

instance (Ord term, Eq state) => Semigroup (Action state term) where
  Expected left <> Expected right = Expected (left <> right)
  Expected _    <>          right = right
  left          <> Expected _     = left
  left          <>          right
    | left  ==               right  = left
    | left `eqUpToLookahead` right  = left `mergeLookahead` right
    | otherwise                     = Conflict left right

-- | Check if two `Action`-s are equal up to lookeahead in `Reduce`.
eqUpToLookahead
  :: Eq term
  => Action state term
  -> Action state term
  -> Bool
eqUpToLookahead (Reduce left) (Reduce right) = ((==) `on` i1Item) left right
eqUpToLookahead  _             _             = False

-- | Merge `Reduce` with same body but different lookahead.
mergeLookahead
  :: Ord term
  => Action state term
  -> Action state term
  -> Action state term
mergeLookahead (Reduce left) (Reduce right) =
  Reduce left
    { i1Lookahead = i1Lookahead left <> i1Lookahead right
    }
mergeLookahead _ _ = error "mergeLookahead: can only merge Reduce actions"

instance (Ord term, Eq state) => Monoid (Action state term) where
  mempty = Expected mempty

-- | An ACTION function.
--
type Act term
  =  State term
  -> term
  -> Action (State term) term

-- | An ACTION function, memoized.
--
type Act' term
  = Map (State term)
  ( Map term (Action (State term) term)
  )

-- | An ACTION function, with `Int` (state index) as state.
--
type Act'' term
  = Map (State term)
  ( Map term (Action Int term)
  )

-- | Construct an ACTION table from GOTO and EOF marker.
--
--   The idea is, if we are in state like
--
--   > { A -> B . d C, {$}
--   >   C -> D . B,   {<}
--   >   S -> . k,     {a, b}
--   >   F -> g g .,   {d}
--   > }
--
--   ... we are in /all of the productions simultaneously/.
--
--   Therefore we should look for productions that contain either given terminal
--   (to shift), or have ended and have given terminal in lookahead (to reduce).
--
--   If the grammar is correct, we will have either of them or both.
--
getAction
  :: (Ord term, Pretty term)
  => Goto (Term term)  -- ^ GOTO function
  -> Act (Term term)
getAction goto from term = foldMap decide from
  where
    decide item = case locus item of
      Just (Term term')
        | term == term' ->
          Shift (goto from (Term term))

      Nothing
        | i1Lookahead item ? term ->
          if isStart from && term == Eof
          then Accept
          else Reduce item

      _ -> Expected mempty

-- | Fill the `Expected` actions with terms that are expected at the point.
--
populateExpectedTokens
  :: (Ord term, Pretty term)
  => Act' term
  -> Act' term
populateExpectedTokens = Map.map populate
  where
    populate (Map.toList -> mapping) = do
      let (fails, successes) = partition (isFailure . snd) mapping
      let expected = Set.fromList [term | (term, _act) <- successes]
      Map.fromList $ ((Expected expected <$) <$> fails) <> successes

    isFailure = \case
      Expected {} -> True
      _           -> False

-- -- | Replace `Set`-s of `Item1`-s with `Int`-s.
-- --
-- witherActionMap
--   :: forall term
--   .  Ord term
--   => Act' term  -- ^ ACTION function (memoized)
--   -> Set term          -- ^ set of all terminals
--   -> (Act'' term, Int)
-- witherActionMap actions terms = (actions', start')
--   where
--     states  = Map.keySet actions
--     states' = Set.toList states
--     [start] = filter isStart states'
--     encode  = Map.fromList $ zip [0..] states'
--     decode  = Map.fromList $ zip states' $ map (First . Just) [0..]

--     First (Just start') = decode ? start

--     actions' :: Act'' term
--     actions' =
--       flip materialize states \state ->
--       flip materialize terms  \term  ->
--         decodeAction (actions ? state ? term)

--     decodeAction
--       :: Action (State term) term
--       -> Action Int term
--     decodeAction = \case
--       Shift state -> do
--         let First (Just state') = decode ? state
--         Shift state'

--       Reduce   rule     -> Reduce rule
--       Accept            -> Accept
--       Conflict l r      -> Conflict (decodeAction l) (decodeAction r)
--       Expected expected -> Expected expected
