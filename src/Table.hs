
-- | Parsing table.
--
module Table where

import Data.Function (on)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ord (comparing)

import Map (Map, (==>))
import Set (Set)
import Set qualified as Set

import Item
import Name
import Point
import Pretty
import Rule
import Util

-- | A table.
--
data Table term result = Table
  { tRules :: [Rule term result]  -- list of rules
  }
  deriving Show via PP (Table term result)

instance Pretty term => Pretty (Table term result) where
  pretty = vcat . map pretty . tRules

-- | FIRSTS function.
--
--   A set of terminals the non-terminal can start with.
--
type Firsts term = Map Name (Set term)

-- | Generate FIRSTS function.
--
getFirsts
  :: forall term result
  .  (Ord term, Pretty term)
  => Table term result  -- ^ parsing table
  -> Firsts term
getFirsts table = close (foldMap firstsOfRule (tRules table)) mempty
  where
    firstsOfRule :: Rule term result -> Firsts term -> Firsts term
    firstsOfRule Rule { rName, rPoints } memo = case rPoints of
      []               -> mempty
      Term    term : _ -> rName ==> Set.ofOne term
      NonTerm name : _ -> rName ==> memo ? name

-- | FOLLOWS function.
--
type Follows term = Map Name (Set term)

-- | Generate FOLLOWS function from FIRSTS function.
--
--   A set of terminals that can immidiately follow a non-terminal.
--
getFollows
  :: forall term result
  .  (Ord term, Pretty term)
  => Firsts term        -- ^ FIRSTS function
  -> Table term result  -- parsing table
  -> term               -- EOF marker
  -> Follows term
getFollows firsts (Table rules) eof =
  close (foldMap followRule states) (Start ==> Set.ofOne eof)
  where
    states = do
      rule <- rules
      NonEmpty.toList (explode rule mempty)

    followRule :: Item1 term result -> Follows term -> Follows term
    followRule item memo = case (locus item, locus =<< next item) of
      (Just (NonTerm name), Nothing)              -> name ==> memo ? i1Name item
      (Just (NonTerm name), Just (Term    term))  -> name ==> Set.ofOne term
      (Just (NonTerm name), Just (NonTerm name')) -> name ==> firsts ? name'
      _                                           -> mempty

-- | Generate CLOSURE function.
--
getClosure
  :: forall term result
  .  (Ord term, Pretty term)
  => Table term result  -- ^ parsing table
  -> Firsts term        -- ^ FIRSTS function
  -> Follows term       -- ^ FOLLOWS function
  -> State term result  -- ^ non-saturated state
  -> State term result
getClosure (Table rules) firsts follows = joinByItemBody . close (foldMap act)
  where
    act item =
      Set.fromList
        case locus item of
          Just (NonTerm name) ->
            [ getFirstState rule
                case locus =<< next item of
                  Just (Term    term)  -> Set.ofOne term
                  Just (NonTerm name') -> firsts  ? name'
                  Nothing              -> follows ? i1Name item
            | rule <- rules
            , rName rule == name]
          _ -> mempty

    joinByItemBody
      :: (Ord term, Pretty term)
      => State term result
      -> State term result
    joinByItemBody
      = Set.fromList
      . map mergeItem1s
      . NonEmpty.groupBy ((==) `on` i1Item)
      . sortBy           (comparing i1Item)
      . Set.toList
      where
        mergeItem1s (Item1 rule lookeahead :| items') =
          Item1 rule (lookeahead <> mconcat (map i1Lookahead items'))

-- | Get first state of a table.
--
getFirstStateOfTable
  :: (Ord term, Pretty term)
  => Table term token  -- ^ parsing table
  -> Firsts term       -- ^ FIRSTS function
  -> Follows term      -- ^ FOLLOWS function
  -> term              -- ^ EOF marker
  -> Set (Item1 term token)
getFirstStateOfTable table@(Table rules) firsts follows lookeahead
  = getClosure table firsts follows
  $ Set.fromList
  [ getFirstState rule (Set.ofOne lookeahead)
  | rule <- rules
  , rName rule == Start
  ]

-- | Get set of all points from a table.
--
getPoints :: (Ord term, Pretty term) => Table term result -> Set (Point term)
getPoints = foldMap (Set.fromList . rPoints) . tRules

-- | Get set of all terminals from a table and EOF marker.
--
getTerminals :: (Ord term, Pretty term) => Table term result -> term -> Set term
getTerminals (Table rules) eof = Set.ofOne eof <> foldMap getRuleTerminals rules
  where
    getRuleTerminals = foldMap getPointTerminal . rPoints
    getPointTerminal = \case
      Term    term -> Set.ofOne term
      NonTerm _    -> mempty
