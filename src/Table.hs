
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
import Term

-- | A table.
--
data Table term = Table
  { tRules :: [Rule term]  -- list of rules
  }
  deriving Show via PP (Table term)

instance Pretty term => Pretty (Table term) where
  pretty = vcat . map pretty . tRules

-- | FIRSTS function.
--
--   A set of terminals the non-terminal can start with.
--
type Firsts term = Map Name (Set term)

-- | Generate FIRSTS function.
--
getFirsts
  :: forall term
  .  (Ord term, Pretty term)
  => Table term  -- ^ parsing table
  -> Firsts (Term term)
getFirsts table = close (foldMap firstsOfRule (tRules table)) mempty
  where
    firstsOfRule :: Rule term -> Firsts (Term term) -> Firsts (Term term)
    firstsOfRule Rule { rName, rPoints } memo = case rPoints of
      []               -> mempty
      Term    term : _ -> rName ==> Set.ofOne (Next term)
      NonTerm name : _ -> rName ==> memo ? name

-- | FOLLOWS function.
--
type Follows term = Map Name (Set term)

-- | Generate FOLLOWS function from FIRSTS function.
--
--   A set of terminals that can immidiately follow a non-terminal.
--
getFollows
  :: forall term
  .  (Ord term, Pretty term)
  => Table term  -- parsing table
  -> Firsts (Term term)        -- ^ FIRSTS function
  -> Follows (Term term)
getFollows (Table rules) firsts =
  close (foldMap followRule states) (Start ==> Set.ofOne Eof)
  where
    states = do
      rule <- rules
      NonEmpty.toList (explode rule mempty)

    followRule :: Item1 (Term term) -> Follows (Term term) -> Follows (Term term)
    followRule item memo = case (locus item, locus =<< next item) of
      (Just (NonTerm name), Nothing)              -> name ==> memo ? i1Name item
      (Just (NonTerm name), Just (Term    term))  -> name ==> Set.ofOne term
      (Just (NonTerm name), Just (NonTerm name')) -> name ==> firsts ? name'
      _                                           -> mempty

-- | Generate CLOSURE function.
--
getClosure
  :: forall term
  .  (Ord term, Pretty term)
  => Table term  -- ^ parsing table
  -> Firsts (Term term)        -- ^ FIRSTS function
  -> Follows (Term term)       -- ^ FOLLOWS function
  -> State (Term term)  -- ^ non-saturated state
  -> State (Term term)
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
      => State (Term term)
      -> State (Term term)
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
  => Table term  -- ^ parsing table
  -> Firsts (Term term)       -- ^ FIRSTS function
  -> Follows (Term term)      -- ^ FOLLOWS function
  -> Set (Item1 (Term term))
getFirstStateOfTable table@(Table rules) firsts follows
  = getClosure table firsts follows
  $ Set.fromList
  [ getFirstState rule (Set.ofOne Eof)
  | rule <- rules
  , rName rule == Start
  ]

-- | Get set of all points from a table.
--
getPoints :: (Ord term, Pretty term) => Table term -> Set (Point (Term term))
getPoints = foldMap (Set.fromList . (map.fmap) Next . rPoints) . tRules

-- | Get set of all terminals from a table and EOF marker.
--
getTerminals :: (Ord term, Pretty term) => Table term -> Set (Term term)
getTerminals (Table rules) = Set.ofOne Eof <> foldMap getRuleTerminals rules
  where
    getRuleTerminals = foldMap getPointTerminal . rPoints
    getPointTerminal = \case
      Term    term -> Set.ofOne (Next term)
      NonTerm _    -> mempty
