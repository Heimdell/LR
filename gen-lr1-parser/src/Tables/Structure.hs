{- |
  Parse tables (ACTION & GOTO)
-}
module Tables.Structure where

import Data.Foldable     (toList, for_, fold)
import Data.Map.Monoidal (type (==>) (Monoidal), (==>))
import Data.Map          ((!))
import Data.Set          (Set)
import GHC.Generics      (Generic, Generically (..))

import Data.Set qualified as Set

import Fixpoint (graphClosure)
import Grammar  (Grammar())
import Position (Position(..))
import Position (splitPositionsByCategory, SortedPositions (..))
import State    (State(positions, State), closure)
import Term
import qualified Data.Map.Monoidal as Monoidal
import Control.Monad.Reader
import Control.Monad.State hiding (State, state)
import Control.Monad
import Decision.Structure
import Data.Function ((&))
import Control.Monad.Identity (Identity (runIdentity))

{- |
  In classic formulation, GOTO and ACTION are somewhat separate tables.

  I decided to join both tables by common dimension of parsing state.
-}
data Action state = Action
  { goto   :: Entity ==> state         -- ^ move after non-terminal is parsed
  , action :: Maybe Term   ==> Set (Decision state)  -- ^ action after terminal is parsed
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

data Conflict = Conflict
  { leading   :: [Point]
  , term      :: Maybe Term
  , positions :: Set Position
  }
  deriving stock (Eq, Ord)

type Conflicts = Set Position ==> Set Conflict

-- instance Pretty Conflict where
--   pPrint Conflict {leading, divergences, positions, term} = do
--     let
--       conflictingLines =
--         divergences & foldMap \case
--           Shifting -> mempty
--           Reducing e len -> e ==> Set.singleton do
--             let (before, after) = splitAt (length leading - len) leading
--             map blank before <> map squiggly after
--     vcat
--       ( hang "At input:" 2 do
--           vcat
--             ( fsep (map pPrint leading <> [pPrint term, "..."])
--             : do
--                 conflictingLines & Monoidal.assocs & foldMap \(e, lines) -> do
--                   lines & foldMap \line -> do
--                     [fsep (map pPrint line) <+> (pPrint e <> "?")]
--             )
--       : "At state:"
--       : foldMap (\pos -> [nest 2 (pPrint' pos)]) (groupPositionsByPrefices positions)
--       )
--     where
--       blank = fill ' '
--       squiggly = fill '~'
--       fill c = \case
--         T n (Term   term)   -> T n (Term   (Text.map (const c) term))
--         E n (Entity entity) -> E n (Entity (Text.map (const c) entity))

--       pPrint' :: PrettyPosition -> Doc
--       pPrint' = pPrint . resetNames

--       resetNames :: PrettyPosition -> PrettyPosition
--       resetNames pp = pp {clause = pp.clause {points = fmap noName pp.clause.points} }

--       noName :: Point -> Point
--       noName = \case
--         T _ term   -> T Nothing term
--         E _ entity -> E Nothing entity


conflicts :: Table State -> State -> Set Conflict
conflicts table state =
  fold $ (.foundConflicts) do
    runIdentity do
      flip execStateT mempty do
        flip runReaderT [] do
          tableToConflicts state table

data Discovered = Discovered
  { visitedStates :: Set State
  , foundConflicts :: Conflicts
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via Generically Discovered

type ConflictM = ReaderT [Point] (StateT Discovered Identity)

tableToConflicts :: State -> Table State -> ConflictM ()
tableToConflicts start Table{actions = Monoidal acts} = go start
  where
    go :: State -> ConflictM ()
    go st = do
      visited <- gets (Set.member st . (.visitedStates))
      unless visited do
        modify \disc -> disc {visitedStates = Set.insert st disc.visitedStates}
        let Action {action, goto} = acts ! st
        for_ (Monoidal.assocs goto) \(point, st') -> do
          local (++ [E Nothing point]) do
            go st'

        for_ (Monoidal.assocs action) \(lookahead, actions) -> do
          for_ actions \case
            Shift st' -> do
              case lookahead of
                Just term -> do
                  local (++ [T Nothing term]) do
                    go st'
                Nothing -> do
                  pure ()
            _ -> do
              pure ()

          case toList actions of
            [_] -> pure ()
            _   -> reportConflict st lookahead

    reportConflict :: State -> Maybe Term -> ConflictM ()
    reportConflict st term = do
      let
        positions = st.positions & Set.filter \pos ->
          case pos.locus of
            Just (T _ term') -> term == Just term'
            Nothing          -> pos.lookahead == term
            _                -> False

        cutPositions = positions & Set.map \pos ->
          case pos.locus of
            Nothing -> pos
            Just _  -> (pos :: Position) {lookahead = Just "?"}

        -- additional
      reported <- gets (Monoidal.member cutPositions . (.foundConflicts))
      unless reported do
        leading <- ask
        modify \desc ->
          desc {foundConflicts =
              Monoidal.insert cutPositions
              (Set.singleton Conflict
                { leading
                , positions = cutPositions
                , term}
              ) desc.foundConflicts
          }
