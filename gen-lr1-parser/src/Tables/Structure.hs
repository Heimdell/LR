{- |
  Parse tables (ACTION & GOTO)
-}
module Tables.Structure where

import Data.Foldable     (toList, for_)
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
import Rule
import qualified Data.Map.Monoidal as Monoidal
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Control.Monad.Writer
import Control.Monad
import Decision.Structure
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Data.Traversable
import Data.Function ((&))
import qualified Data.Text as Text

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

data Conflict = Conflict
  { leading     :: [Point]
  , divergences :: Set Line
  , term        :: Term
  }
  deriving stock (Eq, Ord)

data Line
  = Reducing Entity Int
  | Shifting
  deriving stock (Eq, Ord)

instance Pretty Conflict where
  pPrint Conflict {leading, divergences, term} = do
    let
      conflictingLines =
        divergences & foldMap \case
          Shifting -> mempty
          Reducing e len -> e ==> Set.singleton do
            let (before, after) = splitAt (length leading - len) leading
            map blank before <> map squiggly after
    vcat
      ( fsep (map pPrint leading <> [pPrint term])
      : do
          conflictingLines & Monoidal.assocs & foldMap \(e, lines) -> do
            lines & foldMap \line -> do
              [fsep (map pPrint line) <+> "<--" <+> pPrint e]
      )
    where
      blank = fill ' '
      squiggly = fill '~'
      fill c = \case
        T n (Term   term)   -> T n (Term   (Text.map (const c) term))
        E n (Entity entity) -> E n (Entity (Text.map (const c) entity))

conflicts :: Table Int -> Set Conflict
conflicts table =
  execWriter do
    flip evalStateT mempty do
      flip runReaderT [] do
        tableToConflicts table

type ConflictM = ReaderT [Point] (StateT (Set Int) (Writer (Set Conflict)))

tableToConflicts :: Table Int -> ConflictM ()
tableToConflicts Table{actions = Monoidal actions} = go 0
  where
    go :: Int -> ConflictM ()
    go st = do
      visited <- gets (Set.member st)
      unless visited do
        modify (Set.insert st)
        let Action {action, goto} = actions ! st
        for_ (Monoidal.assocs action) \(term, actions) -> do
          for_ actions \case
            Shift st' -> do
              local (++ [T Nothing term]) do
                go st'
            _ -> pure ()

          case toList actions of
            [x] -> pure ()
            xs  -> reportConflict term xs

        for_ (Monoidal.assocs goto) \(point, st') -> do
          local (++ [E Nothing point]) do
            go st'

    reportConflict :: Term -> [Decision Int] -> ConflictM ()
    reportConflict term decisions = do
      leading <- ask
      let divergences = foldMap (Set.singleton . decisionToLine) decisions
      tell $ Set.singleton Conflict {leading, divergences, term}

decisionToLine :: Decision Int -> Line
decisionToLine = \case
  Shift  _        -> Shifting
  Reduce e clause -> Reducing e (length clause.points)
  Accept          -> Reducing "Start" 1
