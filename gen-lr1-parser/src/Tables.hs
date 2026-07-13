module Tables where

import Data.Foldable                  (toList)
import Data.Function                  ((&))
import Text.PrettyPrint.HughesPJClass hiding ((<>))

import Decision()

import Data.Map.Monoidal qualified as Map
import Symbol
import LR1Item
import Rule
import Data.Text (Text)

import Data.Foldable     (for_, fold)
import Data.Map.Monoidal (type (==>) (Monoidal), (==>))
import Data.Map          ((!))
import Data.Set          (Set)
import GHC.Generics      (Generic, Generically (..))

import Data.Set qualified as Set

import Control.Fixpoint (graphClosure)
import Grammar  (Grammar())
import LR1State    (LR1State(positions, LR1State), closure)
import qualified Data.Map.Monoidal as Monoidal
import Control.Monad.Reader
import Control.Monad.State hiding (State, state)
import Control.Monad
import Decision
import Control.Monad.Identity (Identity (runIdentity))

{- |
  In classic formulation, GOTO and ACTION are somewhat separate tables.

  I decided to join both tables by common dimension of parsing state.
-}
data Action state = Action
  { goto   :: NonTerminal    ==> state         -- ^ move after non-terminal is parsed
  , action :: Lookahead ==> Set (Decision state)  -- ^ action after terminal is parsed
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
endpointNodes :: Action LR1State -> [LR1State]
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
collectTargetStates :: Table LR1State -> [LR1State]
collectTargetStates Table {actions} = foldMap endpointNodes actions

{- |
  Advance a set of positions one point each and build a closure of them.

  It is assumed that /all/ positions are at the same point, for instance:

  > T = ( .E )
  > E = .E + F
-}
advanceOnePoint :: Grammar -> Set LR1Item -> LR1State
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
adjacentSubgraph :: Grammar -> LR1State -> Table LR1State
adjacentSubgraph grammar state@LR1State {positions} =
  Table do
    state ==> gotos <> shifts <> reduce
  where
    sorted = splitPositionsByCategory positions

    gotos, shifts, reduce :: Action LR1State
    gotos  = mempty { goto   =           advanceOnePoint grammar <$> sorted.expectsNonTerminal   }
    shifts = mempty { action = doShift . advanceOnePoint grammar <$> sorted.expectsTerminal }
    reduce = mempty { action =           foldMap reducingDecision    sorted.needsReduction  }

makeTables :: Grammar -> LR1State -> Table LR1State
makeTables grammar firstState =
  graphClosure (adjacentSubgraph grammar) collectTargetStates firstState

data Conflict = Conflict
  { leading   :: [Symbol]
  , term      :: Lookahead
  , positions :: Set LR1Item
  }
  deriving stock (Eq, Ord)

type Conflicts = Set LR1Item ==> Set Conflict

conflicts :: Table LR1State -> LR1State -> Set Conflict
conflicts table state =
  fold $ (.foundConflicts) do
    runIdentity do
      flip execStateT mempty do
        flip runReaderT [] do
          tableToConflicts state table

data Discovered = Discovered
  { visitedStates :: Set LR1State
  , foundConflicts :: Conflicts
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via Generically Discovered

type ConflictM = ReaderT [Symbol] (StateT Discovered Identity)

tableToConflicts :: LR1State -> Table LR1State -> ConflictM ()
tableToConflicts start Table{actions = Monoidal acts} = go start
  where
    go :: LR1State -> ConflictM ()
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
                LookForTerm term -> do
                  local (++ [T Nothing term]) do
                    go st'
                LookForEOF -> do
                  pure ()
            _ -> do
              pure ()

          case toList actions of
            [_] -> pure ()
            _   -> reportConflict st lookahead

    reportConflict :: LR1State -> Lookahead -> ConflictM ()
    reportConflict st term = do
      let
        positions = st.positions & Set.filter \pos ->
          case pos.locus of
            Just (T _ term') -> term == LookForTerm term'
            Nothing          -> pos.lookahead == term
            _                -> False

        cutPositions = positions & Set.map \pos ->
          case pos.locus of
            Nothing -> pos
            Just _  -> (pos :: LR1Item) {lookahead = LookForTerm "?"}

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

-------------------------------------------------------------------------------

instance (Pretty state) => Pretty (Action state) where
  pPrint Action {goto, action} = vcat
    [ goto
        & Map.assocs
        & map (\(node, fan) -> hang (pPrint node) 2 (pPrint fan))
        & punctuate "\n" & vcat
    , "   "
    , action
        & Map.assocs
        & map (\(node, fan) -> hang (pPrint node) 2 (vcat (map pPrint (toList fan))))
        & punctuate "\n" & vcat
    ]

instance (Pretty state) => Pretty (Table state) where
  pPrint Table {actions} = pPrint actions

instance Pretty Conflict where
  pPrint Conflict {leading, positions, term} = vcat
    [ "There is a conflict for input sequence"
    , nest 2 do fsep (map pPrint leading) <+> pPrintShaded term <+> pPrintShaded ("..." :: Text)
    , "  "
    , hang ("Сonflicting positions of rules for lookahead" <+> pPrintShaded term <+> "in the same parsing state are") 2 do
      vcat do
        positions & foldMap \pos ->
          [positionLine leading pos]
    , "  "
    ]

positionLine :: [Symbol] -> LR1Item -> Doc
positionLine points pos = do
  let (before, after) = splitAt (length points - pos.offset) points
  let additional = drop pos.offset $ toList pos.clause.points
  if null additional
  then do
    vcat [fsep
      [ fsep (map pPrint before)
      , zeroWidthText "\ESC[4m" <> fsep (map pPrint after) <> zeroWidthText "\ESC[0m"
      , zeroWidthText "\ESC[0m\ESC[2m" <> pPrint pos.lookahead
      , "..." <> zeroWidthText "\ESC[0m"
      ], nest 2 (pPrintShaded pos)]
  else do
    vcat [fsep
      [ fsep (map pPrint before)
      , zeroWidthText "\ESC[4m" <> fsep (map pPrint after) <> zeroWidthText "\ESC[2;3;4m"
      , fsep (map pPrint additional) <> zeroWidthText "\ESC[0m"
      , zeroWidthText "\ESC[0m\ESC[2;3m" <> pPrint pos.lookahead
      , "..." <> zeroWidthText "\ESC[0m"
      ], nest 2 (pPrintShaded pos)]

pPrintShaded :: Pretty a => a -> Doc
pPrintShaded point =
  zeroWidthText "\ESC[2m" <> pPrint point <> zeroWidthText "\ESC[0m"
