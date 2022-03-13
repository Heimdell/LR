module LR1.GOTO where

import Control.Monad (foldM)
import Control.Monad.State qualified as MTL
import Data.Function (on, (&))
import Data.List (groupBy, sortBy)
import Data.Maybe (mapMaybe, fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import GHC.Generics (Generic)

import LR1.FIRST   qualified as FIRST
import LR1.Fixpoint (one, Get ((?)), set)
import LR1.Grammar qualified as Grammar
import LR1.Item    qualified as Item
import LR1.Map     qualified as Map
import LR1.Point   qualified as Point
import LR1.State   qualified as State

newtype T = GOTO
  { unwrap :: Map.T State.Index (Map.T Point.T State.Index)
  }
  deriving stock (Show, Generic)

newtype Typed t a = Typed T

instance Get LR1.GOTO.T (State.Index, Point.T) State.Index where
  GOTO m ? (i, t) = m Map.! i Map.! t

make :: forall m. State.HasReg m => Grammar.T -> FIRST.T -> m LR1.GOTO.T
make grammar first = do
  state0 <- State.firstState grammar first
  loop (one state0) (GOTO Map.empty)
  where
    -- Add states to the state registry until no new states can be found.
    loop :: Set State.Index -> LR1.GOTO.T -> m LR1.GOTO.T
    loop pool goto = do
      (pool', goto') <- foldM add (Set.empty, goto) pool
      if Set.null pool'
      then return goto'
      else loop pool' goto'

    -- Given a state, register it in registry and all its transitions in GOTO.
    add :: (Set State.Index, LR1.GOTO.T) -> State.Index -> m (Set State.Index, LR1.GOTO.T)
    add (pool, GOTO goto) index = do
      -- Add the state to goto.
      -- Unless there's empty (Point -> State.Index) map, we can't add stuff later.
      let
        goto'
          | Map.member index goto = goto
          | otherwise             = Map.insert index Map.empty goto

      materialized <- MTL.gets ((Map.! index) . State.indices)

      let
        -- Group all items in the state by the entity at the locus
        -- (entity said item must accept to proceed)
        itemsByNextPoint =
          materialized
            & State.items               -- get Item set
            & Set.toList                -- get Item list
            & mapMaybe Item.uncons      -- select (Locus, Next) from non-reducing Items
            & sortBy (on compare fst)   -- groupBy can only work with sorted ._.
            & groupBy (on (==) fst)     -- group into (Locus, Next)
                                        -- v-- turn [(Locus, Next)] into (Locus, [Next])
            & fmap do \pairs@((point, _) : _) -> (point, set (map snd pairs))

      -- for each (Locus, [Next]) generate new state as CLOSURE([Next])
      -- and add all (ThisState => Locus => NewState) transitions to GOTO table.
      foldM (addItem index) (pool, GOTO goto') itemsByNextPoint

    -- for (Locus, [Next]) generate new state as CLOSURE([Next])
    -- and add (ThisState => Locus => NewState) transition to GOTO table.
    addItem :: State.Index -> (Set State.Index, LR1.GOTO.T) -> (Point.T, Set Item.T) -> m (Set State.Index, LR1.GOTO.T)
    addItem index (pool, GOTO goto) (point, items) = do
      -- NewState = CLOSURE([Next])
      (nextIndex, new) <- State.closure grammar first items

      -- if the new state was not registered yet, add it to the pool
      -- of next iteration
      let pool' = if new then Set.insert nextIndex pool else pool

      -- Add (ThisState => Locus => NewState) transition to GOTO.
      let goto' = Map.adjust (Map.insert point nextIndex) index goto
      return (pool', GOTO goto')

expected :: LR1.GOTO.T -> State.Index -> Map.T Point.T State.Index
expected (GOTO goto) index = goto Map.! index

dump :: State.HasReg m => Text -> LR1.GOTO.T -> m String
dump header (GOTO goto) = do
  let
    asList = goto
      & Map.toList
      & (fmap.fmap) Map.toList
  stateList <- for asList \(srcIndex, dests) -> do
    srcState <- MTL.gets ((Map.! srcIndex) . State.indices)
    destStates <- for dests \(point, destIndex) -> do
      destState <- MTL.gets ((Map.! destIndex) . State.indices)
      return (point, destState)
    return (srcState, destStates)

  let
    showBlock (src, dests)  = unlines (show src : fmap showDest dests)
    showDest  (point, dest) = "  " <> show point <> "\t" <> show dest

  return $ stateList
    & fmap showBlock
    & (Text.unpack header :)
    & unlines

(!?) :: [(Int, Int)] -> State.T -> State.T
table !? state = state { State.index = fromJust $ lookup (State.index state) table }
