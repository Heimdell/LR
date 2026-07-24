module Tables where
import Grammar
import LR1State
import Transitions
import Data.Set (Set)
import Control.Monad.Reader
import Control.Monad.State hiding (state)
import Data.Text (Text)
import Symbol
import Data.Map (Map, (!))
import LR0Item
import LR1Item
import Data.Map.Monoidal (type (==>), (==>), (!?))
import Data.Foldable
import Data.Traversable
import qualified Data.Map as Map
import Pretty
import Data.Function ((&))
import qualified Data.Map.Monoidal as Monoidal

{- |
  Parser automata.
-}
type Table = Map Int (TransitionsTo Int)

data Cache = Cache
  { table    :: Table
  , states   :: Map Int LR1State
  , cores    :: Set LR0Item ==> Set Int -- ^ for PGM compression
  , lastIx   :: Int
  , types    :: Map NonTerminal Text
  }

emptyCache :: Map NonTerminal Text -> Cache
emptyCache types = Cache
  { table  = mempty
  , states = mempty
  , cores  = mempty
  , lastIx = 0
  , types
  }

type M = ReaderT CachedGrammar (State Cache)

reportConflicts :: Cache -> [Doc]
reportConflicts Cache {table, states} =
  table & Map.foldMapWithKey \stId fan -> do
    fan.actions & Monoidal.foldMapWithKey \lookahead decs -> do
      if length decs < 2 then [] else
        [ hang ("Conflict (state #" <> pPrint stId <> "):") 2 do
            vcat
              [ vcat do
                  (states ! stId).items & toList & map \item -> do
                    case item.lr0item of
                      Shifts sh -> case sh.locus.symbol of
                        Term t | LookForTerm t == lookahead -> pPrint item
                        _ -> mempty
                      Reduces _ | item.lookahead == lookahead -> pPrint item
                      _ -> mempty
              ]
        , ""
        ]

{- |
  Result of search for similar state.
-}
data StateSimilarity
  = Conflicts                                    -- ^ have conflicts or generate one on merge
  | Similar LR1State Int (TransitionsTo Kernel)  -- ^ compatible, can merge them
  | Same Int                                     -- ^ they are exactly the same

{- |
  `Same`    overrides  all.
  `Similar` overrides `Conflicts`.
-}
instance Semigroup StateSimilarity where
  Conflicts        <> other   = other
  Similar _  _  _  <> Same ix = Same ix
  Similar st ix ts <> _       = Similar st ix ts
  Same       ix    <> _       = Same ix

instance Monoid StateSimilarity where
  mempty = Conflicts

{- |
  Find set if state[ID] with the same LR0-core as given one.
-}
findByCore :: LR1State -> M (Set Int)
findByCore st = do
  gets ((!? st.lr0state) . (.cores))

{- |
  Search for mergeable candidate.
-}
findCollision :: LR1State -> M StateSimilarity
findCollision st = do
  ixs <- findByCore st
  fold <$> for (toList ixs) \ix -> do
    st' <- gets ((! ix) . (.states))
    pure $ similarity (hasConflict st) st st' ix

{- |
  Calculate if two states are mergeable.

  - If they are exactly the same, the result is `Same`.
  - If either has conflict, the result is `Conflicts`.
  - If merged state is equal to right one, the result is `Same`.
    Note, that due to previous check, the conflict is impossible here.
  - If merged state has conflict, the result is `Conflicts`.
  - If none above is true, the result is `Similar`.

  /I believe/ it implements Pager's Weak Compatibility criteria.
-}
similarity
  :: Bool      -- ^ new state has any conflicts
  -> LR1State  -- ^ new state
  -> LR1State  -- ^ existing state
  -> Int       -- ^ ID of existing state
  -> StateSimilarity
similarity oneHasConflict one another ix = if
  | one == another -> Same ix
  | oneHasConflict || hasConflict another -> Conflicts
  | otherwise -> do
    let new = one <> another
    let transitions = transitionsFrom new
    if
      | new == another  -> Same ix
      | transitionsHaveConflict transitions -> Conflicts
      | otherwise       -> Similar new ix transitions

{- |
  Check if the state has internal conflict.

  Conflicts = any lookahead term has more than 1 decision
-}
hasConflict :: LR1State -> Bool
hasConflict = transitionsHaveConflict . transitionsFrom

transitionsHaveConflict :: TransitionsTo Kernel -> Bool
transitionsHaveConflict = any ((> 1) . length) . (.actions)

data Closure
  = Done         Int                        -- ^ no more work required
  | NeedUpdating Int (TransitionsTo Kernel) -- ^ there are graph edges that can be updated

{- |
  Calculate closure, attempt merge.

  Find if any more work should be done for this kernel.
-}
closureWithMerge :: Kernel -> M Closure
closureWithMerge kernel = do
  state <- asks (`closure` kernel)
  findCollision state >>= \case
    Same knownIndex -> do
      -- another state is equal to this one
      -- or /includes/ this one
      pure (Done knownIndex)

    Similar mergedState ix transitions -> do
      -- another state can be merged with this one
      setState ix mergedState
      pure (NeedUpdating ix transitions)

    Conflicts -> do
      -- this state is incompatible from all known one
      ix <- alloc state
      pure (NeedUpdating ix (transitionsFrom state))

{- |
  Allocate state with new ID, update global ID counter.
-}
alloc :: LR1State -> M Int
alloc st = do
  ix <- gets (.lastIx)
  modify' \cache -> cache {lastIx = cache.lastIx + 1}
  setState ix st
  pure ix

{- |
  Allocate state with new ID, update global ID counter.

  Replaces state at given index with a new one.
  Registers the state's LR(0)-core with given ID.
-}
setState :: Int -> LR1State -> M ()
setState ix st = modify' \cache -> cache
  { states   = Map.insert ix st cache.states
  , cores    = (st.lr0state ==> [ix]) <> cache.cores
  }

{- |
  Build parsing automata, recursively, starting from a kernel.
-}
buildGraph :: Kernel -> M Int
buildGraph kernel = do
  closureWithMerge kernel >>= \case
    Done ix -> do
      -- nothing need to be done
      pure ix

    NeedUpdating ix transitions -> do
      -- recusively build or update outcoming edges of the graph
      branch <- traverseTransitionsTo buildGraph transitions
      addTable ix branch
      pure ix

addTable :: Int -> TransitionsTo Int -> M ()
addTable st fan = do
  modify' \cache -> cache
    { table = Map.insertWith (<>) st fan cache.table
    }

buildTable :: CachedGrammar -> Kernel -> (Int, Cache)
buildTable cache kernel =
  flip runState (emptyCache cache.types) do
    flip runReaderT cache do
      buildGraph kernel
