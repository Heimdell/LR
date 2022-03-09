{-# OPTIONS_GHC -Wno-orphans #-}
module LR1.State where

import qualified LR1.Item as Item
import Data.Set (Set)
import Data.Function (on, (&))
import LR1.Fixpoint (fixpoint, one, Get ((?)))
import qualified Control.Monad.State as MTL
import qualified LR1.Grammar as Grammar
import qualified LR1.FIRST as FIRST
import qualified Data.Set as Set
import qualified LR1.Point as Point
-- import Control.Lens (makeLenses, uses, use, (+=))
-- import Control.Lens.Operators ((%=))
import qualified LR1.Map as Map
import Data.List (groupBy, sortOn)
import LR1.Item (T(lookahead))
import qualified LR1.Term as Term
import Control.Arrow ((&&&))
import GHC.Generics (Generic)

type Index = Int

deriving stock instance Generic Int

data T = State
  { index  :: Index
  , items  :: Set Item.T
  , kernel :: Set Item.T
  }

instance Eq LR1.State.T where
  (==) = (==) `on` items

instance Ord LR1.State.T where
  compare = compare `on` items

instance Show LR1.State.T where
  show State { kernel = set, index } =
    "(" <> show index <> ")\n"
      <> foldMap (\item -> "  " <> show item <> "\n") set

data Reg = Reg
  { states  :: Map.T LR1.State.T Index
  , indices :: Map.T Index LR1.State.T
  , counter :: Index
  }

emptyReg :: Reg
emptyReg = Reg
  { states  = Map.empty
  , indices = Map.empty
  , counter = 0
  }

-- makeLenses ''Reg

type HasReg m = MTL.MonadState Reg m

closure :: HasReg m => Grammar.T -> FIRST.T -> Set Item.T -> m (LR1.State.Index, Bool)
closure grammar first items = do
  let
    state = fixpoint items do
      foldMap do
        \item1 -> case Item.locus item1 of
          Nothing -> mempty
          Just (Point.Term _) -> mempty
          Just (Point.NonTerm entity) -> do
            let
              localahead = case Item.next item1 >>= Item.locus of
                Nothing -> Item.lookahead item1
                Just (Point.Term term) -> one term
                Just (Point.NonTerm nextEntity) -> first ? nextEntity

            grammar ? entity & Set.map (Item.start localahead)

  register (State (error "state index is not set") (normalize state) items)

firstState :: HasReg m => Grammar.T -> FIRST.T -> m Index
firstState grammar first =
  fst <$> closure grammar first
    ( one
    $ Item.start (one Term.EndOfStream)
    $ Grammar.firstRule grammar
    )

normalize :: Set Item.T -> Set Item.T
normalize items =
  items
    & Set.toList
    & groupBy ((==) `on` (Item.entity &&& Item.label &&& Item.pos))
    & fmap do \list@(item : _) -> item { lookahead = Set.unions $ fmap Item.lookahead list }
    & Set.fromList

register :: HasReg m => LR1.State.T -> m (Index, Bool)
register state = do
  MTL.gets (Map.lookup state . states) >>= \case
    Nothing -> do
      index <- MTL.gets counter
      let state1 = state { index }
      MTL.modify \Reg {states, indices, counter} -> Reg
        { states  = Map.insert state1 index states
        , indices = Map.insert index state1 indices
        , counter = 1 + counter
        }
      return (index, True)

    Just n -> do
      return (n, False)

instance Show Reg where
  show Reg {states} = do
    Map.keys states
      & sortOn index
      & fmap show
      & unlines