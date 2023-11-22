
module Goto.Base where

import Data.Set qualified as Set
import Data.Traversable
import Data.Function
import Data.Bifunctor (second)

import State
import Rule
import Point
import Position
import Data.Map.Monoidal qualified as Map
import Monoidal (search'', show')
import Colored

data Goto = MkGoto { table :: Map.Map Vertex (Map.Map Point Vertex) }

goto :: CacheM Goto
goto = do
  start <- initial
  MkGoto <$> search'' adjacent start
  where
    adjacent :: Vertex -> CacheM (Map.Map Point Vertex)
    adjacent v = do
      let points = expected v
      pairs <- for (Set.toList points) \p -> do
        v' <- nextOn v p
        return (p, v')

      return (Map.fromList pairs)

data Instr
  = Shift Vertex
  | Reduce Rule
  | Accept
  | Conflict (Set.Set Instr)
  deriving stock (Eq, Ord)

instance Semigroup Instr where
  l           <> r           | l == r = l
  Conflict ls <> Conflict rs          = Conflict (ls <> rs)
  Conflict ls <> r                    = Conflict (Set.insert r ls)
  l           <> Conflict rs          = Conflict (Set.insert l rs)
  l           <> r                    = Conflict (Set.fromList [l, r])

instance Monoid Instr where
  mempty = Conflict mempty

isConflict :: Instr -> Bool
isConflict Conflict {} = True
isConflict _           = False

data Action = MkAction { table :: Map.Map Vertex (Map.Map Term Instr) }

action :: Goto -> Action
action goto = MkAction
  $ Map.memoise (Map.keys goto.table) reduces <>
    shifts goto
  where
    reduces :: Vertex -> Map.Map Term Instr
    reduces st =
      st.closure & foldMap \pos ->
        case locus pos of
          Just _  -> mempty
          Nothing ->
            Map.singleton pos.lookahead
              if pos.rule.entity == "S"
              then Accept
              else Reduce pos.rule

    shifts :: Goto -> Map.Map Vertex (Map.Map Term Instr)
    shifts goto = goto.table
      & Map.map do
          \fan -> Map.toList fan & foldMap \case
            (Term t, v) -> Map.singleton t (Shift v)
            _           -> mempty

conflicts :: Action -> Action
conflicts (MkAction action) = action
  & Map.map (Map.filter isConflict)
  & Map.filter (not . null)
  & MkAction