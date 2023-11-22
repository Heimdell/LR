
module Goto.Instances where

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
import Goto.Base

instance Show Goto where
  show goto = goto.table
    & Map.toList
    & fmap do \(v, pv) -> show v <> indent (showPV pv)
    & unlines
    where
      showPV :: Map.Map Point Vertex -> String
      showPV pv = pv
        & Map.toList
        & fmap do \(p, v') -> magenta "<" <> show p <> magenta ">" <> "\n" <> indent (show v')
        & unlines

instance Show Instr where
  show = \case
    Shift v    -> green "SHIFT\n" ++ indent (show v)
    Reduce r   -> blue "REDUCE " <> show r
    Accept     -> yellow "ACCEPT"
    Conflict s -> red "CONFLICT\n" <> indent (unlines $ fmap show $ Set.toList s)

instance Show Action where
  show goto = goto.table
    & Map.toList
    & fmap do \(v, pv) -> show v <> indent (showPV pv) <> "\n"
    & unlines
    where
      showPV :: Map.Map Term Instr -> String
      showPV pv = pv
        & Map.toList
        & fmap do \(p, v') -> magenta "<" <> show p <> magenta ">" <> "\n" <> indent (show v')
        & unlines
