
module State.Instances where

import Control.Monad.State
import Control.Monad.Reader
import Data.Set qualified as Set
import Data.Hashable
import Data.Function
import Data.List

import Grammar
import First
import Point
import Position
import Data.Map.Monoidal qualified as Map
import Monoidal (show', search)
import State.Base

instance Show Vertex where
  show MkVertex {closure, kernel} = kernel.set
    & Set.toList
    & sortOn do \pos -> (pos.rule, pos.offset)
    & groupBy do (==) `on` \pos -> (pos.rule, pos.offset)
    & fmap do \(pos : rest) -> pos { lookahead = Set.insert pos.lookahead (foldMap (Set.singleton . (.lookahead)) rest) }
    & fmap show
    & unlines