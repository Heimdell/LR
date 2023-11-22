
module Position.Instances where

import Control.Monad (guard)
import Data.Set qualified as Set
import Data.Hashable
import Data.Function

import Rule
import Point
import Colored
import Position.Base

instance Show Position where
  show pos = concat
    [ show pos.rule.entity
    , magenta " = "
    , unwords (map show (take pos.offset pos.rule.points))
    , red " . "
    , unwords (map show (drop pos.offset pos.rule.points))
    , magenta "         {"
    , show pos.lookahead
    , magenta "}"
    ]

instance Show (Position_ (Set.Set Term)) where
  show pos = concat
    [ show pos.rule.entity
    , magenta " = "
    , unwords (map show (take pos.offset pos.rule.points))
    , red " . "
    , unwords (map show (drop pos.offset pos.rule.points))
    , magenta "         {"
    , unwords (map show (Set.toList pos.lookahead))
    , magenta "}"
    ]