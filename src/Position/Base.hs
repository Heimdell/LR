
module Position.Base where

import Control.Monad (guard)
import Data.Set qualified as Set
import Data.Hashable
import Data.Function

import Rule
import Point
import Colored

data Position_ term = MkPosition
  { rule      :: Rule
  , offset    :: Int
  , lookahead :: term
  , hash      :: Int
  }

instance Eq  Position where (==)    = (==)    `on` (.hash)
instance Ord Position where compare = compare `on` (.hash)


mkPosition :: Rule -> Int -> Term -> Position
mkPosition rule offset lookahead = MkPosition
  { rule
  , offset
  , lookahead
  , hash = rule.hash `hashWithSalt` hash offset `hashWithSalt` lookahead
  }

type Position = Position_ Term

start :: Rule -> Term -> Position
start rule lookahead = mkPosition rule 0 lookahead

productive :: Position -> Bool
productive pos = pos.offset < length pos.rule.points

locus :: Position -> Maybe Point
locus pos = do
  guard (productive pos)
  return (pos.rule.points !! pos.offset)

next :: Position -> Maybe Position
next pos = do
  guard (productive pos)
  return $ mkPosition pos.rule (pos.offset + 1) pos.lookahead

startAll :: Set.Set Rule -> Set.Set Term -> Set.Set Position
startAll rules terms =
  rules & foldMap \rule ->
  terms & Set.map \t ->
    start rule t
