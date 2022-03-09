module LR1.Position where

import Data.List (uncons)

import LR1.Point qualified as Point
import LR1.Rule  qualified as Rule

data T = Position
  { before :: []    Point.T
  , locus  :: Maybe Point.T
  , after  :: []    Point.T
  }
  deriving stock (Eq, Ord)

instance Show LR1.Position.T where
  show Position {before, locus, after} =
    unwords (show <$> reverse before)
      <> maybe " [] " (\term -> " [" <> show term <> "] ") locus
      <> unwords (show <$> after)

start :: Rule.T -> LR1.Position.T
start Rule.Rule {points = initial : rest} = Position
  { before = []
  , locus  = Just initial
  , after  = rest
  }
start Rule.Rule {entity, label} =
  error $ "Rule of " <> show entity <> " with label " <> show label <> " is empty"

next :: LR1.Position.T -> Maybe LR1.Position.T
next Position { before, locus, after } = case (locus, uncons after) of
  (Just loc, Just (nextLoc, rest)) -> Just Position
    { before = loc : before
    , locus  = Just nextLoc
    , after  = rest
    }

  (Just loc, Nothing) -> Just Position
    { before = loc : before
    , locus  = Nothing
    , after  = []
    }

  _ -> Nothing

len :: LR1.Position.T -> Int
len Position {before, locus, after} = length before + length after + maybe 0 (const 1) locus
