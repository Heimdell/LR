{- |
  A position in the rule.

  It can either point to some element of the rule, or be a "reducing" position,
  that points right beyond the end of the rule.

  It is a specialized zipper over a list of points.
-}
module LR1.Position where

import Data.List (uncons)

import LR1.Point qualified as Point
import LR1.Rule  qualified as Rule

{- |
  A position in the rule.
-}
data T = Position
  { before :: []    Point.T  -- ^ All points already parsed, in reverse.
  , locus  :: Maybe Point.T  -- ^ Possibly, current point.
  , after  :: []    Point.T  -- ^ All points yet to be parsed, after the locus.
  }
  deriving stock (Eq, Ord)

instance Show LR1.Position.T where
  show Position {before, locus, after} =
    unwords (show <$> reverse before)
      <> maybe " [] " (\term -> " [" <> show term <> "] ") locus
      <> unwords (show <$> after)

{- |
  Get starting position of the rule, pointing at first point.
-}
start :: Rule.T -> LR1.Position.T
start Rule.Rule {points = initial : rest} = Position
  { before = []
  , locus  = Just initial
  , after  = rest
  }
start Rule.Rule {entity, label} =
  error $ "Rule of " <> show entity <> " with label " <> show label <> " is empty"

{- |
  Move to the next position, if possible.
-}
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

{- |
  Length of the underlying rule.
-}
len :: LR1.Position.T -> Int
len Position {before, locus, after} = length before + length after + maybe 0 (const 1) locus
