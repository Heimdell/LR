module Position (module M) where

import Position.Structure as M
  ( Position(..)
  , groupPositionsByCurrentPoints
  , start
  , lookaheadAfterCurrentPoint
  , startingPosition
  )
import Position.Pretty as M
  ( PrettyPosition(..)
  , groupPositionsByPrefices
  )
