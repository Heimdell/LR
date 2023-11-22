
module Rule.Base where

import Data.String
import Point
import Colored
import Data.Hashable
import Data.Function

data Rule = MkRule
  { entity :: Entity
  , points :: [Point]
  , mark   :: Int
  , hash   :: Int
  }

mkRule :: Entity -> [Point] -> Int -> Rule
mkRule entity points mark = MkRule
  { entity
  , points
  , mark
  , hash = hash entity `hashWithSalt` points `hashWithSalt` mark
  }

firstPoint :: Rule -> Point
firstPoint rule = head rule.points