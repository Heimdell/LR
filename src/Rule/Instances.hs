
module Rule.Instances where

import Data.String
import Point
import Colored
import Data.Hashable
import Data.Function

import Rule.Base

instance Show Rule where
  show rule = concat
    [ blue (show rule.mark)
    , magenta " . "
    , show rule.entity
    , magenta " = "
    , unwords (map show rule.points)
    ]

instance IsString Rule where
  fromString src
    | m : "." : e : "=" : points <- words src
    = mkRule (fromString e) (map fromString points) (read m)

instance Ord Rule where compare = compare `on` (.hash)
instance Eq  Rule where (==)    = (==)    `on` (.hash)
