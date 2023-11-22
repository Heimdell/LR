
module Point.Instances.IsString where

import Data.Char   (isUpper)
import Data.String (IsString (..))

import Point.Base

instance IsString Entity where
  fromString = MkEntity . fromString

instance IsString Term where
  fromString = MkTerm . fromString

instance IsString Point where
  fromString cs
    | isUpper (head cs) = Entity (fromString cs)
    | otherwise         = Term   (fromString cs)