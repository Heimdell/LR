
module Name where

import Data.String (IsString (..))

import Pretty

data Name = Name { raw :: String } | Start
  deriving stock (Eq, Ord)
  deriving Show via PP Name

instance IsString Name where
  fromString = Name

instance Pretty Name where
  pretty = \case
    Start    -> color 5 $ text "S'"
    Name raw -> color 3 $ text raw

