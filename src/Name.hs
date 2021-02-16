
-- | A name for entity.
--
module Name where

import Data.String (IsString (..))

import Pretty

-- | A name for entity.
--
data Name
  = -- | A name made from string.
    Name { raw :: String }

    -- | Special name for first production.
  | Start
  deriving stock (Eq, Ord)
  deriving Show via PP Name

instance IsString Name where
  fromString = Name

instance Pretty Name where
  pretty = \case
    Start    -> color 5 $ text "S'"
    Name raw -> color 3 $ text raw

