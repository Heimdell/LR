
-- | A wrapper for `String`, so I can make it pretty.
--
module S where

import Data.String (IsString (..))

import Pretty

-- | A `String`.
newtype S = S { unS :: String }
  deriving newtype (Eq, Ord)
  deriving Show via PP S

instance IsString S where
  fromString = S

instance Pretty S where
  pretty = text . unS
