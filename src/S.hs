
module S where

import Data.String (IsString (..))

import Pretty

newtype S = S { unS :: String }
  deriving newtype (Eq, Ord)
  deriving Show via PP S

instance IsString S where
  fromString = S

instance Pretty S where
  pretty = color 2 . text . unS
