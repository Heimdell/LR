
-- | A `Point` is a terminal or a non-terminal as a single entity.
--
module Point where

import Data.Char   (isUpper)
import Data.String (IsString(..))

import Name
import Pretty

-- | A point.
--
data Point term
  = -- | Terminal
    Term    term

    -- | Non-terminal
  | NonTerm Name
  deriving stock (Eq, Ord)
  deriving Show via PP (Point term)

instance IsString term => IsString (Point term) where
  fromString str@(fstChar : _)
    | isUpper fstChar = NonTerm (Name       str)
    | otherwise       = Term    (fromString str)

  fromString [] = error "Point.fromString: empty string is not allowed"

instance Pretty term => Pretty (Point term) where
  pretty = \case
    Term    term -> pretty term
    NonTerm name -> pretty name
