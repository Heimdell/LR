
module Point where

import Data.Char   (isUpper)
import Data.String (IsString(..))

import Name
import Pretty

data Point term
  = Term    term
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

elimPoint :: (term -> c) -> (Name -> c) -> Point term -> c
elimPoint fromTerm fromNonTerm = \case
  Term    term -> fromTerm    term
  NonTerm name -> fromNonTerm name
