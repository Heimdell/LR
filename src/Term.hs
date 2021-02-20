
module Term where

import Data.String (IsString (..))

import Pretty

data Term t
  = Next t
  | Eof
  deriving stock (Eq, Ord)
  deriving Show via PP (Term t)

instance Pretty t => Pretty (Term t) where
  pretty = \case
    Next t -> color 2 (pretty t)
    Eof    -> color 2 "$"

instance IsString term => IsString (Term term) where
  fromString = Next . fromString