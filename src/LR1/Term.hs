module LR1.Term where

import Data.String (IsString (fromString))
import GHC.Generics (Generic)

import Data.Text (Text)

data T
  = Term Text
  | EndOfStream
  deriving stock (Eq, Ord, Generic)

instance Show LR1.Term.T where
  show = \case
    Term le -> show le
    EndOfStream -> "$"

instance IsString LR1.Term.T where
  fromString = Term . fromString
