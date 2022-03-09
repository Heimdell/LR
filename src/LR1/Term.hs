module LR1.Term where

import LR1.Lexeme qualified as Lexeme
import Data.String (IsString (fromString))
import GHC.Generics (Generic)

data T
  = Term Lexeme.T
  | EndOfStream
  deriving stock (Eq, Ord, Generic)

instance Show LR1.Term.T where
  show = \case
    Term le -> show le
    EndOfStream -> "$"

instance IsString LR1.Term.T where
  fromString = Term . fromString
