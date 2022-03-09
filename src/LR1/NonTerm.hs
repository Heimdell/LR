module LR1.NonTerm where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.String (IsString (fromString))

data T
  = NonTerm Text
  | Start
  deriving stock (Eq, Ord)

instance Show LR1.NonTerm.T where
  show = \case
    NonTerm txt -> Text.unpack txt
    Start -> "S"

instance IsString LR1.NonTerm.T where
  fromString = NonTerm . fromString
