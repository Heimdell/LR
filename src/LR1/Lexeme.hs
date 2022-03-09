module LR1.Lexeme where

import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text

data T
  = Concrete Text
  | Category Text
  deriving stock (Eq, Ord)

instance Show LR1.Lexeme.T where
  show = \case
    Concrete txt -> Text.unpack txt
    Category txt -> "<" <> Text.unpack txt <> ">"

instance IsString LR1.Lexeme.T where
  fromString = Concrete . fromString

cat :: Text -> LR1.Lexeme.T
cat = Category
