module LR1.Point where

import Data.String (IsString (fromString))
import Data.Text (Text)

import LR1.Lexeme  qualified as Lexeme
import LR1.NonTerm qualified as NonTerm
import LR1.Term    qualified as Term

data T
  = Term Term.T
  | NonTerm NonTerm.T
  deriving stock (Eq, Ord)

instance Show LR1.Point.T where
  show = \case
    Term    te -> show te
    NonTerm nt -> show nt

instance IsString LR1.Point.T where
  fromString = Term . fromString

e :: Text -> LR1.Point.T
e = NonTerm . NonTerm.NonTerm

cat :: Text -> LR1.Point.T
cat = Term . Term.Term . Lexeme.Category

isEntity :: LR1.Point.T -> Bool
isEntity NonTerm {} = True
isEntity _          = False
