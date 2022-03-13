
{- |
  A nonterminal - or "entity".
-}
module LR1.NonTerm where

import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text

{- |
  A nonterminal.
-}
data T
  = NonTerm Text  -- ^ Named one.
  | Start         -- ^ Starting one.
  deriving stock (Eq, Ord)

instance Show LR1.NonTerm.T where
  show = \case
    NonTerm txt -> Text.unpack txt
    Start -> "S"

instance IsString LR1.NonTerm.T where
  fromString = NonTerm . fromString
