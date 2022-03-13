{- |
  A terminal, represents a class of one atomic input fragment.
-}
module LR1.Term where

import Data.String (IsString (fromString))
import Data.Text (Text)
import GHC.Generics (Generic)

{- |
  A terminal.
-}
data T
  = Term Text    -- ^ A terminal (token class) with name.
  | EndOfStream  -- ^ A terminal representing "end of input" situation.
  deriving stock (Eq, Ord, Generic)

instance Show LR1.Term.T where
  show = \case
    Term le -> show le
    EndOfStream -> "$"

instance IsString LR1.Term.T where
  fromString = Term . fromString
