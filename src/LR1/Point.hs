{- |
  Atomic element of the rule - terminal or non-terminal.
-}
module LR1.Point where

import Data.String (IsString (fromString))

import LR1.Term    qualified as Term
import Data.Text (Text)

{- |
  Element of the rule.
-}
data T
  = Term    Term.T     -- ^ A terminal.
  | NonTerm Text  -- ^ A non-terminal (or "entity").
  deriving stock (Eq, Ord)

instance Show LR1.Point.T where
  show = \case
    Term    te -> show te
    NonTerm nt -> show nt

instance IsString LR1.Point.T where
  fromString = Term . fromString
