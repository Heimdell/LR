module Decision where

import Rule
import Pretty

{- |
  Decision on terminal.
-}
data Decision state
  = Accept
  | Reduce Rule
  | Shift  state
  deriving stock (Eq, Ord, Functor, Foldable, Traversable)
  deriving (Show) via PP (Decision state)

instance Pretty state => Pretty (Decision state) where
  pPrint = \case
    Accept      -> "ACCEPT"
    Reduce rule -> "Reduce" <+> pPrint rule
    Shift  st   -> "Shift " <+> pPrint st
