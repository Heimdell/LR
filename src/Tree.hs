
-- | A standard S-expr.
module Tree where

import Name
import Pretty

-- | S-expr tree.
data Tree token
  = -- | atom
    Leaf token

    -- | branch
  | Join Name [Tree token]
  deriving Show via PP (Tree token)

instance Pretty token => Pretty (Tree token) where
  pretty = \case
    Leaf token -> pretty token
    Join (Name ('-' : _)) [t] -> pretty t
    Join n ts -> "(" <.> color 5 (pretty n) <+> fsep (map pretty ts) <.> ")"
