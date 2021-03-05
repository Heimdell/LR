
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
    Leaf token -> color 2 $ pretty token
    Join _ [t] -> pretty t
    Join _ ts -> "(" <.> fsep (map pretty ts) <.> ")"
