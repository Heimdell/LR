
module Tree where

import Name
import Pretty

data Tree token
  = Leaf token
  | Join Name [Tree token]
  deriving Show via PP (Tree token)

instance Pretty token => Pretty (Tree token) where
  pretty = \case
    Leaf token -> pretty token
    Join n ts -> "(" <.> color 5 (pretty n) <+> fsep (map pretty ts) <.> ")"
