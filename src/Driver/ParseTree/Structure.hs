module Driver.ParseTree.Structure where

import Data.Text (Text)

data Tree a
  = Atom a
  | Reduced Text [Tree a]
