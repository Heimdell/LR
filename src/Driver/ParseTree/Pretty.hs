module Driver.ParseTree.Pretty where

import Driver.ParseTree.Structure
import Text.PrettyPrint.HughesPJClass
import qualified Data.Text as Text

instance Pretty a => Pretty (Tree a) where
  pPrint = \case
    Atom a -> pPrint a
    Reduced ctor forest -> parens (hang (text (Text.unpack ctor)) 2 (vcat (map pPrint forest)))

instance Pretty a => Show (Tree a) where
  show = show . pPrint