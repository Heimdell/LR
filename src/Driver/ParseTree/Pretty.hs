module Driver.ParseTree.Pretty where

import Text.PrettyPrint.HughesPJClass (hang, parens, text, vcat, Pretty(pPrint))

import Data.Text qualified as Text

import Driver.ParseTree.Structure (Tree(..))

instance Pretty a => Pretty (Tree a) where
  pPrint = \case
    Atom a -> pPrint a
    Reduced ctor forest -> parens (hang (text (Text.unpack ctor)) 2 (vcat (map pPrint forest)))

instance Pretty a => Show (Tree a) where
  show = show . pPrint
