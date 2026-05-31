module Data.Map.Monoidal.Pretty where

import Data.Map.Monoidal.Structure
import Text.PrettyPrint.HughesPJClass
import Data.Function ((&))

instance (Pretty k, Pretty v) => Pretty (k ==> v) where
  pPrint monoidal =
    monoidal
      & assocs
      & map do \(k, v) -> hang (pPrint k <+> "=>") 2 (pPrint v)
      & vcat

instance (Pretty k, Pretty v) => Show (k ==> v) where
  show = show . pPrint