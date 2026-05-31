module Data.Map.Monoidal.Pretty where

import Data.Function                  ((&))
import Text.PrettyPrint.HughesPJClass ((<+>), hang, vcat, Pretty(pPrint))

import Data.Map.Monoidal.Structure (type (==>), assocs)

instance (Pretty k, Pretty v) => Pretty (k ==> v) where
  pPrint monoidal =
    monoidal
      & assocs
      & map do \(k, v) -> hang (pPrint k <+> "=>") 2 (pPrint v)
      & vcat

instance (Pretty k, Pretty v) => Show (k ==> v) where
  show = show . pPrint
