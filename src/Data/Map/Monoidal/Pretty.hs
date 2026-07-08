module Data.Map.Monoidal.Pretty where

import Data.Foldable                  (toList)
import Data.Function                  ((&))
import Text.PrettyPrint.HughesPJClass ((<+>), hang, vcat, Pretty(pPrint))
import Data.Set                       (Set)

import Data.Map.Monoidal.Structure (type (==>), assocs)
import Data.Map (Map)
import qualified Data.Map as Map

instance (Pretty k, Pretty v) => Pretty (k ==> v) where
  pPrint monoidal =
    monoidal
      & assocs
      & map do \(k, v) -> hang (pPrint k <+> "=>") 2 (pPrint v)
      & vcat

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pPrint monoidal =
    monoidal
      & Map.assocs
      & map do \(k, v) -> hang (pPrint k <+> "=>") 2 (pPrint v)
      & vcat

instance (Pretty k, Pretty v) => Show (k ==> v) where
  show = show . pPrint

instance (Pretty k) => Pretty (Set k) where
  pPrint s = s
    & toList
    & map pPrint
    & vcat
