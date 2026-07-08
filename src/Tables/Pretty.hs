module Tables.Pretty where

import Data.Foldable                  (toList)
import Data.Function                  ((&))
import Text.PrettyPrint.HughesPJClass (hang, punctuate, vcat, Pretty(pPrint))

import Tables.Structure (Action(..), Table(..))

import Data.Map.Monoidal qualified as Map

instance (Pretty state) => Pretty (Action state) where
  pPrint Action {goto, action} = vcat
    [ goto
        & Map.assocs
        & map (\(node, fan) -> hang (pPrint node) 2 (pPrint fan))
        & punctuate "\n" & vcat
    , "   "
    , action
        & Map.assocs
        & map (\(node, fan) -> hang (pPrint node) 2 (vcat (map pPrint (toList fan))))
        & punctuate "\n" & vcat
    ]

instance (Pretty state) => Pretty (Table state) where
  pPrint Table {actions} = pPrint actions
