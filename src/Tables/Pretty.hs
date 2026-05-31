module Tables.Pretty where

import Text.PrettyPrint.HughesPJClass (hang, punctuate, vcat, Pretty(pPrint))
import Data.Function                  ((&))
import Data.Foldable                  (toList)
import Data.Map.Monoidal qualified as Map

import Tables.Structure ( Action(..), Table(..) )

instance Pretty Action where
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

instance Pretty Table where
  pPrint Table {actions} = pPrint actions
