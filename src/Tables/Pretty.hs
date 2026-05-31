module Tables.Pretty where

import Text.PrettyPrint.HughesPJClass (hang, punctuate, vcat, Pretty(pPrint))
import Data.Function                  ((&))
import Position.Pretty                (groupPositionsByPrefices)
import Data.Foldable                  (toList)
import Data.Map.Monoidal qualified as Map

import Tables.Structure

instance Pretty State where
  -- pPrint State {kernel} =
  --   kernel
  pPrint State {positions} =
    positions
      & groupPositionsByPrefices
      & toList
      & map pPrint
      & vcat

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

instance Pretty Decision where
  pPrint = \case
    Shift st -> hang "Shift" 2 (pPrint st)
    Reduce rule -> hang "Reduce" 2 (pPrint rule)
    -- Conflict decs -> hang "CONFLICT" 2 (vcat (map pPrint (toList decs)))
    Accept -> "A-C-C-E-P-T"

instance Show Decision where
  show = show . pPrint