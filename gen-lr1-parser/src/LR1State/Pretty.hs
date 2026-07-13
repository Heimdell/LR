module LR1State.Pretty where

import Data.Foldable                  (toList)
import Data.Function                  ((&))
import Text.PrettyPrint.HughesPJClass (vcat, Pretty(pPrint))

-- import Position        (groupPositionsByPrefices)
import LR1State.Structure (LR1State(..))

instance Pretty LR1State where
  pPrint LR1State {kernel} =
    kernel
  -- pPrint LR1State {positions} =
  --   positions
      -- & groupPositionsByPrefices
      & toList
      & map pPrint
      & vcat
