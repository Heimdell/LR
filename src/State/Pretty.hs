module State.Pretty where

import State.Structure
import Text.PrettyPrint.HughesPJClass
import Position (groupPositionsByPrefices)
import Data.Foldable (toList)
import Data.Function ((&))


instance Pretty State where
  -- pPrint State {kernel} =
  --   kernel
  pPrint State {positions} =
    positions
      & groupPositionsByPrefices
      & toList
      & map pPrint
      & vcat
