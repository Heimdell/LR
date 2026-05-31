module State.Pretty where

import Data.Foldable                  (toList)
import Data.Function                  ((&))
import Text.PrettyPrint.HughesPJClass (vcat, Pretty(pPrint))

import Position        (groupPositionsByPrefices)
import State.Structure (State(positions, State))

instance Pretty State where
  -- pPrint State {kernel} =
  --   kernel
  pPrint State {positions} =
    positions
      & groupPositionsByPrefices
      & toList
      & map pPrint
      & vcat
