module RawGrammar.Pretty where

import Data.Function                  ((&))
import Text.PrettyPrint.HughesPJClass (vcat, Pretty(pPrint))

import RawGrammar.Structure (Grammar(ruleOrder, Grammar))

instance Pretty Grammar where
  pPrint Grammar {ruleOrder} =
    ruleOrder
      & fmap pPrint
      & vcat

instance Show Grammar where show = show . pPrint
