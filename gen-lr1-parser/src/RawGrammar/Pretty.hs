module RawGrammar.Pretty where

import Data.Function                  ((&))
import Text.PrettyPrint.HughesPJClass (vcat, Pretty(pPrint))

import RawGrammar.Structure

instance Pretty Grammar where
  pPrint Grammar {rules} =
    rules
      & fmap pPrint
      & vcat

instance Show Grammar where show = show . pPrint
