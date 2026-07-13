module RawGrammar where

import Data.Function                  ((&))
import Text.PrettyPrint.HughesPJClass (vcat, Pretty(pPrint))

import Rule
import Symbol
import Data.Set (Set)

data Grammar = Grammar
  { starts :: Set NonTerminal
  , rules  :: [Rule]
  }

instance Pretty Grammar where
  pPrint Grammar {rules} =
    rules
      & fmap pPrint
      & vcat

instance Show Grammar where show = show . pPrint
