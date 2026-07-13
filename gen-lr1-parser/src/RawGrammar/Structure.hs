module RawGrammar.Structure where

import Rule
import Symbol
import Data.Set (Set)

data Grammar = Grammar
  { starts :: Set NonTerminal
  , rules  :: [Rule]
  }
