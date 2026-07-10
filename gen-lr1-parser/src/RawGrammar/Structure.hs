module RawGrammar.Structure where

import Rule
import Term

data Grammar = Grammar
  { starter   :: Entity
  , ruleOrder :: [Rule]
  }
