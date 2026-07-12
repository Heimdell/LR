module RawGrammar.Structure where

import Rule
import Term
import Data.Set (Set)

data Grammar = Grammar
  { starter   :: Set Entity
  , ruleOrder :: [Rule]
  }
