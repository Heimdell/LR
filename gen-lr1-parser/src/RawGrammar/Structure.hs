module RawGrammar.Structure where

import Rule
import Term
import Data.Set (Set)

data Grammar = Grammar
  { starts :: Set Entity
  , rules  :: [Rule]
  }
