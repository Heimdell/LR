module Grammar.Example where

import Grammar.Structure (Grammar, makeGrammar)
import Rule              (mkRule)
import Term              (Point(..))

grammarTest :: Grammar
grammarTest = makeGrammar
  [ mkRule "S" [E "E"] "Stop"
  , mkRule "E" [E "E", T "+", E "F"] "Add"
  , mkRule "E" [E "E", T "-", E "F"] "Subtract"
  , mkRule "E" [E "F"] "F->E"
  , mkRule "F" [E "F", T "*", E "T"] "Mult"
  , mkRule "F" [E "F", T "/", E "T"] "Divide"
  , mkRule "F" [E "T"] "T->F"
  , mkRule "T" [T "number"] "Number"
  , mkRule "T" [T "(", E "E", T ")"] "Group"
  ]
