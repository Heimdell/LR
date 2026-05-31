module Grammar.Example where

import Grammar.Structure  (Grammar, makeGrammar)
import Rule               (Rule(Rule))
import Term               (Point(..))
import Data.List.NonEmpty (NonEmpty((:|)))

grammarTest :: Grammar
grammarTest = makeGrammar
  [ Rule "S" 0 (E "E" :| []) "Stop"
  , Rule "E" 0 (E "E" :| T "+" : E "F" : []) "Add"
  , Rule "E" 0 (E "E" :| T "-" : E "F" : []) "Subtract"
  , Rule "E" 0 (E "F" :| []) "F->E"
  , Rule "F" 0 (E "F" :| T "*" : E "T" : []) "Mult"
  , Rule "F" 0 (E "F" :| T "/" : E "T" : []) "Divide"
  , Rule "F" 0 (E "T" :| []) "T->F"
  , Rule "T" 0 (T "number" :| []) "Number"
  , Rule "T" 0 (T "(" :| E "E" : T ")" : []) "Group"
  ]
