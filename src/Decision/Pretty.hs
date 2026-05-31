module Decision.Pretty where

import Text.PrettyPrint.HughesPJClass (hang, Pretty(pPrint))

import Decision.Structure (Decision(..))

instance Pretty Decision where
  pPrint = \case
    Shift  st   -> hang "Shift"  2 (pPrint st)
    Reduce rule -> hang "Reduce" 2 (pPrint rule)
    Accept      -> "A-C-C-E-P-T"

instance Show Decision where
  show = show . pPrint
