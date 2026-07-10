module Decision.Pretty where

import Text.PrettyPrint.HughesPJClass (hang, Pretty(pPrint))

import Decision.Structure (Decision(..))

instance (Pretty state) => Pretty (Decision state) where
  pPrint = \case
    Shift    st   -> hang "Shift"  2 (pPrint st)
    Reduce _ rule -> hang "Reduce" 2 (pPrint rule)
    Accept        -> "A-C-C-E-P-T"

instance (Pretty state) => Show (Decision state) where
  show = show . pPrint
