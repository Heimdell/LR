module Decision.Pretty where

import Decision.Structure ( Decision(..) )
import Text.PrettyPrint.HughesPJClass ( hang, Pretty(pPrint) )

instance Pretty Decision where
  pPrint = \case
    Shift st -> hang "Shift" 2 (pPrint st)
    Reduce rule -> hang "Reduce" 2 (pPrint rule)
    -- Conflict decs -> hang "CONFLICT" 2 (vcat (map pPrint (toList decs)))
    Accept -> "A-C-C-E-P-T"

instance Show Decision where
  show = show . pPrint
