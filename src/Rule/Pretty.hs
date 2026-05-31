module Rule.Pretty where

import Data.Foldable                  (toList)
import Text.PrettyPrint.HughesPJClass ((<+>), fsep, Pretty(pPrint))

import Rule.Structure (Rule(points, Rule, entity))

instance Pretty Rule where
  pPrint Rule {entity, points} =
    pPrint entity <+> "=" <+> fsep (map pPrint (toList points))
