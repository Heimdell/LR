module Rule.Pretty where

import Data.Foldable                  (toList)
import Text.PrettyPrint.HughesPJClass

import Rule.Structure

instance Pretty Rule where
  pPrint Rule {entity, type_, clauses} =
    hang (pPrint entity <+> ":" <+> pPrint type_) 2 do
      vcat do
        zipWith (\c cls -> text c <+> pPrint cls) ("=" : repeat "|") clauses

instance Pretty Clause where
  pPrint Clause {points, reducer} =
    hang
      (hang
        (fsep (map pPrint (toList points)) <+> "{")
        2
        (pPrint reducer)
      )
      0
      "}"
