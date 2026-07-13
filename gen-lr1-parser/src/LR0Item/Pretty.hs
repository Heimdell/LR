module LR0Item.Pretty where

import Data.Foldable                  (toList)
import Text.PrettyPrint.HughesPJClass (Pretty, pPrint, (<+>), fsep)

import LR0Item.Structure
import Rule

instance Pretty LR0Item where
  pPrint LR0Item {entity, clause, offset} =
    pPrint entity <+> "=" <+> fsep (map pPrint front) <+> case rest of
      [] -> "reducing"
      locus : other ->
        ("[" <> pPrint locus <> "]")
          <+> fsep (map pPrint other)
    where
      (front, rest) = splitAt offset (toList clause.points)
