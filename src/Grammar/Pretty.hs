module Grammar.Pretty where

import Data.Foldable                  (toList)
import Data.Function                  ((&))
import Data.Set                       (Set)
import Text.PrettyPrint.HughesPJClass (vcat, Doc, Pretty(pPrint))

import Grammar.Structure (Grammar(rules, Grammar))
import Rule              (Rule)
import Term              (Entity)

import qualified Data.Map.Monoidal as Map

instance Pretty Grammar where
  pPrint Grammar {rules} =
    rules
      & Map.assocs
      & fmap ruleBlock
      & vcat
    where
      ruleBlock :: (Entity, Set Rule) -> Doc
      ruleBlock (_, ruleset) =
        vcat (map pPrint (toList ruleset))
