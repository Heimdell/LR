module Grammar.Pretty where

import Text.PrettyPrint.HughesPJClass ( vcat, Doc, Pretty(pPrint) )
import Data.Function ((&))
import Data.Foldable (toList)

import Grammar.Structure
import Term (Entity)
import Data.Set (Set)
import Rule (Rule)
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
