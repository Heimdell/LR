module Grammar where

import Data.Foldable                  (toList)
import Data.Set                       (Set)
import Text.PrettyPrint.HughesPJClass (vcat, Doc, Pretty(pPrint))

import Symbol              (NonTerminal)

import qualified Data.Map.Monoidal as Map

import Control.Monad.State

import Data.Foldable (fold)

import Data.Array qualified as Array ((!))
import Data.Set   qualified as Set
import Data.Function

import Data.Map.Monoidal (type (==>), (!), (==>))
import Control.Fixpoint          (fixpoint)
import Rule
import Symbol              (Symbol(E, T), Terminal)

import Data.Text (Text)
import Data.Traversable (for)

data Grammar = Grammar
  { ruleOrder :: [Rule]
  , rules     :: NonTerminal ==> Set Rule
  , terminals :: Set Terminal
  , entities  :: Set NonTerminal
  , first     :: NonTerminal ==> Set Terminal
  -- , starter   :: NonTerminal
  , types     :: NonTerminal ==> Set Text
  }

makeGrammar :: [Rule] -> Grammar
makeGrammar ruleOrder = Grammar
  { ruleOrder
  , rules
  , terminals = foldMap ruleTerminals ruleOrder
  , entities  = foldMap ruleEntities  ruleOrder
  , first
  -- , starter
  , types = foldMap ruleTypes ruleOrder
  }
  where
    rules =
      Map.fromList (map singleRule (evalState (assignNumberToRulesClauses ruleOrder) 0))

    assignNumberToRulesClauses :: [Rule] -> State Int [Rule]
    assignNumberToRulesClauses ruleset = do
      for ruleset \rule -> do
        clauses <- for rule.clauses \clause -> do
          index <- get <* modify (+ 1)
          pure (setNumber index clause)
        pure rule {clauses}

    singleRule :: Rule -> (NonTerminal, Set Rule)
    singleRule rule = (rule.entity, Set.singleton rule)

    first :: NonTerminal ==> Set Terminal
    first = fixpoint (foldMap fromRule (fold rules)) mempty
      where
        fromRule :: Rule -> NonTerminal ==> Set Terminal -> NonTerminal ==> Set Terminal
        fromRule rule cache = do
          rule.entity ==> do
            rule.clauses & foldMap \clause -> do
              case clause.points Array.! 0 of
                T _ term   -> Set.singleton term
                E _ entity -> cache ! entity

-------------------------------------------------------------------------------

instance Pretty Grammar where
  pPrint Grammar {rules} =
    rules
      & Map.assocs
      & fmap ruleBlock
      & vcat
    where
      ruleBlock :: (NonTerminal, Set Rule) -> Doc
      ruleBlock (_, ruleset) =
        vcat (map pPrint (toList ruleset))

instance Show Grammar where show = show . pPrint
