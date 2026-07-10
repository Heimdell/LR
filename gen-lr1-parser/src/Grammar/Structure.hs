module Grammar.Structure where

import Control.Monad.State

import Data.Foldable (fold)
import Data.Set      (Set)

import Data.Array qualified as Array ((!))
import Data.Set   qualified as Set
import Data.Function

import Data.Map.Monoidal (type (==>), (!), (==>))
import Fixpoint          (fixpoint)
import Rule
import Term              (Point(E, T), Entity, Term)

import Data.Map.Monoidal qualified as Map
import Data.Array (listArray)
import Data.Text.Position
import Data.Text (Text)
import Data.Traversable (for)

data Grammar = Grammar
  { ruleOrder :: [Rule]
  , rules     :: Entity ==> Set Rule
  , terminals :: Set Term
  , entities  :: Set Entity
  , first     :: Entity ==> Set Term
  , starter   :: Entity
  , types     :: Entity ==> Set Text
  }

makeGrammar :: Entity -> [Rule] -> Grammar
makeGrammar starter ruleOrder = Grammar
  { ruleOrder
  , rules
  , terminals = foldMap ruleTerminals ruleOrder
  , entities  = foldMap ruleEntities  ruleOrder
  , first
  , starter
  , types = foldMap ruleTypes ruleOrder
  }
  where
    rules =
      Map.fromList (map singleRule (evalState (assignNumberToRulesClauses ruleOrder) 0)) <> do
        "Start" ==> Set.singleton do
          Rule "Start" Nothing
            [ Clause
              { reducer = "res"
              , points  = listArray (0, 0) [E (Just "res") starter]
              , mark    = -1
              , pos     = Pos 0 0 "<nowhere>"
              }
            ]

    assignNumberToRulesClauses :: [Rule] -> State Int [Rule]
    assignNumberToRulesClauses rules = do
      for rules \rule -> do
        clauses <- for rule.clauses \clause -> do
          index <- get <* modify (+ 1)
          pure (setNumber index clause)
        pure rule {clauses}

    singleRule :: Rule -> (Entity, Set Rule)
    singleRule rule = (rule.entity, Set.singleton rule)

    first :: Entity ==> Set Term
    first = fixpoint (foldMap fromRule (fold rules)) mempty
      where
        fromRule :: Rule -> Entity ==> Set Term -> Entity ==> Set Term
        fromRule rule cache = do
          rule.entity ==> do
            rule.clauses & foldMap \clause -> do
              case clause.points Array.! 0 of
                T _ term   -> Set.singleton term
                E _ entity -> cache ! entity
