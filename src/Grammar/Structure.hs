module Grammar.Structure where

import Rule
import Term
import Data.Set (Set)
import qualified Data.Map.Monoidal as Map
import Data.Map.Monoidal (type (==>), (!), (==>))
import qualified Data.Set as Set
import Data.Foldable (fold)
import Fixpoint (fixpoint)
import qualified Data.List.NonEmpty as NonEmpty

data Grammar = Grammar
  { ruleOrder :: [Rule]
  , rules     :: Entity ==> Set Rule
  , terminals :: Set Term
  , entities  :: Set Entity
  , first     :: Entity ==> Set Term
  }

makeGrammar :: [Rule] -> Grammar
makeGrammar ruleOrder = Grammar
  { ruleOrder
  , rules
  , terminals = foldMap ruleTerminals ruleOrder
  , entities  = foldMap ruleEntities  ruleOrder
  , first
  }
  where
    rules = Map.fromList (map singleRule ruleOrder)

    singleRule :: Rule -> (Entity, Set Rule)
    singleRule rule = (rule.entity, Set.singleton rule)

    first :: Entity ==> Set Term
    first = fixpoint (foldMap fromRule (fold rules)) mempty
      where
        fromRule :: Rule -> Entity ==> Set Term -> Entity ==> Set Term
        fromRule rule cache = do
          rule.entity ==> do
            case NonEmpty.head rule.points of
              T term   -> Set.singleton term
              E entity -> cache ! entity
