module Grammar.Structure where

import Data.Foldable (fold)
import Data.Set      (Set)

import Data.Array qualified as Array ((!))
import Data.Set   qualified as Set

import Data.Map.Monoidal (type (==>), (!), (==>))
import Fixpoint          (fixpoint)
import Rule              (Rule(..), ruleTerminals, ruleEntities, setNumber)
import Term              (Point(E, T), Entity, Term)

import Data.Map.Monoidal qualified as Map
import Data.Array (listArray)
import Backend.DefaultLexer

data Grammar = Grammar
  { ruleOrder :: [Rule]
  , rules     :: Entity ==> Set Rule
  , terminals :: Set Term
  , entities  :: Set Entity
  , first     :: Entity ==> Set Term
  , starter   :: Entity
  }

makeGrammar :: Entity -> [Rule] -> Grammar
makeGrammar starter ruleOrder = Grammar
  { ruleOrder
  , rules
  , terminals = foldMap ruleTerminals ruleOrder
  , entities  = foldMap ruleEntities  ruleOrder
  , first
  , starter
  }
  where
    rules =
      Map.fromList (map singleRule (zipWith setNumber [0..] ruleOrder)) <> do
        "Start" ==> Set.singleton do
          Rule
            { reducer = "res"
            , points  = listArray (0, 0) [E (Just "res") starter]
            , mark    = -1
            , entity  = starter
            , pos     = Pos 0 0 "<nowhere>"
            }

    singleRule :: Rule -> (Entity, Set Rule)
    singleRule rule = (rule.entity, Set.singleton rule)

    first :: Entity ==> Set Term
    first = fixpoint (foldMap fromRule (fold rules)) mempty
      where
        fromRule :: Rule -> Entity ==> Set Term -> Entity ==> Set Term
        fromRule rule cache = do
          rule.entity ==> do
            case rule.points Array.! 0 of
              T _ term   -> Set.singleton term
              E _ entity -> cache ! entity
