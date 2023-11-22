
module Grammar where

import Data.String
import Data.Set qualified as Set
import Data.Function
import Data.Maybe
import Data.Char

import Point
import Rule
import Data.Map.Monoidal qualified as Map

data Grammar = MkGrammar
  { inOrder :: [Rule]
  , rules   :: Map.Map Entity (Set.Set Rule)
  }

fromStrings :: [String] -> Grammar
fromStrings rulesStr = MkGrammar
  { inOrder
  , rules = inOrder & foldMap \rule ->
      rule.entity `Map.singleton` Set.singleton rule
  }
  where
    inOrder = map fromString rulesStr

instance Show Grammar where
  show grammar =
    unlines (map show grammar.inOrder)

allPoints :: Grammar -> Set.Set Point
allPoints = foldMap rulePoints . (.inOrder)
  where
    rulePoints :: Rule -> Set.Set Point
    rulePoints rule = Set.singleton (Entity rule.entity) <> Set.fromList rule.points