{- |
  Grammatic rules.
-}
module Rule.Structure where

import Term (Point, Entity, Term, pointTerminals, pointEntities)

import Data.List.NonEmpty             (NonEmpty)
import Data.Set                       (Set)
import Data.Set qualified as Set
import Data.Text                      (Text)

{- |
  Rule in the form of `Entity` ::= {`Point`} @Reducer@.
-}
data Rule = Rule
  { entity  :: Entity          -- ^ entity constructed by rule
  , mark    :: Int             -- ^ number unique to rule
  , points  :: NonEmpty Point  -- ^ sequence of [non]terminals
  , reducer :: Text            -- ^ action to perform
  }
  deriving stock (Eq, Ord)

{- |
  Terminals, mentioned in the rule.
-}
ruleTerminals :: Rule -> Set Term
ruleTerminals rule
  = foldMap (foldMap Set.singleton . pointTerminals) rule.points

{- |
  Non-terminals, mentioned or declared in the rule.
-}
ruleEntities :: Rule -> Set Entity
ruleEntities rule
  =  Set.singleton rule.entity
  <> foldMap (foldMap Set.singleton . pointEntities) rule.points
