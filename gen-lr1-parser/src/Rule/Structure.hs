{- |
  Grammatic rules.
-}
module Rule.Structure where

import Data.Array    (Array)
import Data.Function (on)
import Data.Set      (Set)
import Data.Text     (Text)
import Data.Maybe    (fromMaybe)

import Data.Array qualified as Array
import Data.Set   qualified as Set
import Data.Map.Monoidal (type (==>), (==>))

import Term (Point, Entity(entity), Term, pointTerminals, pointEntities)
import Data.Text.Position (Pos)


{- |
  Rule in the form of `Entity` ::= {`Point`} @Reducer@.
-}
data Rule = Rule
  { entity  :: Entity          -- ^ entity constructed by rule
  , type_   :: Maybe Text
  , mark    :: Int             -- ^ number unique to rule
  , points  :: Array Int Point  -- ^ sequence of [non]terminals
  , pos     :: Pos
  , reducer :: Text            -- ^ action to perform
  }

instance Eq  Rule where (==)    = (==)    `on` (.mark)
instance Ord Rule where compare = compare `on` (.mark)

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

mkRule :: Entity -> [Point] -> Pos -> Text -> Rule
mkRule entity pointList pos reducer = Rule
  { entity
  , points = Array.listArray (0, length pointList - 1) pointList
  , mark   = -1
  , reducer
  , pos
  }

setNumber :: Int -> Rule -> Rule
setNumber mark rule = rule {mark}

ruleTypes :: Rule -> Entity ==> Set Text
ruleTypes rule = rule.entity ==> Set.singleton (fromMaybe rule.entity.entity rule.type_)
