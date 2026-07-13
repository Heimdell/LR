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

import Symbol (Symbol, NonTerminal(entity), Terminal, pointTerminals, pointEntities)
import Data.Text.Position (Pos)


{- |
  Rule in the form of `NonTerminal` ::= {`Symbol`} @Reducer@.
-}
data Rule = Rule
  { entity  :: NonTerminal          -- ^ entity constructed by rule
  , type_   :: Maybe Text
  , clauses :: [Clause]
  }

instance Eq  Rule where (==)    = (==)    `on` (.entity)
instance Ord Rule where compare = compare `on` (.entity)

instance Eq  Clause where (==)    = (==)    `on` (.mark)
instance Ord Clause where compare = compare `on` (.mark)

data Clause = Clause
  { mark    :: Int             -- ^ number unique to rule
  , points  :: Array Int Symbol  -- ^ sequence of [non]terminals
  , pos     :: Pos
  , reducer :: Text            -- ^ action to perform
  }

{- |
  Terminals, mentioned in the rule.
-}
ruleTerminals :: Rule -> Set Terminal
ruleTerminals rule
  = foldMap (foldMap (foldMap Set.singleton . pointTerminals) . (.points)) rule.clauses

{- |
  Non-terminals, mentioned or declared in the rule.
-}
ruleEntities :: Rule -> Set NonTerminal
ruleEntities rule
  =  Set.singleton rule.entity
  <> foldMap (foldMap (foldMap Set.singleton . pointEntities) . (.points)) rule.clauses

mkClause :: [Symbol] -> Pos -> Text -> Clause
mkClause pointList pos reducer = Clause
  { points = Array.listArray (0, length pointList - 1) pointList
  , mark   = -1
  , reducer
  , pos
  }

setNumber :: Int -> Clause -> Clause
setNumber mark clause = clause {mark}

ruleTypes :: Rule -> NonTerminal ==> Set Text
ruleTypes rule = rule.entity ==> Set.singleton (fromMaybe rule.entity.entity rule.type_)

forClauses :: (Clause -> Clause) -> Rule -> Rule
forClauses f rule = rule {clauses = fmap f rule.clauses}
