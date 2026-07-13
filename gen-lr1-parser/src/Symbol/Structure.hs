{- |
  Elements of rule.
-}
module Symbol.Structure where

import Data.String (IsString)
import Data.Text   (Text)
import GHC.Records

{- |
  Terminal, used in grammar.
-}
newtype Term = Term
  { term :: Text
  }
  deriving newtype (Eq, Ord, IsString)

{- |
  Non-terminal, used in grammar.
-}
newtype Entity = Entity
  { entity :: Text
  }
  deriving newtype (Eq, Ord, IsString)

{- |
  Terminal or non-terminal, used in grammar.
-}
data Symbol
  = T (Maybe Text) Term
  | E (Maybe Text) Entity
  deriving stock (Eq, Ord)

instance HasField "name" Symbol (Maybe Text) where
  getField = \case
    T name _ -> name
    E name _ -> name

{- |
  Check it point is a terminal.
-}
pointTerminals :: Symbol -> Maybe Term
pointTerminals = \case
  T _ term -> Just term
  _        -> Nothing

{- |
  Check it point is a non-terminal.
-}
pointEntities :: Symbol -> Maybe Entity
pointEntities = \case
  E _ entity -> Just entity
  _          -> Nothing
