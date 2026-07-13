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
newtype Terminal = Terminal
  { term :: Text
  }
  deriving newtype (Eq, Ord, IsString)

data Lookahead
  = LookForTerm Terminal
  | LookForEOF
  deriving stock (Eq, Ord)

{- |
  Non-terminal, used in grammar.
-}
newtype NonTerminal = NonTerminal
  { entity :: Text
  }
  deriving newtype (Eq, Ord, IsString)

{- |
  Terminal or non-terminal, used in grammar.
-}
data Symbol
  = T (Maybe Text) Terminal
  | E (Maybe Text) NonTerminal
  deriving stock (Eq, Ord)

instance HasField "name" Symbol (Maybe Text) where
  getField = \case
    T name _ -> name
    E name _ -> name

{- |
  Check it point is a terminal.
-}
pointTerminals :: Symbol -> Maybe Terminal
pointTerminals = \case
  T _ term -> Just term
  _        -> Nothing

{- |
  Check it point is a non-terminal.
-}
pointEntities :: Symbol -> Maybe NonTerminal
pointEntities = \case
  E _ entity -> Just entity
  _          -> Nothing
