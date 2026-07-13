module Symbol where

import Data.String (IsString)
import Data.Text   (Text)
import GHC.Records

import Text.PrettyPrint.HughesPJClass (text, Pretty(pPrint))
import Data.Text qualified as Text


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

-------------------------------------------------------------------------------

instance Pretty Terminal   where pPrint = pPrint . (.term)
instance Pretty NonTerminal where pPrint = pPrint . (.entity)

instance Pretty Symbol where
  pPrint = \case
    T mbName term   -> maybe (pPrint term)   (\name -> pPrint name <> ":" <> pPrint term  ) mbName
    E mbName entity -> maybe (pPrint entity) (\name -> pPrint name <> ":" <> pPrint entity) mbName

instance Show Terminal   where show = show . pPrint
instance Show NonTerminal where show = show . pPrint

instance Pretty Text where
  pPrint = text . Text.unpack

instance Pretty Lookahead where
  pPrint = \case
    LookForTerm term -> pPrint term
    LookForEOF       -> "<eof>"
