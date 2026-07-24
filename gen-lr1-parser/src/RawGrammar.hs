module RawGrammar where


import Symbol
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text.Position (Pos)

data RawGrammar = RawGrammar
  { imports :: [Text]
  , targets :: NonEmpty (Pos, NonTerminal)
  , rules   :: NonEmpty RawRule
  }
  deriving stock (Show)

data RawRule = RawRule
  { pos     :: Pos
  , entity  :: NonTerminal
  , type_   :: Text
  , clauses :: NonEmpty RawClause
  }
  deriving stock (Show)

data RawClause = RawClause
  { pos     :: Pos
  , symbols :: NonEmpty (Pos, NamedSymbol)
  , reduce  :: Text
  }
  deriving stock (Show)
