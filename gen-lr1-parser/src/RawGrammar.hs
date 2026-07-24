module RawGrammar where


import Symbol
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text.Position (Pos)
import GHC.Records

data RawGrammar = RawGrammar
  { imports :: [Text]
  , targets :: NonEmpty (Pos, NonTerminal)
  , rules   :: NonEmpty RawStmt
  }
  deriving stock (Show)

data RawStmt = StmtRule RawRule | StmtSep RawSep
  deriving stock (Show)

instance HasField "entity" RawStmt NonTerminal where
  getField = \case
    StmtRule ru -> ru.entity
    StmtSep  se -> se.entity

data RawSep = RawSep
  { pos    :: Pos
  , entity :: NonTerminal
  , type_  :: Text
  , single :: (Pos, Symbol)
  , sep    :: Maybe Terminal
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
