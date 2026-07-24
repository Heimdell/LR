module Symbol where

import Data.String (IsString (fromString))
import Data.Text   (Text)

import Pretty


{- |
  Name for lexeme class.
  Represents sequence of a single lexeme.
-}
type Terminal = Text

{- |
  Name for construct.
  Represents sequence that is /at least/ one lexeme long.
-}
type NonTerminal = Text

{- |
  Extend `Terminal` with an EOF constructor.

  This is a terminal we expect after some `Rule` finished parsing.
-}
data Lookahead
  = LookForEOF
  | LookForTerm Terminal
  deriving stock (Eq, Ord)
  deriving (Show) via PP Lookahead

instance Pretty Lookahead where
  pPrint = \case
    LookForEOF       -> "⊥"
    LookForTerm term -> pPrint term

instance IsString Lookahead where
  fromString = LookForTerm . fromString

{- |
  Piece of RHS expression of a rule.

  > E = E '+' F
  >     ~ ~~~ ~ all 3 are symbols

  Symbol intentionally cannot represent starting non-terminal `S` or terminal `LookForEOF`.
-}
data Symbol
  = Term    Text
  | NonTerm Text
  deriving stock (Eq, Ord)
  deriving (Show) via PP Symbol

instance Pretty Symbol where
  pPrint = \case
    Term    term   -> pPrint term
    NonTerm entity -> pPrint entity

{- |
  Non-terminal, constructed by some rule.

  This is extension of `NonTerminal` with starting terminal `S`.
-}
data Entity
  = S
  | Named NonTerminal
  deriving stock (Eq, Ord)
  deriving (Show) via PP Entity

instance Pretty Entity where
  pPrint = \case
    S       -> "⊤"
    Named n -> pPrint n

instance IsString Entity where
  fromString = Named . fromString

data NamedSymbol = (:@)
  { name   :: Maybe Text
  , symbol :: Symbol
  }
  deriving stock (Eq, Ord)
  deriving (Show) via PP NamedSymbol

instance IsString NamedSymbol where
  fromString = (Nothing :@) . Term . fromString

instance Pretty NamedSymbol where
  pPrint (name :@ symbol) = maybe id (\n -> (pPrint n <.> ":" <.>)) name (pPrint symbol)
