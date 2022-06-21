
{-|
  Enough stick and acorns to ducktape yourself something like a lexer
  (that is compatible with a parser from this package).
-}

module Text.Brick.Lexer (module Text.Brick.Lexer, SourcePos (..), module Data.Char, L.scientific) where

import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Char qualified as LC
import Text.Megaparsec (MonadParsec(eof), SourcePos, getSourcePos, parse, errorBundlePretty, Parsec)
import Control.Monad (guard)
import Data.Char
import Data.Function
import Data.Void ( Void )
import Data.Text ( Text )
import Data.String (fromString)
import Data.Text qualified as Text
import Text.Megaparsec qualified as L hiding (many)
import Control.Applicative ( Alternative(many, (<|>), empty) )
import Control.Arrow (ArrowChoice(left))
import Text.Megaparsec (unPos)

import Text.Brick.Driver.LR1 (IsPosition(..))

type Parser = Parsec Void Text

space :: Parser ()
space = L.space LC.space1 (L.skipLineComment "//") (L.skipBlockCommentNested "(*" "*)")

tok :: Parser a -> Parser a
tok = L.lexeme space

-- | A keyword.
--
kw
  :: Text  -- ^ What does it looks like?
  -> l     -- ^ Lexeme type.
  -> p     -- ^ Parsed state.
  -> Parser (l, SourcePos, p)
kw s l p = do
  pos <- getSourcePos
  _ <- L.symbol space s
  return (l, pos, p)

-- | A continious piece of digits or letter, for instance.
--
satisfy1
  :: (Char -> Bool)  -- ^ Predicate.
  -> l               -- ^ Lexeme type.
  -> (Text -> p)     -- ^ A constructor for parse.
  -> Parser (l, SourcePos, p)
satisfy1 pre l p = do
  pos <- getSourcePos
  s <- L.takeWhile1P Nothing pre
  return (l, pos, p s)

nameLike
  :: l
  -> (Char -> Bool)
  -> (Char -> Bool)
  -> (Text -> p)
  -> Text
  -> Parser (l, SourcePos, p)
nameLike l headChar tailChars p reserved = L.try do
  pos <- getSourcePos
  h   <- L.satisfy headChar
  t   <- L.takeWhileP Nothing tailChars
  let res = Text.cons h t
  guard (res `notElem` Text.words reserved)
  return (l, pos, p res)

stringLit
  :: l
  -> (Text -> p)
  -> Parser (l, SourcePos, p)
stringLit l p = do
  pos <- getSourcePos
  str <- (LC.char '"'  >> L.manyTill L.charLiteral (LC.char '"'))
    <|>  (LC.char '\'' >> L.manyTill L.charLiteral (LC.char '\''))
  return (l, pos, p $ fromString str)

asToken :: l -> Parser a -> (a -> p) -> Parser (l, SourcePos, p)
asToken l k p = do
  pos <- getSourcePos
  res <- k
  return (l, pos, p res)

-- | Why isn't it in @base@?
--
choose :: Alternative f => [f a] -> f a
choose = Prelude.foldl (<|>) empty

-- | Consume a `Text`, produce lexeme stream.
--
runLexer
  :: l                           -- ^ End-of-file marker.
  -> p                           -- ^ End-of-file parse - /probably/ won't be used (can't tell for sure, too much duct tape).
  -> [Parser (l, SourcePos, p)]  -- ^ Parsers for lexemes, in order of descending priority.
  -> String                      -- ^ Filename.
  -> Text                        -- ^ Input.
  -> Either String [(l, SourcePos, p)]
runLexer end pend ps fname inp = do
  left errorBundlePretty $ parse manyP fname inp
  where
    manyP = do
      space
      ts <- many (choose ps <* space)
      pos <- getSourcePos
      eof
      return (ts <> [(end, pos, pend)])

instance IsPosition SourcePos where
  posName p = p.sourceName
  posLine p = p.sourceLine & unPos
  posColumn p = p.sourceColumn & unPos