
module Lexer where

-- import Control.Applicative ( Alternative(many) )
-- import Data.Text qualified as Text
-- import Data.Void qualified as Void
-- import Text.Megaparsec hiding (many)
-- import Text.Megaparsec.Char ( space1 )
-- import Text.Megaparsec.Char.Lexer qualified as L

-- import LR1.Term qualified as Term
-- import Data.Text (Text)
-- import Data.Foldable (asum)
-- import Data.String (IsString(fromString))
-- import Data.Function ((&))
-- import Data.Bifunctor (first)

-- data Lexer = Lexer
--   { lineComment           :: Text
--   , blockComment          :: (Text, Text)
--   , blockCommentRecursive :: Bool
--   , terminals             :: [Text]
--   , stringLiteral         :: Parser (Term.T, Text)
--   , numberLiteral         :: Parser (Term.T, Text)
--   , customLexemes         :: [Parser (Term.T, Text)]
--   }

-- runLexer :: Lexer -> String -> Text -> Either String [(Term.T, Loc, Text)]
-- runLexer lexer fname = first errorBundlePretty . flip parse fname do
--   let
--     space' = space
--       do lineComment lexer
--       do blockComment lexer
--       do blockCommentRecursive lexer

--     lexemes
--       = mkTerminals (terminals lexer)
--       : stringLiteral lexer
--       : numberLiteral lexer
--       : customLexemes lexer

--   many (lexeme space' lexemes)
--     <* eof

-- data Loc = Loc { col, line :: Int, file :: Text }

-- instance Show Loc where
--   show Loc {line, col} = show line <> ":" <> show col

-- type Parser = Parsec Void.Void Text.Text

-- choose :: Alternative a => [a x] -> a x
-- choose = asum

-- space :: Text -> (Text, Text) -> Bool -> Parser ()
-- space lc (bcb, bce) nest =
--   L.space space1
--     do L.skipLineComment lc
--     do (if nest then L.skipBlockCommentNested else L.skipBlockComment) bcb bce

-- lexeme :: Parser () -> [Parser (Term.T, Text)] -> Parser (Term.T, Loc, Text)
-- lexeme spac list = do
--   spac
--   pos <- getSourcePos
--   (t, s) <- choose list
--   spac
--   return (t, sourcePosToLoc pos, s)

-- stdNumberLiteral :: Term.T -> Parser (Term.T, Text)
-- stdNumberLiteral = asTerm L.scientific

-- stdStringLiteral :: Text -> Text -> Term.T -> Parser (Term.T, Text)
-- stdStringLiteral begin end = asTerm $ chunk begin >> manyTill L.charLiteral (chunk end)

-- sourcePosToLoc :: SourcePos -> Loc
-- sourcePosToLoc pos =
--   Loc
--     { col  = sourceColumn pos & unPos
--     , line = sourceLine   pos & unPos
--     , file = sourceName   pos & fromString
--     }

-- mkTerminals :: [Text] -> Parser (Term.T, Text)
-- mkTerminals =
--   choose . map term
--   where
--     term :: Text -> Parser (Term.T, Text)
--     term t = do
--       chunk t
--       return (Term.Term t, t)

-- asTerm :: MonadParsec e s m => m b -> a -> m (a, Tokens s)
-- asTerm p t = do
--   (s, _) <- match p
--   return (t, s)

-- runP :: Parser a -> Text.Text -> a
-- runP p = either (error . errorBundlePretty) id . parse p "-"
