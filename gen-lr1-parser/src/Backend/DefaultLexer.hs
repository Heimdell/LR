module Backend.DefaultLexer where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Foldable (fold, traverse_)
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative
import qualified Data.Set as Set
import Data.Char (isAlpha, isLowerCase, isUpperCase, isDigit, isSpace)
import Data.Set (Set)
import Control.Monad (void)
import System.Exit (exitFailure)
import Data.Text.Position
import Data.Lexeme

data Context__ = Context__
  { column, line ::  Int
  , filename     ::  FilePath
  , sourceLines  :: [Text]
  , source       ::  Text
  , chars        ::  Int
  }

openContext :: FilePath -> Text -> Context__
openContext filename source = Context__
  { column = 1
  , line   = 1
  , filename
  , source
  , sourceLines = Text.lines source
  , chars = 0
  }

openContextFromFile :: FilePath -> IO Context__
openContextFromFile filename = do
  openContext filename <$> Text.readFile filename

runLexer :: FilePath -> Text -> M a -> Either LexerError a
runLexer filename source ma =
  evalState (runExceptT (ma <* eof)) (openContext filename source)

advanceChar :: Char -> Text -> Context__ -> Context__
advanceChar c rest ctx = case c of
  '\n' -> ctx
    { column      = 1
    , line        = 1 + ctx.line
    , source      = rest
    , sourceLines = drop 1 ctx.sourceLines
    , chars       = 1 + ctx.chars
    }
  _ -> ctx
    { column = 1 + ctx.column
    , source = rest
    , chars  = 1 + ctx.chars
    }

advance :: Context__ -> Maybe (Char, Context__)
advance ctx = do
  (c, rest) <- Text.uncons ctx.source
  pure (c, advanceChar c rest ctx)

currentPosition :: Context__ -> Pos
currentPosition Context__{line, column, filename} = Pos
  { column
  , line
  , filename
  -- , sourceLine = fold (take 1 sourceLines)
  }

data LexerError
  = Expected String Pos
  | None

showLexerError :: LexerError -> Text -> String
showLexerError = \cases
  (Expected msg pos) text -> "expected " <> msg <> " at\n" <> showPos pos text
  (None)             _    -> "???"

type M = ExceptT LexerError (State Context__)

consumed :: M a -> M Text
consumed ma = do
  old <- get
  _   <- ma
  new <- get
  pure (Text.take (new.chars - old.chars) old.source)

satisfy :: String -> (Char -> Bool) -> M ()
satisfy msg pre = do
  gets advance >>= \case
    Nothing -> throwError . Expected msg  =<< gets currentPosition
    Just (c, new)
      | pre c     -> put new
      | otherwise -> throwError . Expected msg =<< gets currentPosition

eof :: M ()
eof = do
  gets advance >>= \case
    Nothing -> pure ()
    Just _ -> throwError . Expected "<eof>" =<< gets currentPosition

instance {-# OVERLAPS #-} Alternative M where
  empty   = throwError None
  l <|> r = catchError l (\_ -> r)

oneOf, noneOf :: String -> String -> M ()
oneOf  msg = satisfy msg . flip Set.member    . Set.fromList
noneOf msg = satisfy msg . flip Set.notMember . Set.fromList

lowercase, uppercase, digit :: M ()
lowercase = satisfy "lowercase" \c -> isAlpha c && isLowerCase c
uppercase = satisfy "uppercase" \c -> isAlpha c && isUpperCase c
digit     = satisfy "digit" isDigit

char :: Char -> M ()
char c = satisfy (show c) (== c)

slug :: String -> M ()
slug = traverse_ char

lowercaseName :: M Text
lowercaseName = consumed do
  lowercase
  many do
    lowercase <|> uppercase <|> digit <|> char '-' <|> char '_' <|> char '?'

uppercaseName :: M Text
uppercaseName = consumed do
  uppercase
  many do
    lowercase <|> uppercase <|> digit <|> char '-' <|> char '_' <|> char '?'

numberLiteral :: M Integer
numberLiteral = read . Text.unpack <$> consumed (some digit)

charLiteral :: M Text
charLiteral
  =    char '\\' *> asum
    [ "\\n"  <$ char '\n'
    , "\\t"  <$ char '\t'
    , "\\\\" <$ char '\\'
    , "\""   <$ char '\"'
    ]
  <|> consumed do
    noneOf "neither '\\' nor '\"'" "\\\""

stringLiteral :: M Text
stringLiteral = do
  char '\"'
  res <- fold <$> many charLiteral
  char '\"'
  pure res

operator :: M Text
operator = consumed do
  some do
    oneOf "operator" "!@#$%^&*-+=\\//<>.,~`:"

punctuator :: M Text
punctuator = consumed do oneOf "punctuator" "()[]{}"

reserve :: Set Text -> Text -> Either Text Text
reserve reserved word
  | Set.member word reserved = Left  word
  | otherwise                = Right word

notFollowedBy :: String -> M a -> M ()
notFollowedBy msg ma = do
  st <- get
  case evalState (runExceptT ma) st of
    Left  {} -> pure ()
    Right {} -> throwError . Expected ("not " <> msg) =<< gets currentPosition

infix 0 <?>
(<?>) :: String -> M a -> M a
msg <?> ma = ma `catchError` \_ -> throwError . Expected msg =<< gets currentPosition

blockComment :: M ()
blockComment = do
  "comment start" <?> slug "/--"
  void do
    many do
      notFollowedBy "comment end" do
        slug "--/"
      blockComment <|> void (satisfy "comment" (const True))
  "comment end" <?> slug "--/"

space :: M ()
space = "space or comment" <?> satisfy "space" isSpace <|> blockComment

spaces :: M ()
spaces = void do many space


lexeme :: Set Text -> M Lexeme
lexeme keywords = do
  start   <- gets currentPosition
  payload <- lexemeBody keywords
  _       <- spaces
  pure (start, payload)

lexemeBody :: Set Text -> M Payload
lexemeBody keywords = asum
  [ either Reserved LowercaseName . reserve keywords <$> lowercaseName
  , either Reserved UppercaseName . reserve keywords <$> uppercaseName
  ,                 NumberLiteral                    <$> numberLiteral
  ,                 StringLiteral                    <$> stringLiteral
  , either Reserved Operator      . reserve keywords <$> operator
  , either Reserved Punctuator    . reserve keywords <$> punctuator
  ]

lexText :: FilePath -> Text -> [String] -> Either LexerError ([Lexeme], Pos)
lexText filename source keywords =
  runLexer filename source do
    (,) <$ spaces <*> many (lexeme (Set.fromList $ map Text.pack keywords)) <*> gets currentPosition

dieOnLexerError :: Either LexerError a -> IO a
dieOnLexerError = \case
  Left le@(Expected _ pos) -> do
    text <- Text.readFile pos.filename
    putStrLn $ showLexerError le text
    exitFailure
  Left None -> error "impossible: LexerError is None"
  Right a -> pure a

dieOnParserError :: Either (Pos, [String]) a -> IO a
dieOnParserError = \case
  Left (pos, expected) -> do
    text <- Text.readFile pos.filename
    putStrLn $ showPos pos text
    putStrLn $ "expected one of " <> show expected
    exitFailure
  Right a -> pure a
