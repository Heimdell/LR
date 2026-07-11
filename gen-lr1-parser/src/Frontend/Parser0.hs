module Frontend.Parser0 where
-- import Data.Array

-- import Text.Parsec.String
-- import Text.ParserCombinators.Parsec.Language (haskell)
-- import Rule
-- import Term
-- import Control.Applicative (some)
-- import Text.Parsec.Token
-- import Text.Parsec
-- import qualified Data.Text as Text
-- import Data.Char (isLower, isUpper, isSpace)
-- import Control.Monad
-- import Data.Text (Text)
-- import Data.Array (Array, listArray)
-- import Data.Foldable
-- import Data.Text.Position ( Pos(Pos) )
-- import qualified RawGrammar as Raw

-- parseGrammar :: FilePath -> IO ([Text], Raw.Grammar)
-- parseGrammar path = either (error . show) id <$> parseFromFile program path

-- program :: Parser ([Text], Raw.Grammar)
-- program = (,)
--   <$  spaces
--   <*> addendum
--   <*  spaces
--   <*> grammar
--   <*  eof

-- start :: Parser Entity
-- start = do
--   for_ ("start" :: String) char
--   spaces
--   entityKind

-- addendum :: Parser [Text]
-- addendum = many do
--   reservedOp haskell "|"
--   Text.dropWhileEnd isSpace . Text.pack <$> anyChar `manyTill` char '\n'

-- grammar :: Parser Raw.Grammar
-- grammar = Raw.Grammar <$> start <*> many rule

-- pos :: Parser Pos
-- pos = Pos
--   <$> do sourceColumn <$> getPosition
--   <*> do sourceLine   <$> getPosition
--   <*> do sourceName   <$> getPosition

-- point :: Parser Point
-- point = asum
--   [ bindingPoint
--   , parens haskell bindingPoint
--   , T Nothing <$> termKind
--   , E Nothing <$> entityKind
--   ]

-- bindingPoint :: Parser Point
-- bindingPoint = do
--   var <- Just <$> variable
--   _ <- char ':'
--   spaces
--   asum
--     [ do e <- entityKind
--          pure (E var e)

--     , do t <- termKind
--          pure (T var t)
--     ]

-- variable :: Parser Text
-- variable = flip (<?>) "binder" do
--   pt <- Text.pack <$> identifier haskell
--   guard case Text.uncons pt of
--     Nothing -> error ""
--     Just (c, _) -> isLower c
--   pure pt

-- termKind :: Parser Term
-- termKind = flip (<?>) "terminal" do
--   try do
--     pt <- Text.pack <$> stringLiteral haskell
--     pure (Term pt)


-- entityKind :: Parser Entity
-- entityKind = flip (<?>) "entity" do
--   try do
--     pt <- Text.pack <$> identifier haskell
--     guard case Text.uncons pt of
--       Nothing -> error ""
--       Just (c, _) -> isUpper c
--     pure (Entity pt)

-- rule :: Parser Rule
-- rule = Rule
--   <$> entityKind
--   <*> optionMaybe do
--     _ <- char ':'
--     spaces
--     Text.dropWhileEnd isSpace . Text.pack <$> manyTill anyChar (lookAhead (reservedOp haskell "="))
--   <*  reservedOp haskell "="
--   <*> flip sepBy (reservedOp haskell "|") do
--     mkClause
--       <$> some point
--       <*  char '{'
--       <*> pos
--       <*> reducingAction
--       <*  char '}'
--       <*  spaces

-- reducingAction :: Parser Text
-- reducingAction = do
--   ((), str) <- cover do
--     reducingActionText

--   pure (Text.pack str)

-- reducingActionText :: Parser ()
-- reducingActionText = do
--   void do
--     many reducingActionLetter

-- reducingActionLetter :: Parser ()
-- reducingActionLetter = asum
--   [ void do noneOf "{}"
--   , braces haskell reducingActionText
--   ]

-- cover :: (Monad m) => ParsecT [tok] u m a -> ParsecT [tok] u m (a, [tok])
-- cover p = do
--   before <- getInput
--   result <- p
--   after  <- getInput
--   let matchedLength = length before - length after
--       consumedText  = take matchedLength before
--   return (result, consumedText)

-- listToArray :: [a] -> Array Int a
-- listToArray as = Data.Array.listArray (0, length as - 1) as
