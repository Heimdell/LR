
module Lex (module Lex, Regex) where

import Control.Applicative
import Control.Monad

import Data.List (partition)

import Text.Regex.TDFA
import Text.Regex.TDFA.String

import Util
import Table
import S
import Pretty
import Term
import Set qualified

token :: String -> Regex
token = asRegex . ("^" <>)

asRegex :: String -> Regex
asRegex s
  = either (\e -> error $ show (s, e)) id
  $ compile defaultCompOpt defaultExecOpt
  $ s

wsp :: Regex
wsp = asRegex "^[ \n]*"

tokenizer :: forall t. Pretty t => [(t, Regex)] -> String -> Either String [(t, S)]
tokenizer lexers input = do
  (_, input') <- input =? (undefined :: t, wsp)

  if null input'
  then do
    return []

  else do
    (r, restInput)  <- go lexers input'
    rs              <- tokenizer lexers restInput
    return (r : rs)

  where
    go [] input = Left ("cannot recognise lexeme: " ++ show (take 20 input))
    go ((make, lexer) : lexers) input = do
        (lexeme, restInput) <- input =? (make, lexer)
        return ((make, S lexeme), restInput)
      <|> do
        go lexers input

(=?) :: Pretty t => String -> (t, Regex) -> Either String (String, String)
input =? (t, regex) = do
  case regexec regex input of
    Right (Just ("", a, b, _)) -> return (a, b)
    Right (Just (z,  a, b, _)) -> error $ show (pretty t, z, a, b)
    Right  Nothing             -> fail "no match"
    Left s                     -> error s

tokenize :: (Ord term, Show term, Pretty term) => Table term -> String -> Either String [(term, S)]
tokenize table
  = tokenizer branches
  where
    allBranches = Set.toList (getTerminals table)
    (reg, kws)  = partition (headIs '?') allBranches
    branches    = prepare kws <> prepareReg reg
    prepare list = list >>= \case
      Next term -> [(term, token $ show term)]
      wat       -> []

    prepareReg list = list >>= \case
      Next term -> [(term, token $ drop 1 $ show term)]
      wat       -> []

    headIs c = \case
      Next (show -> (c1 : _)) -> c == c1
      _                       -> False