
module Lex (module Lex, Regex) where

import Control.Applicative
import Control.Monad

import Text.Regex.TDFA
import Text.Regex.TDFA.String

import Util
import Table
import S
import Pretty
import Set qualified

token :: String -> Regex
token = asRegex . ("^" <>)

asRegex :: String -> Regex
asRegex s
  = either (\e -> error $ show (s, e)) id
  $ compile defaultCompOpt defaultExecOpt s

wsp :: Regex
wsp = asRegex "^[ \n]*"

tokenizer :: Pretty t => [(t, Regex)] -> String -> Either String [(t, S)]
tokenizer lexers input = do
  (_, input') <- input =? wsp

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
        (lexeme, restInput) <- input =? lexer
        return ((make, S lexeme), restInput)
      <|> do
        go lexers input

(=?) :: String -> Regex -> Either String (String, String)
input =? regex = do
  case regexec regex input of
    Right (Just ("", a, b, _)) -> return (a, b)
    Right  Nothing             -> fail "no match"
    Left s                     -> error s
