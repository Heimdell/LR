
import Data.Function ((&))

import Control.Monad.State
import Data.Text qualified as Text
import Text.Read

import LR1.ACTION  qualified as ACTION
import LR1.FIRST   qualified as FIRST
import LR1.GOTO    qualified as GOTO
import LR1.Grammar qualified as Grammar
import LR1.Lexeme  qualified as Lexeme
import LR1.NonTerm (T(Start))
import LR1.Parser  qualified as Parser
import LR1.Point (e, cat)
import LR1.State   qualified as State
import LR1.Term    qualified as Term
import LR1.Typed   qualified as Typed
import LR1.Typed (Rule (..), clause, clauseS)

data AST
  = Plus AST AST
  | Mult AST AST
  | Num  String
  deriving stock Show

main :: IO ()
main = do
  let
    (grammar, mapping, proxy) = Typed.grammar mdo
      start <- Typed.clauseS
        [ expr `E` R id ]

      expr <- Typed.clause
        [ expr   `E` "+" `T` factor `E` R \a _ b -> Plus a b
        , factor `E` R id
        ]

      factor <- Typed.clause
        [ factor `E` "*" `T` term `E` R \a _ b -> Mult a b
        , term   `E` R id
        ]

      term <- Typed.clause
        [ "(" `T` expr `E` ")" `T` R \_ a _ -> a
        , "num" `C` R id
        ]

      return start

    first = FIRST.make grammar

  print grammar

  flip runStateT State.emptyReg $ do
    -- build tables
    goto   <- GOTO.make grammar first
    action <- ACTION.make goto

    let conflicts = ACTION.conflicts action

    -- check conflicts
    unless (null $ ACTION.unwrap conflicts) do
      log' <- ACTION.dump "CONFLICTS" conflicts
      liftIO $ putStrLn log'
      error "conflicts"

    -- lexing
    let
      t = Term.Term . Lexeme.Concrete
      d = Term.Term . Lexeme.Category

      lexer = (<> [(Term.EndOfStream, "")]) . map lex' . words

      lex' s
        | Just (_ :: Int) <- readMaybe s = (d "num", s)
        | otherwise                      = (t (Text.pack s), s)

    -- lex input
    liftIO $ putStrLn "Input something, like \"1 * 2 + 3 * ( 4 + 5 ) * 6\""
    str <- liftIO $ getLine

    let input = lexer str
    liftIO $ print input

    -- run parser
    res <- Parser.run goto action input
    liftIO $ print $ Parser.reduce proxy mapping Num res

  return ()
