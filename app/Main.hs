
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
import LR1.Typed (Rule (..), clause, clauseS, noWrap)

data Expr
  = Plus   Expr Factor
  | Factor Factor
  deriving stock Show

data Factor
  = Mult Factor Term
  | Term Term
  deriving stock Show

data Term
  = Expr Expr
  | Num  String
  deriving stock Show

main :: IO ()
main = do
  let
    (grammar, mapping, proxy) = Typed.grammar mdo
      start <- Typed.clauseS @Expr expr

      expr <- Typed.clause @Expr
        [ Reduce Plus   :! expr :. "+" :! factor
        , Reduce Factor :! factor
        ]

      factor <- Typed.clause @Factor
        [ Reduce Mult :! factor :. "*" :! term
        , Reduce Term :! term
        ]

      term <- Typed.clause @Term
        [ Reduce Expr :. "(" :! expr :. ")"
        , Reduce Num  :? "num"
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
    liftIO $ print $ Parser.reduce proxy mapping res

  return ()
