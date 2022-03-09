
import Data.Function ((&))

import LR1.Grammar qualified as Grammar
import LR1.FIRST qualified as FIRST
import LR1.Item qualified as Item
import LR1.State qualified as State
import LR1.Term qualified as Term
import LR1.GOTO qualified as GOTO
import LR1.ACTION qualified as ACTION
import LR1.Lexeme qualified as Lexeme
import LR1.Parser qualified as Parser
import LR1.NonTerm (T(Start))
import LR1.Point (e, cat)
import Data.Set qualified as Set
import Control.Monad.State
import LR1.Fixpoint (one)
import Data.Text qualified as Text
import Data.Text (Text)
import Data.Monoid
import Text.Read

main = do
  let
    grammar = Grammar.empty
      & Grammar.add Start    "start"  [e "Expr"]
      & Grammar.add "Expr"   "plus"   [e "Expr", "+", e "Factor"]
      & Grammar.add "Expr"   "factor" [e "Factor"]
      & Grammar.add "Factor" "mult"   [e "Factor", "*", e "Term"]
      & Grammar.add "Factor" "term"   [e "Term"]
      & Grammar.add "Term"   "grp"    ["(", e "Expr", ")"]
      & Grammar.add "Term"   "num"    [cat "num"]

    first = FIRST.make grammar

  print grammar

  flip runStateT State.emptyReg $ do
    -- build tables
    goto   <- GOTO.make grammar first
    action <- ACTION.make goto

    let conflicts = ACTION.conflicts action

    -- check conflicts
    unless (null $ ACTION.unwrap conflicts) do
      log <- ACTION.dump "CONFLICTS" conflicts
      liftIO $ putStrLn log
      error "conflicts"

    -- lexing
    let
      t = Term.Term . Lexeme.Concrete
      d = Term.Term . Lexeme.Category

      lexer = (<> [(Term.EndOfStream, "")]) .  map lex . words

      lex s
        | Just (n :: Int) <- readMaybe s = (d "num", s)
        | otherwise                      = (t (Text.pack s), s)

    -- lex input
    let input = lexer "1 * 2 + 3 * ( 4 + 5 ) * 6"
    liftIO $ print input

    -- run parser
    res <- Parser.run goto action input
    liftIO $ print res

  return ()
