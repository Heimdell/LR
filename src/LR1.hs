module LR1
  ( parse
  , compile
  , grammar
  , clauseS
  , clause
  , (&!), (&.), (&*), (&#), (&?), Rule(Reduce), sepBy
  ) where
import LR1.ETyped (Grammar (Grammar), grammar, clauseS, clause, (&!), (&.), (&*), (&#), (&?), Rule(Reduce), sepBy)
import LR1.Term qualified as Term
import qualified LR1.FIRST as FIRST
import qualified LR1.State as State
import qualified LR1.GOTO as GOTO
import qualified LR1.ACTION as ACTION
import Control.Monad.IO.Class
import qualified LR1.Parser as Parser
import Control.Monad
import Control.Monad.State (evalStateT, get, evalState)
import Data.Data (Typeable)
import Control.Monad.Catch (MonadThrow)

data Tables t a = Tables
  { action :: ACTION.T
  , goto   :: GOTO.T
  , reg    :: State.Reg
  }

compile :: Grammar t a -> Tables t a
compile (Grammar grammar') = flip evalState State.emptyReg  do
  let first = FIRST.make grammar'
  goto   <- GOTO.make grammar' first
  action <- ACTION.make goto
  reg    <- get
  return Tables {goto, action, reg}

parse :: (MonadIO m, MonadThrow m, Show t, Typeable t, Show pos, Typeable pos) => Tables t a -> [(Term.T, pos, t)] -> m a
parse Tables {action, goto, reg} input = do
  flip evalStateT reg do
    let conflicts = ACTION.conflicts action

    -- check conflicts
    unless (null $ ACTION.unwrap conflicts) do
      log' <- ACTION.dump "CONFLICTS" conflicts
      liftIO $ putStrLn log'
      error "conflicts"

    -- lexing
    Parser.run goto action input
