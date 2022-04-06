{- |
  Library interface.

  Provides DSL to describe grammar, generate tables and parser runner.

  Here is an example of how to use this module (also contains a makeshift lexer).

  @
      import Data.Maybe (fromMaybe)

      import LR1.Term qualified as Term
      import LR1
      import Data.String (IsString(fromString))

      data JSON
        = Array  [JSON]
        | Object [(String, JSON)]
        | String   String
        | Null
        deriving stock Show

      lexer :: String -> [(Term.T, (), String)]
      lexer = (<> [(Term.EndOfStream, (), "")]) . map lex' . words
        where
          lex' = \case
            s@('"'  : _) -> (Term.Term "string", (), s)
            s@('\'' : _) -> (Term.Term "string", (), s)
            s            -> (fromString s,       (), s)

      main :: IO ()
      main = do
        let
          list = fromMaybe []

          json = LR1.compile $ LR1.grammar mdo
            expr <- LR1.clause @JSON
              [ Reduce Array  &! array
              , Reduce Object &! object
              , Reduce String &# "string"
              , Reduce Null   &. "null"
              ]

            array <- LR1.clause @[JSON]
              [ Reduce list &. "[" &? exprs &. "]"
              ]

            object <- LR1.clause @[(String, JSON)]
              [ Reduce list &. "{" &? pairs &. "}"
              ]

            pair <- LR1.clause @(String, JSON)
              [ Reduce (,) &# "string" &. ":" &! expr
              ]

            exprs <- LR1.sepBy expr ","
            pairs <- LR1.sepBy pair ","

            return expr

        -- lex input
        putStrLn "Input something, like \"[ 'a' , null , { 'b' : [ ] , 'c' : null } , 'd' ]\""
        str <- getLine
        let input = lexer str
        print input

        -- run parser
        res <- LR1.parse json input
        print res

        return ()
  @

  This is verbatim code of @.//app//Main.hs@ module.
-}
module LR1
  ( -- * DSL
    grammar
  , clause
  , (&!)
  , (&.)
  , (&*)
  , (&#)
  , (&?)
  , Rule(Reduce)
  , sepBy

    -- * Runners
  , parse
  , compile
  , Tables
  ) where
import LR1.ETyped (Grammar (Grammar), grammar, clause, (&!), (&.), (&*), (&#), (&?), Rule(Reduce), sepBy)
import LR1.Term qualified as Term
import qualified LR1.Grammar as Grammar
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
import Control.Monad.Reader (ReaderT(runReaderT))
import qualified LR1.Func as Func
import Debug.Trace (traceShowM)

{- |
  ACTION/GOTO tables and state registry for parser to use.
-}
data Tables t a = Tables
  { action :: ACTION.T
  , goto   :: GOTO.T
  , reg    :: State.Reg
  }

{- |
  Build ACTION/GOTO tables and state registry.
-}
compile :: Grammar t a -> Tables t a
compile (Grammar grammar') = flip evalState State.emptyReg  do
  -- traceShowM grammar'
  let first = FIRST.make grammar'
  -- traceShowM first
  goto   <- GOTO.make grammar' first
  action <- ACTION.make (Grammar.s grammar') goto
  reg    <- get
  return Tables {goto, action, reg}

{- |
  Run parser using prebuilt tables on list of triples (lexemeType, position, lexeme).
-}
parse :: (MonadIO m, MonadThrow m, MonadFail m, Show t, Typeable t, Show pos, Typeable pos, Typeable a) => Tables t a -> [(Term.T, pos, t)] -> m a
parse Tables {action, goto, reg} input = do
  flip evalStateT reg do
    let conflicts = ACTION.conflicts action

    -- check conflicts
    unless (null $ ACTION.unwrap conflicts) do
      log' <- ACTION.dump "CONFLICTS" conflicts
      liftIO $ putStrLn log'
      error "conflicts"

    -- lexing
    dyn <- runReaderT (Parser.run input) (goto, action)
    return $ Func.fromDynUnsafe dyn
