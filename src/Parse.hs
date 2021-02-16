
{- | The parsring algorithm.

-}
module Parse where

import Control.Monad (unless)

import Map qualified as Map
import Set (Set)
import Set qualified as Set

import Action
import Item
import Goto
import Point
import Pretty
import Util
import Table

{- | The core of the parser.
-}
parser
  :: forall term token result
  .  (Ord term, Pretty term, Pretty token)
  => Act' term result   -- ^ ACTION table
  -> Goto term result   -- ^ GOTO table
  -> (token -> result)  -- ^ S-expr atom wrapper
  -> (token -> term)    -- ^ token classifier
  -> State term result  -- ^ initial state
  -> [token]            -- ^ token stream
  -> Either Doc ([State term result], [result])
parser action goto wrap classify start = (`go` ([start], []))
  where
    go
      :: [token]
      ->            ([State term result], [result])
      -> Either Doc ([State term result], [result])
    go input@(token : restInput) (top : items, stack) = do
      -- traceShowM ("====")
      -- traceShowM ("token", pretty token)
      -- traceShowM ("state", top)
      -- traceShowM ("actions", action ? top)
      case action ? top ? classify token of
        Shift    state -> go restInput (state : top : items, wrap token : stack)
        Accept         -> return       (        top : items,              stack)
        Expected set   -> throwExpected set input
        Reduce   rule  -> do
          let len = ruleLength rule
          let top' : items' = drop len (top : items)
          let (taken, rest) = splitAt len stack
          go input
            ( goto top' (NonTerm (i1Name rule)) : top' : items'
            , i1Reduce rule (reverse taken) : rest
            )
        act@Conflict {} -> Left $ "uh-oh," <+> pretty act

    go [] (top : _, _) =
      throwExpected (Map.keySet (action ? top)) []

    go _ _ =
      error "parser: automata is corrupted"

    throwExpected
      :: Set term
      -> [token]
      -> Either Doc ([State term result], [result])
    throwExpected set input = Left
      $       "Expected" `indent` pretty (Set.ShortSet set)
      `above` "at"       `indent` pretty (ShortList    input)

{- | Collect a list of conflicts.
-}
reviewActions :: Pretty term => Act' term result -> [Doc]
reviewActions actions = do
  (`foldMap` Map.toList actions) \(state, decisions) -> do
    (`foldMap` Map.toList decisions) \(term, action) -> do
      case action of
        Conflict {} -> do
          pure
            $       "Conflict at state" `indent` pretty state
            `above` "at token"          `indent` pretty term
            `above` ":"                 `indent` pretty action
        _ -> mempty

{- | Run parser, return result or produce an error.
-}
parse
  :: (Ord term, Pretty term, Pretty token)
  => Table term result  -- ^ parsing table
  -> (token -> result)  -- ^ wrapper into an S-expr
  -> (token -> term)    -- ^ gets token type
  -> term               -- ^ eof marker
  -> [token]            -- ^ token stream
  -> Either Doc result
parse table wrap classify eof input = do
  let firsts'  = getFirsts table
  let follows' = getFollows firsts' table eof
  let initial  = getFirstStateOfTable table firsts' follows' eof
  let goto'    = getGoto table firsts' follows'
  let points'  = getPoints table
  let items'   = getItems points' goto' initial
  let tokens'  = getTerminals table eof
  let action'  = getAction goto' eof
  let action1  = materialize (\s -> materialize (action' s) tokens') items'
  let action2  = populateExpectedTokens action1

  let conflicts = reviewActions action2

  unless (null conflicts) do
    Left $ vcat conflicts

  -- traceShowM action2

  (_, result : _) <- parser action2 goto' wrap classify initial input
  return result

instance MonadFail (Either Doc) where
  fail s = Left $ "fail:" <+> text s
