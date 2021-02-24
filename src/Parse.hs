
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
import Tree
import Term

{- | The core of the parser.
-}
parser
  :: forall term token
  .  (Ord term, Pretty term, Pretty token)
  => Act'  (Term term)   -- ^ ACTION table
  -> Goto  (Term term)   -- ^ GOTO table
  -> State (Term term)  -- ^ initial state
  -> [(term, token)]            -- ^ token stream
  -> Either Doc ([State (Term term)], [Tree token])
parser action goto start = (`go` ([start], []))
  where
    go
      :: [(term, token)]
      ->            ([State (Term term)], [Tree token])
      -> Either Doc ([State (Term term)], [Tree token])
    go input@((term, token) : restInput) (top : items, stack) = do
      -- traceShowM ("")
      -- traceShowM ("token", pretty token, pretty term)
      -- traceShowM ("state", top)
      -- traceShowM ("actions", pretty $ action ? top)
      -- traceShowM ("next", pretty $ action ? top ? Next term)
      case action ? top ? Next term of
        Shift    state -> go restInput (state : top : items, Leaf token : stack)
        Accept         -> throwExpected (Set.ofOne Eof) input
        Expected set   -> throwExpected set input
        Reduce   rule  -> do
          let len = ruleLength rule
          let top' : items' = drop len (top : items)
          let (taken, rest) = splitAt len stack
          go input
            ( goto top' (NonTerm (i1Name rule)) : top' : items'
            , Join (i1Reduce rule) (reverse taken) : rest
            )
        act@Conflict {} -> Left $ "uh-oh," <+> pretty act

    go [] (top : items, stack) =
      case action ? top ? Eof of
        Accept          -> return (top : items, stack)
        Expected set    -> throwExpected set []
        Reduce   rule  -> do
          let len = ruleLength rule
          let top' : items' = drop len (top : items)
          let (taken, rest) = splitAt len stack
          go []
            ( goto top' (NonTerm (i1Name rule)) : top' : items'
            , Join (i1Reduce rule) (reverse taken) : rest
            )
        act@Conflict {} -> Left $ "uh-oh," <+> pretty act
        _               -> throwExpected (Map.keySet (action ? top)) []

    go _ _ =
      error "parser: automata is corrupted"

    throwExpected
      :: Set (Term term)
      -> [(term, token)]
      -> Either Doc ([State (Term term)], [Tree token])
    throwExpected set input = Left
      $       "Expected" `indent` pretty (Set.ShortSet set)
      `above` "at"       `indent` pretty (ShortList    input)

{- | Collect a list of conflicts.
-}
reviewActions :: Pretty term => Act' term -> [Doc]
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
  => Table term -- ^ parsing table
  -> [(term, token)]   -- ^ token stream
  -> Either Doc (Tree token)
parse table = do
  let firsts'  = getFirsts            table
  let follows' = getFollows           table firsts'
  let initial  = getFirstStateOfTable table firsts' follows'
  let goto'    = getGoto              table firsts' follows'
  let points'  = getPoints            table
  let items'   = getItems             points' goto' initial
  let tokens'  = getTerminals         table
  let action'  = getAction            goto'
  let action1  = materialize (\s -> materialize (action' s) tokens') items'
  let action2  = populateExpectedTokens action1

  let conflicts = reviewActions action2

  if null conflicts
  then do
    \input -> do
      (_, result : _) <- parser action2 goto' initial input
      return result
  else do
    const $ Left $ vcat conflicts

