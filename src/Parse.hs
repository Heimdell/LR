
module Parse where

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

parser
  :: forall term token result
  .  (Ord term, Pretty term, Pretty token)
  => Act' term result
  -> Goto term result
  -> (token -> result)
  -> (token -> term)
  -> Set (Item1 term result)
  -> [token]
  -> Either Doc ([Set (Item1 term result)], [result])
parser action goto wrap classify start = (`go` ([start], []))
  where
    go
      :: [token]
      ->            ([Set (Item1 term result)], [result])
      -> Either Doc ([Set (Item1 term result)], [result])
    go input@(token : restInput) (top : items, stack) = do
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
      -> Either Doc ([Set (Item1 term result)], [result])
    throwExpected set input = Left
      $       "Expected" `indent` pretty (Set.ShortSet set)
      `above` "at"       `indent` pretty (ShortList    input)

parse
  :: (Ord term, Pretty term, Pretty token)
  => Table term result
  -> (token -> result)
  -> (token -> term)
  -> term
  -> [token]
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

  (_, result : _) <- parser action2 goto' wrap classify initial input
  return result

instance MonadFail (Either Doc) where
  fail s = Left $ "fail:" <+> text s
