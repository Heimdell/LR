module Driver.LR1 where

import Control.Monad                  (unless, foldM)
import Control.Monad.Except           (MonadError(throwError))
import Data.Foldable                  (toList)
import Data.Function                  ((&))
import Data.Set                       (Set)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), hang, vcat)

import Data.Set qualified as Set

import Data.Map.Monoidal ((!), type (==>))
import Decision          (Decision(..))
import Driver.ParseTree  (Tree(..))
import Grammar.Example   (grammarTest)
import Position          (startingPosition)
import Rule              (Rule(points, entity, reducer))
import State             (State, closure)
import Tables            (Action(goto, Action, action), Table(..), makeTables)
import Term              (Term)

import qualified Data.Map.Monoidal as Map

data ParseError a
  = Expected (Set Term) Term a
  deriving stock (Show)

instance MonadFail (Either a) where
  fail = error

consume :: Table -> ([State], [Tree a]) -> (Term, a) -> Either (ParseError a) ([State], [Tree a])
consume Table {actions} (top : stack, trees) (klass, term) = do
  let
    action :: Term ==> Set Decision
    Action {action} = actions ! top

    possible :: Set Term
    possible = action & Map.foldMapWithKey (const . Set.singleton)

  unless (Set.member klass possible) do
    throwError $ Expected possible klass term

  case toList (action ! klass) of
    [single] -> do
      case single of
        Accept -> pure (top : stack, trees)
        Shift state -> pure (state : top : stack, Atom term : trees)
        Reduce rule -> do
          let len = length rule.points
          case drop len (top : stack) of
            top' : stack' -> do
              let (reverse -> taken, trees') = splitAt len trees
              let top''                      = (actions ! top').goto ! rule.entity
              let stack''                    = top'' : top' : stack'
              let trees''                    = Reduced rule.reducer taken : trees'
              consume Table {actions} (stack'', trees'') (klass, term)

            _ -> error "Parser state is corrupted"
    conflict -> error $ show $ hang "Conflict:" 2 $ vcat $ map pPrint conflict

consume _ _ _ = error "No end-of-file $ token provided"

parse :: Table -> State -> [(Term, a)] -> Either (ParseError a) (Tree a)
parse tables initial tokens = do
  (_, [result]) <- foldM (consume tables) ([initial], []) tokens
  pure result

test :: Tree String
test = either (error . show) id do
  let starter = closure grammarTest (Set.singleton startingPosition)
  let tables = makeTables grammarTest starter
  parse tables starter input
  where
    input =
      [ ("(",      "(")
      , ("number", "1")
      , ("+",      "+")
      , ("number", "2")
      , (")",      ")")
      , ("*",      "*")
      , ("number", "3")
      , ("$",      "$")
      ]
