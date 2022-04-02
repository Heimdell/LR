{- |
  Driver for parsing in LR(1) mode.
-}
module LR1.GLR where

import Control.Monad (foldM)
import Control.Monad.Catch qualified as MTL
import Data.Data (Typeable)
import Data.List (intercalate)
import Data.Set qualified as Set
import Unsafe.Coerce (unsafeCoerce)

import LR1.ACTION qualified as ACTION
import LR1.Func   qualified as Func
import LR1.GOTO   qualified as GOTO
import LR1.Point  qualified as Point
import LR1.State  qualified as State
import LR1.Term   qualified as Term
import LR1.Utils (Get((?)))
import Data.Traversable (for)

{- |
  Thrown on unexpected terms.

  Show what terms were expected at what position.
-}
data Expected a pos = Expected { eTerms :: [Term.T], ePos :: pos, eTerm :: Term.T, eVal :: a }
  deriving anyclass (MTL.Exception)

newtype AnyOf a pos = AnyOf [Expected a pos]
  deriving anyclass (MTL.Exception)

instance (Show a, Show pos) => Show (Expected a pos) where
  show = \case
    Expected ts pos t a ->
      "Unexpected term " <> show a <> " (" <> show t <> ") at position " <> show pos <> ", expected any of those: " <> intercalate ", " (map show ts)

instance (Show a, Show pos) => Show (AnyOf a pos) where
  show = \case
    AnyOf [] -> "Unexpected parser failure"
    AnyOf [x] -> show x
    AnyOf (Expected ts p t a : rest) ->
      show (Expected (ts <> (eTerms =<< rest)) p t a)

type GLRState a t pos = Either [Expected t pos] ([State.Index], [a])
type GLRStates a t pos = [GLRState a t pos]

{- |
  Runs the parser. This is the simplest one - no recovery and conflict resolution.

  If /somehow/ given an ACTION table with an `ACTION.Conflict`, it would crash with
  `error`.
-}
run
  :: forall a t pos m
  .  ( Show t
     , Typeable t
     , Show pos
     , Typeable pos
     , State.HasReg m
     , MTL.MonadThrow m
     )
  => GOTO.T              -- ^ GOTO table.
  -> ACTION.T            -- ^ ACTION table.
  -> [(Term.T, pos, t)]  -- ^ Lexed output.
  -> m [a]
run goto action input = retract =<< foldM consumeAll [Right ([0], [])] input
  where
    retract :: GLRStates a t pos -> m [a]
    retract states = case split states of
      (exn, []) -> MTL.throwM $ AnyOf exn
      (_,   xs) -> return $ head . snd <$> xs

    split :: GLRStates a t pos -> ([Expected t pos], [([State.Index], [a])])
    split states = flip foldMap states \case
      Left exs -> (exs, [])
      Right x0 -> ([], [x0])

    consumeAll :: GLRStates a t pos -> (Term.T, pos, t) -> m (GLRStates a t pos)
    consumeAll states term = do
      let
        (lefts, rights) = split states

      if null rights
      then do
        MTL.throwM $ AnyOf lefts
      else do
        statess <- for rights (`consume` term)
        return $ mconcat statess

    consume :: ([State.Index], [a]) -> (Term.T, pos, t) -> m (GLRStates a t pos)
    consume (top : states, values) (term, pos, a) = do
      let expected = ACTION.expected action top

      if Set.member term expected
      then do
        perform (top : states, values) (term, pos, a) $ action ? (top, term)

      else do
        return [Left [Expected (Set.toList expected) pos term a]]

    consume _ _ = error "parser state is corrupted"

    perform :: ([State.Index], [a]) -> (Term.T, pos, t) -> ACTION.Action -> m (GLRStates a t pos)
    perform (top : states, values) (term, pos, a) act = case act of
      ACTION.Accept -> do
        return [Right (states, values)]

      ACTION.Reduce func entity size -> do
        let top' : states' = drop size (top : states)
        let (taken, rest) = splitAt size values
        let states'' = goto ? (top', Point.NonTerm entity) : top' : states'
        let values'  = Func.call func (reverse taken) : rest
        consume (states'', values') (term, pos, a)

      ACTION.Shift state -> do
        return [Right (state : top : states, unsafeCoerce a : values)]

      ACTION.Conflict l r -> do
        s1 <- perform (top : states, values) (term, pos, a) l
        s2 <- perform (top : states, values) (term, pos, a) r
        return (s1 <> s2)

      ACTION.Empty -> error "how?"

    perform _ _ _ = error "parser state is corrupted"
