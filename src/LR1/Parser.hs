{- |
  Driver for parsing in LR(1) mode.
-}
module LR1.Parser where

import Control.Monad (foldM, unless)
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

{- |
  Thrown on unexpected terms.

  Show what terms were expected at what position.
-}
data Expected a pos = Expected [Term.T] pos Term.T a
  deriving anyclass (MTL.Exception)

instance (Show a, Show pos) => Show (Expected a pos) where
  show = \case
    Expected ts pos t a ->
      "Unexpected term " <> show a <> " (" <> show t <> ") at position " <> show pos <> ", expected any of those: " <> intercalate ", " (map show ts)

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
  -> m a
run goto action = fmap (head . snd) <$> foldM consume ([0], [])
  where
    consume :: ([State.Index], [a]) -> (Term.T, pos, t) -> m ([State.Index], [a])
    consume (top : states, values) (term, pos, a) = do
      let expected = ACTION.expected action top

      unless (Set.member term expected) do
        MTL.throwM $ Expected (Set.toList expected) pos term a

      case action ? (top, term) of
        ACTION.Accept -> do
          return (states, values)

        ACTION.Reduce func entity size -> do
          let top' : states' = drop size (top : states)
          let (taken, rest) = splitAt size values
          let states'' = goto ? (top', Point.NonTerm entity) : top' : states'
          let values'  = Func.call func (reverse taken) : rest
          consume (states'', values') (term, pos, a)

        ACTION.Shift state -> do
          return (state : top : states, unsafeCoerce a : values)

        ACTION.Conflict _ _ -> error "LR(1) parser can't handle conflicts"
        ACTION.Empty -> error "how?"

    consume _ _ = error "parser state is corrupted"
