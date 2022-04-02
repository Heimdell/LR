{- |
  Driver for parsing in LR(1) mode.
-}
module LR1.Parser where

import Control.Monad (unless)
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
import qualified Control.Monad.RWS as MTL
import Data.Foldable (for_)
import qualified Control.Monad.State as MTL

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
     , MTL.MonadReader (GOTO.T, ACTION.T) m
     , MTL.MonadThrow m
     , MTL.MonadFail m
     )
  => [(Term.T, pos, t)]  -- ^ Lexed output.
  -> m a
run input = do
  state' <- MTL.execStateT
    do for_ input consume
    ([0], [])

  return $ (head . snd) state'
  where
    consume :: (Term.T, pos, t) -> MTL.StateT ([State.Index], [a]) m ()
    consume (term, pos, a) = do
      (top : states, values) <- MTL.get
      (goto, action) <- MTL.ask

      let expected = ACTION.expected action top

      unless (Set.member term expected) do
        MTL.throwM $ Expected (Set.toList expected) pos term a

      case action ? (top, term) of
        ACTION.Accept -> do
          MTL.put (states, values)

        ACTION.Reduce func entity size -> do
          let top' : states' = drop size (top : states)
          let (taken, rest)  = splitAt size values
          let top''          = goto ? (top', Point.NonTerm entity)
          let states''       = top'' : top' : states'
          let values'        = Func.call func (reverse taken) : rest
          MTL.put (states'', values')
          consume (term, pos, a)

        ACTION.Shift state -> do
          MTL.put (state : top : states, unsafeCoerce a : values)

        ACTION.Conflict _ _ -> error "LR(1) driver can't handle conflicts"
        ACTION.Empty -> error "how?"
