module LR1.Parser where

import Control.Monad (foldM, unless)
import Control.Monad.Catch qualified as MTL
import Data.Data (Typeable)
import Data.Function ((&))
import Data.Map (Map, (!))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tree (Tree (..), drawTree)

import LR1.ACTION qualified as ACTION
import LR1.Fixpoint (Get((?)))
import LR1.GOTO   qualified as GOTO
import LR1.Map    qualified as Map
import LR1.Point  qualified as Point
import LR1.State  qualified as State
import LR1.Term   qualified as Term
import LR1.Func   qualified as Func

data ParseTree a
  = Leaf a
  | Join Text [ParseTree a]
  deriving stock Functor

reduce :: Proxy b -> Map Text Func.T -> (a -> b) -> ParseTree a -> b
reduce p ctors tok = \case
  Leaf a -> tok a
  Join f args -> Func.call (ctors ! f) (map (reduce p ctors tok) args)

instance Show a => Show (ParseTree a) where
  show = drawTree . toTree . fmap show
    where
      toTree = \case
        Leaf a -> Node a []
        Join t ts -> Node (Text.unpack t) (map toTree ts)

data Expected a = Expected [Term.T] Term.T a
  deriving stock (Show)
  deriving anyclass (MTL.Exception)

run :: forall a m. (Show a, Typeable a) => (State.HasReg m, MTL.MonadThrow m) => GOTO.T -> ACTION.T -> [(Term.T, a)] -> m (ParseTree a)
run goto action = fmap (head . snd) <$> foldM consume ([0], [])
  where
    consume :: ([State.Index], [ParseTree a]) -> (Term.T, a) -> m ([State.Index], [ParseTree a])
    consume (top : states, values) (term, a) = do
      let expected = ACTION.expected action top

      unless (Map.member term expected) do
        MTL.throwM $ Expected (Map.keys expected) term a

      case action ? (top, term) of
        ACTION.Accept -> do
          return (states, values)

        ACTION.Reduce txt t n -> do
          let top' : states' = drop n (top : states)
          let (taken, rest) = splitAt n values
          let states'' = goto ? (top', Point.NonTerm t) : top' : states'
          let values'  = Join txt (reverse taken) : rest
          consume (states'', values') (term, a)

        ACTION.Shift n -> do
          return (n : top : states, Leaf a : values)

        ACTION.Conflict _ _ -> error "LR(1) parser can't handle conflicts"
        ACTION.Empty -> error "how?"

    consume _ _ = error "parser state is corrupted"
