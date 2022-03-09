module LR1.Parser where
import qualified LR1.State as State
import qualified LR1.Term as Term
import qualified LR1.ACTION as ACTION
import qualified LR1.GOTO as GOTO
import Data.Text (Text)
import Control.Monad (foldM, unless)
import LR1.Fixpoint (Get((?)))
import qualified LR1.Point as Point
import qualified Data.Text as Text
import Data.Tree
import Control.Monad.Catch
import qualified LR1.Map as Map
import Data.Data (Typeable)

data ParseTree a
  = Leaf a
  | Join Text [ParseTree a]
  deriving stock Functor

instance Show a => Show (ParseTree a) where
  show = drawTree . toTree . fmap show
    where
      toTree = \case
        Leaf a -> Node a []
        Join t ts -> Node (Text.unpack t) (map toTree ts)

data Expected a = Expected [Term.T] Term.T a
  deriving stock (Show)
  deriving anyclass (Exception)

run :: forall a m. (Show a, Typeable a) => (State.HasReg m, MonadThrow m) => GOTO.T -> ACTION.T -> [(Term.T, a)] -> m ([State.Index], [ParseTree a])
run goto action = foldM consume ([0], [])
  where
    consume :: ([State.Index], [ParseTree a]) -> (Term.T, a) -> m ([State.Index], [ParseTree a])
    consume (top : states, values) (term, a) = do
      let expected = ACTION.expected action top

      unless (Map.member term expected) do
        throwM $ Expected (Map.keys expected) term a

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
