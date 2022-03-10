
module LR1.Typed where

import Control.Arrow qualified as Arrow
import Control.Monad.State qualified as MTL
import Control.Monad.Writer qualified as MTL
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Unsafe.Coerce (unsafeCoerce)

import LR1.Point   qualified as Point
import LR1.Term    qualified as Term
import LR1.NonTerm qualified as NonTerm
import LR1.Rule    qualified as Rule
import LR1.Grammar qualified as Grammar
import LR1.Func    qualified as Func
import LR1.Lexeme  qualified as Lexeme

newtype Entity a = Entity NonTerm.T

data Rule t a where
  T  :: Rule t (t -> a) -> Term.T   -> Rule t a
  -- T_ :: Rule t       a  -> Term.T   -> Rule t a
  C  :: Rule t (t -> a) -> Text     -> Rule t a
  E  :: Rule t (a -> b) -> Entity a -> Rule t b
  R  :: a                           -> Rule t a

noWrap :: Rule t (a -> a)
noWrap = R id

fresh :: MTL.MonadState Int m => m String
fresh = do
  MTL.modify (+ 1)
  MTL.get <&> show

clause :: forall a t m. (MTL.MonadState Int m, MTL.MonadWriter ([Rule.T], Map Text Func.T) m) => [Rule t a] -> m (Entity a)
clause cases = do
  name <- fresh
  let entity = NonTerm.NonTerm (Text.pack name)
  for_ (zip [0 :: Int ..] cases) \(i, rule) -> do
    let (reverse -> points, f) = toPoints rule
    let label       = Text.pack (name <> "-" <> show i)
    MTL.tell
      ( [Rule.Rule {entity, label, points}]
      , Map.singleton label f
      )

  return $ Entity entity

clauseS :: forall a m. (MTL.MonadState Int m, MTL.MonadWriter ([Rule.T], Map Text Func.T) m) => Entity a -> m (Entity a)
clauseS ent = do
  name <- fresh
  let entity = NonTerm.Start
  let (points, f) = toPoints (noWrap `E` ent)
  let label       = Text.pack name
  MTL.tell
    ( [Rule.Rule {entity, label, points}]
    , Map.singleton label f
    )

  return $ Entity entity

toPoints :: Rule t a -> ([Point.T], Func.T)
toPoints = \case
  T k t -> do
    case toPoints k of
      (pts, Func.Func f) ->
        (Point.Term t : pts, Func.Func f)

  -- T_ k t -> do
  --   case toPoints k of
  --     (pts, Func.Func f) ->
  --       (Point.Term t : pts, Func.Func (unsafeCoerce (const f))

  C k t -> do
    case toPoints k of
      (pts, Func.Func f) ->
        (Point.Term (Term.Term (Lexeme.Category t)) : pts, Func.Func f)

  E k (Entity e) -> do
    case toPoints k of
      (pts, Func.Func f) ->
        (Point.NonTerm e : pts, Func.Func f)

  R a -> do
    ([], Func.Func (unsafeCoerce a))

grammar
  :: forall a
  .  MTL.StateT Int (MTL.Writer ([Rule.T], Map Text Func.T)) (Entity a)
  -> (Grammar.T, Map Text Func.T, Proxy a)
grammar action = (g, m, Proxy)
  where
    (g, m) = Arrow.first Grammar.fromRules $ MTL.execWriter (MTL.evalStateT action 0)
