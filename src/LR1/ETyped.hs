{-# OPTIONS_GHC -Wno-orphans #-}

module LR1.ETyped where
import LR1.Term qualified as Term
import LR1.NonTerm qualified as NonTerm
import LR1.ERule qualified as ERule
import LR1.Point qualified as Point
import LR1.Func qualified as Func
import qualified Control.Monad.State as MTL
import qualified Control.Monad.Writer as MTL
import qualified LR1.Rule as Rule
import qualified Data.Text as Text
import Data.Foldable (for_)
import qualified LR1.Grammar as Grammar
import Data.Functor ((<&>))

data Rule t a where
  Reduce :: a -> Rule t a
  (:!)   :: Rule t (a -> b) -> Point t a -> Rule t b
  (:.)   :: Rule t b        -> Term.T    -> Rule t b

infixl 1 :!, :.

data Point t a where
  P    :: Pt t a -> Point t a
  Some :: Pt t a -> Point t [a]
  Many :: Pt t a -> Point t [a]
  Opt  :: Pt t a -> Point t (Maybe a)

data Pt t a where
  T :: Term.T     -> Pt t t
  E :: Entity t a -> Pt t a

newtype Entity t a = Entity NonTerm.T

newtype Grammar t a = Grammar Grammar.T

compile :: Rule t a -> ([ERule.EPoint], Func.T)
compile = \case
  Reduce a -> ([], Func.func a [])
  ru :! po ->
    (points <> [point], f)
      where
        (points, f) = compile ru
        point       = compilePoint po
  ru :. t ->
    (points <> [ERule.The_ $ Point.Term t], f)
      where
        (points, f) = compile ru

compilePoint :: Point t a -> ERule.EPoint
compilePoint = \case
  P    pt -> ERule.The  (compilePt pt)
  Some pt -> ERule.Some (compilePt pt)
  Many pt -> ERule.Many (compilePt pt)
  Opt  pt -> ERule.Opt  (compilePt pt)

compilePt :: Pt t a1 -> Point.T
compilePt = \case
  T t -> Point.Term t
  E (Entity en) -> Point.NonTerm en

fresh :: MTL.MonadState Int m => m String
fresh = do
  MTL.modify (+ 1)
  MTL.get <&> show

clause :: forall a t m. (MTL.MonadState Int m, MTL.MonadWriter [Rule.T] m) => [Rule t a] -> m (Entity t a)
clause cases = do
  name <- fresh
  let ename = NonTerm.NonTerm (Text.pack name)
  for_ cases \rule -> do
    let (epoints, elabel) = compile rule
    MTL.tell $ ERule.compile [ERule.ERule {ename, epoints, elabel}]
  return (Entity ename)

clauseS :: forall a t m. (MTL.MonadState Int m, MTL.MonadWriter [Rule.T] m) => Entity t a -> m (Entity t a)
clauseS ent = do
  let (epoints, elabel) = compile $ Reduce id :! P (E ent)
  MTL.tell $ ERule.compile [ERule.ERule {ename = NonTerm.Start, epoints, elabel}]

  return $ Entity NonTerm.Start

grammar
  :: forall a t
  .  MTL.StateT Int (MTL.Writer [Rule.T]) (Entity t a)
  -> Grammar t a
grammar action = Grammar g
  where
    g = Grammar.fromRules $ MTL.execWriter (MTL.evalStateT action 0)

class ToPoint p t a | p t a -> t a where
  toPoint :: p -> Point t a

instance ToPoint (Point t a) t a where
  toPoint = id

instance ToPoint Term.T t t where
  toPoint = P . T

instance ToPoint (Entity t a) t a where
  toPoint = P . E

class ToPt p t a | p t -> t a where
  toPt :: p -> Pt t a

instance ToPt Term.T t t where
  toPt = T

instance ToPt (Entity t a) t a where
  toPt = E

some :: Entity t a -> Point t [a]
some = Some . toPt

many :: Entity t a -> Point t [a]
many = Many . toPt

opt :: Entity t a -> Point t (Maybe a)
opt = Opt . toPt

(&!) :: Rule t (a -> b) -> Entity t a -> Rule t b
l &! r = l :! toPoint r

(&+) :: Rule t ([a] -> b) -> Entity t a -> Rule t b
l &+ r = l :! Some (toPt r)

(&*) :: Rule t ([a] -> b) -> Entity t a -> Rule t b
l &* r = l :! Many (toPt r)

(&?) :: Rule t (Maybe a -> b) -> Entity t a -> Rule t b
l &? r = l :! Opt (toPt r)

(&.) :: Rule t b -> Term.T -> Rule t b
l &. r = l :. r

(&#) :: Rule t (t -> b) -> Term.T -> Rule t b
l &# r = l :! toPoint r

sepBy :: (MTL.MonadState Int m, MTL.MonadFix m, MTL.MonadWriter [Rule.T] m) => Entity t a -> Term.T -> m (Entity t [a])
sepBy p sep = mdo
  expr <- clause
    [ Reduce pure &! p
    , Reduce (:)  &! p &. sep &! expr
    ]
  return expr
