{-# OPTIONS_GHC -Wno-orphans #-}

{- |
  Typed interface with extensions for LR(1)-grammars.
-}

module LR1.ETyped where

import Control.Monad.State qualified as MTL
import Control.Monad.Writer qualified as MTL
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.String (fromString)

import LR1.ERule   qualified as ERule
import LR1.Func    qualified as Func
import LR1.Grammar qualified as Grammar
import LR1.NonTerm qualified as NonTerm
import LR1.Point   qualified as Point
import LR1.Rule    qualified as Rule
import LR1.Term    qualified as Term

{- |
  A typed representation for rules.
-}
data Rule t a where
  -- | Reducing action, starts the rule.
  Reduce
    :: a         -- ^ The reducer function or a result.
    -> Rule t a

  -- | Add meaningful point.
  (:!)
    :: Rule t (a -> b)  -- ^ Rule.
    -> Point t a        -- ^ A point to glue.
    -> Rule t b

  -- | Add decirative term.
  (:.)
    :: Rule t b  -- ^ Rule.
    -> Term.T    -- ^ A term to glue.
    -> Rule t b

infixl 1 :!, :.

{- |
  A typed representation for extended points.
-}
data Point t a where
  P    :: Pt t a -> Point t a          -- ^ Normal point.
  Some :: Pt t a -> Point t [a]        -- ^ Point with Kleene plus.
  Many :: Pt t a -> Point t [a]        -- ^ Point with Kleene star.
  Opt  :: Pt t a -> Point t (Maybe a)  -- ^ Optional point.

{- |
  A typed representation for normal points.
-}
data Pt t a where
  T :: Term.T     -> Pt t t
  E :: Entity t a -> Pt t a

{- |
  An entity with its resulting type and term type fixed.
-}
newtype Entity t a = Entity NonTerm.T

{- |
  A grammar with its resulting type and term type fixed.
-}
newtype Grammar t a = Grammar Grammar.T

{- |
  Turn typed rules into untyped one.
-}
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
  where
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

{- |
  Create rules for entity, given a list of alternative parses.
-}
clause :: forall a t m. (MTL.MonadState Int m, MTL.MonadWriter [Rule.T] m) => [Rule t a] -> m (Entity t a)
clause cases = do
  name <- fromString <$> fresh
  for_ cases \rule -> do
    let (points, label) = compile rule
    MTL.tell $ ERule.compile [ERule.ERule {name, points, label}]
  return (Entity name)
  where
    fresh :: MTL.MonadState Int m => m String
    fresh = do
      MTL.modify (+ 1)
      MTL.get <&> show

{- |
  Create `NonTerm.Start`  rule that redirects to root entity.

  That root entity will be the result of the whole grammar.
-}
clauseS :: forall a t m. (MTL.MonadState Int m, MTL.MonadWriter [Rule.T] m) => Entity t a -> m (Entity t a)
clauseS ent = do
  let (points, label) = compile $ Reduce id :! P (E ent)
  MTL.tell $ ERule.compile [ERule.ERule {name = NonTerm.Start, points, label}]

  return $ Entity NonTerm.Start

{- |
  Pack a grammar into the ruleset.

  You need to return the the root entity from the DSL. The result of parsing
  will be that entity, parsed.
-}
grammar
  :: forall a t
  .  MTL.StateT Int (MTL.Writer [Rule.T]) (Entity t a)
  -> Grammar t a
grammar action = Grammar g
  where
    g = Grammar.fromRules $ MTL.execWriter (MTL.evalStateT (action >>= clauseS) 0)

{- |
  Glue entity to end of the rule.
-}
(&!) :: Rule t (a -> b) -> Entity t a -> Rule t b
l &! r = l :! P (E r)

{- |
  Glue 1-or-more entity to end of the rule.
-}
(&+) :: Rule t ([a] -> b) -> Entity t a -> Rule t b
l &+ r = l :! Some (E r)

{- |
  Glue 0-or-more entity to end of the rule.
-}
(&*) :: Rule t ([a] -> b) -> Entity t a -> Rule t b
l &* r = l :! Many (E r)

{- |
  Glue 0-or-1 entity to end of the rule.
-}
(&?) :: Rule t (Maybe a -> b) -> Entity t a -> Rule t b
l &? r = l :! Opt (E r)

{- |
  Glue decorative (ignored) term to the end of the rule.
-}
(&.) :: Rule t b -> Term.T -> Rule t b
l &. r = l :. r

{- |
  Glue captured term to the end of the rule.
-}
(&#) :: Rule t (t -> b) -> Term.T -> Rule t b
l &# r = l :! P (T r)

{- |
  Produce rule that works like standard @sepBy@ combinator in @parsec@.

  This would generate an entity.
-}
sepBy :: (MTL.MonadState Int m, MTL.MonadFix m, MTL.MonadWriter [Rule.T] m) => Entity t a -> Term.T -> m (Entity t [a])
sepBy p sep = mdo
  expr <- clause
    [ Reduce pure &! p
    , Reduce (:)  &! p &. sep &! expr
    ]
  return expr
