{-# OPTIONS_GHC -Wno-orphans #-}

{- |
  Typed interface with extensions for LR(1)-grammars.
-}

module LR1.ETyped where

import Control.Monad.Writer qualified as MTL
import Data.Foldable (for_)
import Data.String (fromString)

import LR1.Func    qualified as Func
import LR1.Grammar qualified as Grammar
import LR1.Point   qualified as Point
import LR1.Rule    qualified as Rule
import LR1.Term    qualified as Term
import Data.Data (Typeable, Proxy (Proxy))
import Control.Monad.Writer (WriterT)
import Data.Set (Set)
import Data.Dynamic (Dynamic, toDyn)
import LR1.Func ( Act(No, Drop, Id, None) )
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Typeable (typeRep)
import Control.Applicative ( Alternative((<|>)) )
import Data.Text (Text)

{- |
  A typed representation for rules.
-}
data Rule t a where
  -- | Reducing action, starts the rule.
  Reduce
    :: Typeable a => a         -- ^ The reducer function or a result.
    -> Rule t a

  -- | Add meaningful point.
  (:!)
    :: ( Typeable a
       , Typeable b
       )
    => Rule t (a -> b)  -- ^ Rule.
    -> Point t a        -- ^ A point to glue.
    -> Rule t b

  -- | Add decirative term.
  (:.)
    :: Typeable b
    => Rule t b  -- ^ Rule.
    -> Term.T    -- ^ A term to glue.
    -> Rule t b

infixl 1 :!, :.

{- |
  A typed representation for extended points.
-}
data Point t a where
  T    :: Term.T     -> Point t t
  E    :: Entity t a -> Point t a
  Some :: Typeable a => Entity t a -> Point t (NE.NonEmpty a)        -- ^ Point with Kleene plus.
  Many :: Typeable a => Entity t a -> Point t [a]        -- ^ Point with Kleene star.
  Opt  :: Typeable a => Entity t a -> Point t (Maybe a)  -- ^ Optional point.

{- |
  A typed representation for normal points.
-}
data Pt t a where

{- |
  An entity with its resulting type and term type fixed.
-}
newtype Entity t a = Entity { unEntity :: Text }

{- |
  A grammar with its resulting type and term type fixed.
-}
newtype Grammar t a = Grammar Grammar.T

type M = WriterT (Set Rule.T) []

compileRule :: forall a t. Typeable a => Rule t a -> M ()
compileRule rule = do
  (args, acts, f) <- compile rule
  MTL.tell $ Set.singleton $ Rule.Rule (unEntity (entityOf @a)) (reverse args) (Func.func f (reverse acts))

entityOf :: forall a t. Typeable a => Entity t a
entityOf = Entity $ fromString $ show $ typeRep (Proxy :: Proxy a)

compile :: forall a t. Typeable a => Rule t a -> M ([Point.T], [Func.Act], Dynamic)
compile = \case
  Reduce a -> do
    return ([], [], toDyn a)

  ru :! po -> do
    (points, acts, f) <- compile ru
    (point,  act)     <- compilePoint po
    return (point ++ points, act : acts, f)

  ru :. t -> do
    (points, acts, f) <- compile ru
    return (Point.Term t : points, Drop : acts, f)

single :: a -> NE.NonEmpty a
single a = a NE.:| []

compilePoint :: forall a t. Typeable a => Point t a -> M ([Point.T], Func.Act)
compilePoint = \case
  T t  -> return ([Point.Term t], Id)
  E en -> return ([Point.NonTerm $ unEntity en], Id)

  LR1.ETyped.Some e -> do
    entity <- makeSome e
    return ([Point.NonTerm $ unEntity entity], Id)

  Many e -> do
    entity <- makeSome e
    choose
      [ return ([],                                None      (Proxy :: Proxy [a]))
      , return ([Point.NonTerm $ unEntity entity], Func.Some (Proxy :: Proxy [a]))
      ]
  Opt en -> do
    choose
      [ return ([],                            No (Proxy :: Proxy (Maybe a)))
      , return ([Point.NonTerm $ unEntity en], Func.FJust)
      ]
  where
    makeSome :: forall b. Typeable b => Entity t b -> M (Entity t (NE.NonEmpty b))
    makeSome e@(Entity nt) = do
      let entity = Entity $ nt <> "-some"
      compileRule $ Reduce single  :! E e
      compileRule $ Reduce NE.cons :! E e :! E entity
      return entity

choose :: Alternative f => [f a] -> f a
choose = foldr1 (<|>)


{- |
  Create rules for entity, given a list of alternative parses.
-}
clause :: forall a t. (Typeable a) => [Rule t a] -> M (Entity t a)
clause cases = do
  for_ cases compileRule
  return $ entityOf @a


-- {- |
--   Pack a grammar into the ruleset.

--   You need to return the the root entity from the DSL. The result of parsing
--   will be that entity, parsed.
-- -}
grammar
  :: forall a t
  .  Typeable a
  => M (Entity t a)
  -> Grammar t a
grammar action = Grammar g
  where
    g = Grammar.fromRules (unEntity $ fst $ head res) (Set.toList (mconcat $ map snd res))
    res = MTL.runWriterT action


{- |
  Glue entity to end of the rule.
-}
(&!) :: (Typeable a, Typeable b) => Rule t (a -> b) -> Entity t a -> Rule t b
l &! r = l :! E r

{- |
  Glue 1-or-more entity to end of the rule.
-}
(&+) :: (Typeable a, Typeable b) => Rule t (NE.NonEmpty a -> b) -> Entity t a -> Rule t b
l &+ r = l :! LR1.ETyped.Some r

{- |
  Glue 0-or-more entity to end of the rule.
-}
(&*) :: (Typeable a, Typeable b) => Rule t ([a] -> b) -> Entity t a -> Rule t b
l &* r = l :! Many r

{- |
  Glue 0-or-1 entity to end of the rule.
-}
(&?) :: (Typeable a, Typeable b) => Rule t (Maybe a -> b) -> Entity t a -> Rule t b
l &? r = l :! Opt r

{- |
  Glue decorative (ignored) term to the end of the rule.
-}
(&.) :: (Typeable b) => Rule t b -> Term.T -> Rule t b
l &. r = l :. r

{- |
  Glue captured term to the end of the rule.
-}
(&#) :: (Typeable t, Typeable b) => Rule t (t -> b) -> Term.T -> Rule t b
l &# r = l :! T r

{- |
  Produce rule that works like standard @sepBy@ combinator in @parsec@.
  This would generate an entity.
-}
sepBy :: (Typeable a) => Entity t a -> Term.T -> M (Entity t (NE.NonEmpty a))
sepBy p sep = do
  clause
    [ Reduce single &! p
    , Reduce (NE.:|)  &! p &. sep &! entityOf
    ]
