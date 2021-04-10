
module Typed where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer

import Data.Proxy
import Data.String (IsString (..))
import Data.Typeable
import Data.Traversable

import Pretty
import Set
import Exts
import Table
import Rule
import Point
import S
import Map
import Util

data TRule l t f where
  TRule :: TPoint l t f -> (f -> g) -> TRule l t g

data TPoint l t f where
  Keyword :: l             -> TPoint l t ()
  T       :: l             -> TPoint l t t
  NT      :: Entity g      -> TPoint l t g
  Plus    :: TPoint l t f  -> TPoint l t [f]
  Star    :: TPoint l t f  -> TPoint l t [f]
  Opt     :: TPoint l t f  -> TPoint l t (Maybe f)
  Or      :: TPoint l t f  -> TPoint l t f  -> TPoint l t f
  And     :: TPoint l t f  -> TPoint l t g  -> TPoint l t (f, g)
  Before  :: TPoint l t () -> TPoint l t g  -> TPoint l t g
  After   :: TPoint l t f  -> TPoint l t () -> TPoint l t f
  SepBy   :: TPoint l t f  -> TPoint l t () -> TPoint l t [f]
  SepBy1  :: TPoint l t f  -> TPoint l t () -> TPoint l t [f]

deriving stock instance Eq  (Entity f)
deriving stock instance Ord (Entity f)
deriving stock instance Eq  l => Eq  (TPoint' l t f)
deriving stock instance Ord l => Ord (TPoint' l t f)

instance Eq l => Eq (SomePoint l t f) where
  SomePoint l _ == SomePoint r _ = eqPoint l r
    where
      eqPoint :: forall l t f g. Eq l => TPoint' l t f  -> TPoint' l t g -> Bool
      eqPoint  Stop'      Stop'       = True
      eqPoint (K'  k rs) (K'  k' rs') =      k ==      k' && rs `eqPoint` rs'
      eqPoint (T'  t rs) (T'  t' rs') =      t ==      t' && rs `eqPoint` rs'
      eqPoint (NT' e rs) (NT' e' rs') = eNum e == eNum e' && rs `eqPoint` rs'
      eqPoint  _          _           = False

instance Ord l => Ord (SomePoint l t f) where
  SomePoint l _ `compare` SomePoint r _ = cmpPoint l r
    where
      cmpPoint :: forall l t f g. Ord l => TPoint' l t f  -> TPoint' l t g -> Ordering
      cmpPoint  Stop'      Stop'       = EQ
      cmpPoint  Stop'      _           = LT
      cmpPoint  _          Stop'       = GT
      cmpPoint (K'  k rs) (K'  k' rs') =      k `compare` k'      <> rs `cmpPoint` rs'
      cmpPoint  K'  {}     _           = LT
      cmpPoint  _          K'  {}      = GT
      cmpPoint (T'  t rs) (T'  t' rs') =      t `compare` t'      <> rs `cmpPoint` rs'
      cmpPoint  T'  {}     _           = LT
      cmpPoint  _          T'  {}      = GT
      cmpPoint (NT' e rs) (NT' e' rs') = eNum e `compare` eNum e' <> rs `cmpPoint` rs'

data TRule' l t f where
  TRule' :: Set (SomePoint l t f) -> TRule' l t f

data TPoint' l t f where
  Stop' ::             TPoint' l t ()
  K'    ::        l -> TPoint' l t f -> TPoint' l t  f
  T'    ::        l -> TPoint' l t f -> TPoint' l t (t, f)
  NT'   :: Entity g -> TPoint' l t f -> TPoint' l t (g, f)

data Entity g where
  Entity :: { eNum :: Int } -> Entity g

data SomeRule l t where
  SomeRule :: TRule' l t f -> Entity f -> SomeRule l t
  deriving Show via PP (SomeRule l t)

data SomePoint l t f where
  SomePoint :: TPoint' l t g -> (g -> f) -> SomePoint l t f

instance Functor (SomePoint l t) where
  fmap f (SomePoint tp k) = SomePoint tp (f . k)

instance Pretty l => Pretty (SomeRule l t) where
  pretty (SomeRule tr n) =
    pretty (eNum n) <+> "="
      `indent` pretty tr

instance Pretty l => Pretty (TRule' l t f) where
  pretty (TRule' sps) = vcat $ fmap pretty $ Set.toList sps

instance Pretty l => Pretty (SomePoint l t f) where
  pretty (SomePoint pt _k) = "|" <+> pretty pt

instance Pretty l => Pretty (TPoint' l t f) where
  pretty  Stop'             = ""
  pretty (K'          k  r) = pretty k <+> pretty r
  pretty (T'          l  r) = pretty l <+> pretty r
  pretty (NT' (Entity e) r) = pretty e <+> pretty r

type M' l t = WriterT [SomeRule l t] (State Int)

grammar :: M' l t (TPoint l t f) -> (Int, [SomeRule l t])
grammar = flip evalState 0 . runWriterT . fmap (eNum . fromNT)


fromNT (NT nt) = nt

rule :: (Ord l, Pretty l) => (f -> g) -> TPoint l t f -> M' l t (TPoint l t g)
rule ctor pt = do
  pts <- point pt
  e   <- newRule $ TRule' $ Set.fromList $ (fmap.fmap) ctor pts
  return (NT e)

newRule
  :: forall f l t m
  .  (MonadState Int m, MonadWriter [SomeRule l t] m, Pretty l)
  => TRule' l t f
  -> m (Entity f)
newRule r = do
  counter <- get
  tell [SomeRule r (Entity counter)]
  modify (+ 1)
  return (Entity counter)

point
  :: forall f g l t
  .  (Ord l, Pretty l)
  => TPoint l t f
  -> M' l t [SomePoint l t f]
point p = case p of
  Keyword    t  -> return [SomePoint (K'  t Stop') (const ())]
  T          t  -> return [SomePoint (T'  t Stop') fst]
  NT         g  -> return [SomePoint (NT' g Stop') fst]
  Plus       p  -> do
    sps <- point p
    for sps \sp -> do
      e  <- newRule $ TRule' $ Set.ofOne sp
      rec
        e1 <- newRule $ TRule' $ Set.fromList
          [ SomePoint (NT' e (NT' e1 Stop')) \(f, (fs, ())) ->  f : fs
          , SomePoint (NT' e         Stop')  \(f,      ())  -> [f]
          ]
      return (SomePoint (NT' e1 Stop') fst)

  Star p -> do
    e <- point (Plus p)
    return $ e ++ [SomePoint Stop' (const [])]

  Opt p -> do
    es <- point p
    return $ (fmap.fmap) Just es ++ [SomePoint Stop' (const Nothing)]

  Or l r -> do
    e1 <- point l
    e2 <- point r
    return $ e1 ++ e2

  And l r -> do
    els <- point l
    concat <$> for els \el -> do
      els2 <- point r
      for els2 \(SomePoint e2 back2) -> do
        e <- newRule $ TRule' $ Set.ofOne el
        return (SomePoint (NT' e e2) \(f, g) -> (f, back2 g))

  Before l r -> do
    sps <- point (And l r)
    return $ (fmap.fmap) snd sps

  After l r -> do
    sps <- point (And l r)
    return $ (fmap.fmap) fst sps

  SepBy f sep -> do
    sp <- point (Opt (SepBy1 f sep))
    return $ (fmap.fmap) (maybe [] id) sp

  SepBy1 f sep -> do
    sp <- point (And f (Star (And sep f)))
    return $ (fmap.fmap) (\(x, xs) -> x : fmap snd xs) sp

(<#>) :: TPoint l t f -> TPoint l t g -> TPoint l t (f, g)
(<#>) = And

(#>) :: TPoint l t () -> TPoint l t g -> TPoint l t g
(#>) = Before

(<#) :: TPoint l t f -> TPoint l t () -> TPoint l t f
(<#) = After

sepBy :: TPoint l t f  -> TPoint l t () -> TPoint l t [f]
sepBy = SepBy

infixr 5 <#, <#>, #>

instance IsString l => IsString (TPoint l t ()) where
  fromString = Keyword . fromString

type Grammar = State Int

data JSON
  = String S
  | Array [JSON]
  | Object [(S, JSON)]

test :: (Int, [SomeRule S S])
test = grammar mdo
  s      <- rule id     $ Or str (Or array object)
  str    <- rule String $ T "str"
  array  <- rule Array  $ "[" #> (s    `sepBy` ",") <# "]"
  object <- rule Object $ "{" #> (pair `sepBy` ",") <# "}"
  pair   <- rule id     $ (T "str" <# ":") <#> s
  return s
