{-# OPTIONS_GHC -Wno-star-is-type #-}

module Text.Brick.DSL
  ( module Text.Brick.DSL
  , Parser.Term (MkTerm)
  ) where

import Control.Monad.Writer
import Control.Monad.State
import Data.Text (Text)
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.Map (Map)
import Data.String (IsString(fromString))
import Data.Dynamic
import Data.Maybe

-- import Debug.Trace

import Text.Brick.Driver.LR1 qualified as Parser

data Rule t pos a where
  Rule :: Text -> [Clause t pos a] -> Rule t pos a

data Clause t pos a where
  (:=>) :: Chain t pos r a -> (pos -> r) -> Clause t pos a

infix 0 :=>

data Chain t pos r a where
  Pos :: Chain t pos r r
  (:!)  :: Typeable a => Chain t pos r (a -> b) -> Point t pos a -> Chain t pos r b

infixl 0 :!

data Point t pos a where
  Term   :: Text           -> Point t pos t
  Entity :: Entity t pos a -> Point t pos a

instance IsString (Point t pos t) where
  fromString = Term . fromString

newtype Entity t pos a = MkEntity Text

type M = WriterT ([Parser.Rule], Map Text Dynamic) (State Int)

rule :: forall (a :: *) (pos :: *) (t :: *). (Typeable pos, Typeable a, Typeable t) => Text -> [Clause t pos a] -> M (Entity t pos a)
rule a b = compile (Rule a b)

compile :: (Typeable pos, Typeable a, Typeable t) => Rule t pos a -> M (Entity t pos a)
compile (Rule ent chains) = do
  for_ chains \(chain :=> reducer) -> do
    compileRule ent chain reducer
  return $ MkEntity ent

compileRule :: (Typeable pos, Typeable a, Typeable t) => Text -> Chain t pos r a -> (pos -> r) -> M ()
compileRule ent chain reducer = do
  name <- gets (fromString . show)
  modify (+ 1)
  (pts, reducer') <- compileChain reducer chain
  tell
    ( [Parser.mkRule (Parser.MkEntity ent) (reverse pts) (Parser.MkMark name)]
    , Map.singleton name reducer'
    )

compileChain
  :: forall a r pos t
  .  (Typeable pos, Typeable a, Typeable t)
  => (pos -> r)
  -> Chain t pos r a
  -> WriterT ([Parser.Rule], Map Text Dynamic) (State Int) ([Parser.Point], Dynamic)
compileChain r = \case
  Pos -> return ([], toDyn r)
  rest :! pt -> do
    (pts, r') <- compileChain r rest
    return (compilePoint pt : pts, r')

compilePoint :: Point t pos a -> Parser.Point
compilePoint = \case
  Term    t           -> Parser.Term   $ Parser.MkTerm   t
  Entity (MkEntity e) -> Parser.Entity $ Parser.MkEntity e

grammar :: forall t pos a. M (Entity t pos a) -> (Parser.Grammar, Map Text Dynamic)
grammar act = do
  let (MkEntity e, (rules, reducer)) = evalState (runWriterT act) 1
  let start = Parser.mkRule "S" [Parser.Entity $ Parser.MkEntity e] "0"
  (Parser.makeGrammar (start : rules), reducer)

postProcess :: (Typeable pos, Typeable t) => Map Text Dynamic -> Parser.ParseTree pos t -> Dynamic
postProcess _     (Parser.Atom _ a) = toDyn a
postProcess funcs (Parser.Branch ctor pos xs) = do
  let fun = funcs Map.! ctor.raw
  let funPos = fun `dynApp` toDyn pos
  foldl dynApp funPos (map (postProcess funcs) xs)

data S = S Int S S | T Int
  deriving stock (Show)

(!.) :: (Typeable t) => Chain t pos r (t -> b) -> Text -> Chain t pos r b
c !. t = c :! Term t

(!*) :: (Typeable t, Typeable a) => Chain t pos r (a -> b) -> Entity t pos a -> Chain t pos r b
c !* e = c :! Entity e

infixl 1 !., !*

dump :: Dynamic -> String
dump (Dynamic rep _) = show rep

runParser :: (Parser.IsPosition pos, Typeable a, Typeable pos, Typeable t) => M (Entity t pos a) -> Text -> [(Parser.Term, pos, t)] -> a
runParser dsl = do
  let (g, post) = grammar dsl
  \src input ->
    case Parser.runLR1 g input of
      Left err -> error $ "\n" <> Parser.report src err
      Right a  -> fromJust $ fromDynamic $ postProcess post a