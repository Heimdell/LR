module Grammar.Check where

import Control.Monad.State
import Control.Monad.Writer
import Data.Set (Set)
import Data.Set qualified as Set

import RawGrammar as Raw
import Grammar as Scoped
import Term
import Rule
import Control.Monad
import Data.Foldable
import GHC.Generics
import Control.Monad.Error.Class (MonadError(throwError))
import Text.PrettyPrint.HughesPJClass

declaredEntities :: Raw.Grammar -> Set Entity
declaredEntities Raw.Grammar {ruleOrder} =
  foldMap (Set.singleton . (.entity)) ruleOrder

data Scope = Scope
  { declared :: Set Entity
  , defined  :: Set Entity
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via Generically Scope

data Error
  = Undefined Entity
  | Redefined Entity
  deriving stock (Eq, Ord)

instance Pretty Error where
  pPrint = \case
    Undefined entity -> "undefined" <+> pPrint entity
    Redefined entity -> "redefined" <+> pPrint entity

type M = WriterT (Set Error) (State Scope)

assertExists :: Entity -> M ()
assertExists entity = do
  yes <- gets (Set.member entity . (.declared))
  unless yes do
    tell $ Set.singleton (Undefined entity)

define :: Entity -> M ()
define entity = do
  yes <- gets (Set.member entity . (.defined))
  when yes do
    tell $ Set.singleton (Redefined entity)
  modify \scope -> scope {defined = Set.insert entity scope.defined}

checkGrammar :: Raw.Grammar -> M ()
checkGrammar Raw.Grammar {ruleOrder} = do
  for_ ruleOrder checkRule

checkRule :: Rule -> M ()
checkRule Rule {entity, clauses} = do
  define entity
  for_ clauses \clause -> do
    for_ clause.points checkPoint

checkPoint :: Point -> M ()
checkPoint = \case
  T {}       -> pure ()
  E _ entity -> assertExists entity

check :: Set Entity -> Raw.Grammar -> Either (Set Error) Scoped.Grammar
check starters grammar = do
  let (_, errors) = evalState (runWriterT checker) mempty { declared = declaredEntities grammar }
  if null errors
  then pure (Scoped.makeGrammar grammar.ruleOrder)
  else throwError errors
  where
    checker = do
      checkGrammar grammar
      for_ starters assertExists
