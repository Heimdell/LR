module Grammar.Check where

import Control.Monad
import Control.Monad.Error.Class (MonadError(throwError))
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text.Position
import Data.Traversable
import GHC.Generics

import Grammar as Scoped
import RawGrammar as Raw
import Rule
import Symbol
import Pretty
import qualified Data.List.NonEmpty as NonEmpty

declaredEntities :: RawGrammar -> Set NonTerminal
declaredEntities RawGrammar {rules} =
  foldMap (Set.singleton . (.entity)) rules

data Scope = Scope
  { declared :: Set NonTerminal
  , defined  :: Set NonTerminal
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via Generically Scope

data Error
  = Undefined Pos NonTerminal
  | Redefined Pos NonTerminal
  deriving stock (Eq, Ord)

instance Pretty Error where
  pPrint = \case
    Undefined pos entity -> "undefined" <+> pPrint entity $$ "at" <+> pPrint pos
    Redefined pos entity -> "redefined" <+> pPrint entity $$ "at" <+> pPrint pos

type M = WriterT (Set Error) (State Scope)

assertExists :: Pos -> NonTerminal -> M ()
assertExists pos entity = do
  yes <- gets (Set.member entity . (.declared))
  unless yes do
    tell $ Set.singleton (Undefined pos entity)

define :: Pos -> NonTerminal -> M ()
define pos entity = do
  yes <- gets (Set.member entity . (.defined))
  when yes do
    tell $ Set.singleton (Redefined pos entity)
  modify \scope -> scope {defined = Set.insert entity scope.defined}

checkGrammar :: RawGrammar -> M Grammar
checkGrammar RawGrammar {targets, imports, rules} = do
  rules' <- for rules checkStmt
  for_ targets (uncurry assertExists)
  pure Grammar
    { targets = foldMap (Set.singleton . snd) targets
    , imports = toList imports
    , rules = foldMap Set.fromList rules'
    , types = rules & foldMap \case
        StmtRule rule -> Map.singleton rule.entity rule.type_
        StmtSep  se   -> Map.singleton se.entity   se.type_
    }

checkStmt :: RawStmt -> M [Rule]
checkStmt = \case
  StmtRule rule -> checkRule rule
  StmtSep  se   -> checkSep  se

checkSep :: RawSep -> M [Rule]
checkSep RawSep {single = (ref, single), entity, pos, sep = se} = do
  define pos entity
  checkPoint (ref, Nothing :@ single)
  pure
    [ Rule
        { entity  = Named entity
        , symbols = [Just "x" :@ single]
        , reduce = "[x]"
        }
    , Rule
        { entity  = Named entity
        , symbols = [Just "x" :@ single]
            `NonEmpty.append` case se of
                Nothing ->          [Just "xs" :@ NonTerm entity]
                Just te -> [term te, Just "xs" :@ NonTerm entity]
        , reduce = "NonEmpty.cons x xs"
        }
    ]

term :: Terminal -> NamedSymbol
term t = Nothing :@ Term t


checkRule :: RawRule -> M [Rule]
checkRule RawRule {pos, entity, clauses} = do
  define pos entity
  for (toList clauses) \clause -> do
    for_ clause.symbols checkPoint
    pure Rule
      { symbols = snd <$> clause.symbols
      , reduce  = clause.reduce
      , entity  = Named entity
      }

checkPoint :: (Pos, NamedSymbol) -> M ()
checkPoint (pos, ns) = case ns.symbol of
  Term {}        -> pure ()
  NonTerm entity -> assertExists pos entity

check :: RawGrammar -> Either (Set Error) Grammar
check grammar = do
  let
    (chechedGrammar, errors) =
      evalState (runWriterT (checkGrammar grammar)) mempty
        { declared = declaredEntities grammar }
  if null errors
  then pure chechedGrammar
  else throwError errors
