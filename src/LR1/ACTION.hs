module LR1.ACTION where

import Control.Monad.State qualified as MTL
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import GHC.Generics (Generic)

import LR1.Fixpoint ((==>), Get ((?)))
import LR1.GOTO    qualified as GOTO
import LR1.Item    qualified as Item
import LR1.Map     qualified as Map
import LR1.NonTerm qualified as NonTerm
import LR1.Point   qualified as Point
import LR1.State   qualified as State
import LR1.Term    qualified as Term
import qualified LR1.Func as Func

data Action
  = Accept
  | Reduce
      { label  :: Func.T
      , entity :: NonTerm.T
      , len    :: Int
      }
  | Shift State.Index
  | Conflict Action Action
  | Empty
  deriving stock (Eq, Generic)

instance Semigroup Action where
  a <> b | a == b = a
  a <> b          = Conflict a b

instance Monoid Action where
  mempty = Empty

newtype T = ACTION
  { unwrap :: Map.T State.Index (Map.T Term.T Action)
  }
  deriving newtype (Semigroup, Monoid, Generic)

newtype Typed t a = Typed T

instance Get LR1.ACTION.T (State.Index, Term.T) Action where
  ACTION m ? (i, t) = m Map.! i Map.! t

make :: forall m. State.HasReg m => GOTO.T -> m LR1.ACTION.T
make goto = do
  states <- MTL.gets State.indices
  return $ foldMap getActions states
  where
    getActions :: State.T -> LR1.ACTION.T
    getActions state = foldMap (getRuleActions (State.index state)) $ State.items state

    getRuleActions :: State.Index -> Item.T -> LR1.ACTION.T
    getRuleActions index item =
      ACTION $ index ==>
        case Item.locus item of
          Nothing | Item.entity item == NonTerm.Start ->
            foldMap (==> Accept) (Item.lookahead item)

          Nothing ->
            foldMap reduceOn (Item.lookahead item)

          Just (Point.Term t) ->
            t ==> Shift (goto ? (index, Point.Term t))

          _ ->
            mempty
      where
        reduceOn :: Term.T -> Map.T Term.T Action
        reduceOn t = t ==> Reduce
          { label  = Item.label  item
          , entity = Item.entity item
          , len    = Item.len    item
          }

dump :: State.HasReg m => Text -> LR1.ACTION.T -> m String
dump header (ACTION goto) = do
  let
    asList = goto
      & Map.toList
      & (fmap.fmap) Map.toList
  stateList <- for asList \(srcIndex, dests) -> do
    srcState <- MTL.gets ((Map.! srcIndex) . State.indices)
    return (srcState, dests)

  let
    showBlock (src, dests)  = unlines (show src : fmap showDest dests)
    showDest  (point, dest) = show point <> "\t" <> show dest

  return $ stateList
    & fmap showBlock
    & (Text.unpack header :)
    & unlines

conflicts :: LR1.ACTION.T -> LR1.ACTION.T
conflicts (ACTION actions) =
  actions
    & Map.map (Map.filter isConflict)
    & Map.filter (any isConflict)
    & ACTION
  where
    isConflict :: Action -> Bool
    isConflict Conflict {} = True
    isConflict _           = False

expected :: LR1.ACTION.T -> State.Index -> Map.T Term.T Action
expected (ACTION actions) index = actions Map.! index

instance Show Action where
  show = \case
    Accept -> "Accept"
    Reduce {label, entity, len} -> "Reduce " <> show label <> "/" <> show len <> " -> " <> show entity
    Shift n -> "Shift " <> show n
    Conflict ac ac' -> "Conflict (" <> show ac <> ", " <> show ac' <> ")"
    Empty -> "Empty"
