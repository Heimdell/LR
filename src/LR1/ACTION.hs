{- |
  ACTION table.

  Answers the question: If we are in the state @N@ and next term is @T@, what
  should we do?
-}

module LR1.ACTION
  ( -- * Shift/reduce table.
    LR1.ACTION.T (..)
  , make
  , dump
  , conflicts
  , expected

    -- * Parser action
  , Action(..)
  )
  where

import Control.Monad.State qualified as MTL
import Data.Function ((&))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import GHC.Generics (Generic)

import LR1.Func    qualified as Func
import LR1.GOTO    qualified as GOTO
import LR1.Item    qualified as Item
import LR1.Map     qualified as Map
import LR1.NonTerm qualified as NonTerm
import LR1.Point   qualified as Point
import LR1.State   qualified as State
import LR1.Term    qualified as Term
import LR1.Utils ((==>), Get ((?)))

{- |
  The only things the parser can do.
-}
data Action
  = -- | Accept current input, return top of the value stack.
    Accept

    -- | Squash `len` items in both state & value stacks into `entity`, push it back.
  | Reduce
      { label  :: Func.T     -- ^ reducer function
      , entity :: NonTerm.T  -- ^ an entity to be reduced into
      , len    :: Int        -- ^ count of items to take
      }

    -- | Go to given state.
  | Shift State.Index

    -- | Two different actions are avaliable. LR(1)-backend can't work with that.
    --   GLR backend _should_ be able to work with that.
  | Conflict Action Action

    -- | So we can have a `Monoid` instance. Isn't used anywhere.
  | Empty
  deriving stock (Eq, Generic)

instance Semigroup Action where
  a <> b | a == b = a
  a <> b          = Conflict a b

instance Monoid Action where
  mempty = Empty

{- | A map from (`State.Index`, `Term.T`) -> `Action`.
-}
newtype T = ACTION
  { unwrap :: Map.T State.Index (Map.T Term.T Action)
  }
  deriving newtype (Semigroup, Monoid, Generic)

{- | Access to ACTION table.
-}
instance Get LR1.ACTION.T (State.Index, Term.T) Action where
  ACTION m ? (i, t) = m Map.! i Map.! t

{- | Generate ACTION table from GOTO table.
-}
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

{- | Print ACTION table for debug purposes.
-}
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

{- | Extract all conflicting actions.
-}
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

{- | Get a set of `Term.T` that are expected at this state.
-}
expected :: LR1.ACTION.T -> State.Index -> Set Term.T
expected (ACTION actions) index = Set.fromList $ Map.keys $ actions Map.! index

instance Show Action where
  show = \case
    Accept -> "Accept"
    Reduce {label, entity, len} -> "Reduce " <> show label <> "/" <> show len <> " -> " <> show entity
    Shift n -> "Shift " <> show n
    Conflict ac ac' -> "Conflict (" <> show ac <> ", " <> show ac' <> ")"
    Empty -> "Empty"
