module LR1.ACTION where

import Data.Text (Text)
import qualified LR1.NonTerm as NonTerm
import LR1.Fixpoint (Map, (==>), Get ((?)))
import qualified LR1.Term as Term
import Control.Lens ( (&), use, uses )
import qualified LR1.State as State
import qualified LR1.Item as Item
import qualified LR1.Point as Point
import qualified LR1.GOTO as GOTO
import qualified Data.Map.Monoidal as Map
import Data.Traversable (for)
import qualified Data.Text as Text
import GHC.Generics (Generic)
-- import Data.List (nub, intercalate, sort)
-- import Data.Maybe (fromJust)

data Action
  = Accept
  | Reduce
      { label  :: Text
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
  { unwrap :: Map State.Index (Map Term.T Action)
  }
  deriving newtype (Semigroup, Monoid, Generic)

instance Get LR1.ACTION.T (State.Index, Term.T) Action where
  ACTION m ? (i, t) = m Map.! i Map.! t

make :: forall m. State.HasReg m => GOTO.T -> m LR1.ACTION.T
make goto = do
  states <- use State.indices
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
        reduceOn :: Term.T -> Map Term.T Action
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
    srcState <- uses State.indices (Map.! srcIndex)
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

expected :: LR1.ACTION.T -> State.Index -> Map Term.T Action
expected (ACTION actions) index = actions Map.! index

-- showTables :: GOTO.T -> LR1.ACTION.T -> [(Int, Int)] -> [Int] -> State.Reg -> String
-- showTables goto action rename reterm reg = do
--   let indices = reg^.State.indices.to Map.keys
--   unlines
--     ( intercalate "\t" ("states" : map show terms ++ map show nonTerms)
--     : map row indices
--     )
--   where
--     terms = action & unwrap & foldMap Map.keys & nub & perm
--     nonTerms = goto & GOTO.unwrap & foldMap Map.keys & nub & filter Point.isEntity

--     re = fromJust . flip lookup rename
--     unre = fromJust . flip lookup (map swap rename)

--     swap (a, b) = (b, a)

--     row :: State.Index -> [Char]
--     row ix =
--       intercalate "\t"
--         ( show ix
--         : map (t ix) terms ++ map (n ix) nonTerms
--         )

--     n :: Int -> Point.T -> String
--     n ix tr = case Map.lookup (unre ix) (GOTO.unwrap goto) >>= Map.lookup tr of
--       Just s -> show (re s)
--       _      -> "."

--     t :: Int -> Term.T -> String
--     t ix tr = case Map.lookup (unre ix) (unwrap action) >>= Map.lookup tr of
--       Just Accept -> "acc"
--       Just (Shift st) -> "s" <> show (re st)
--       Just Reduce {} -> "r"
--       _              -> "."

--     perm :: [Term.T] -> [Term.T]
--     perm items = [items !! (reterm !! i) | i <- [0 .. length items - 1]]


instance Show Action where
  show = \case
    Accept -> "Accept"
    Reduce {label, entity, len} -> "Reduce " <> Text.unpack label <> "/" <> show len <> " -> " <> show entity
    Shift n -> "Shift " <> show n
    Conflict ac ac' -> "Conflict (" <> show ac <> ", " <> show ac' <> ")"
    Empty -> "Empty"
