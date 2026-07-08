module Interface.Structure where

import Control.Monad.State
import Control.Monad.Logic


import Term
import Data.Text (Text)
import Data.Map.Monoidal qualified as Map
import Data.Map.Monoidal (type (==>), (==>))
import Data.Set (Set)
import Data.Foldable (asum, Foldable (fold))
import Control.Monad (unless)
import Data.Functor (void)
import qualified Data.Set as Set
import Data.Traversable (for)
import Data.Maybe (mapMaybe, fromMaybe)
import Text.PrettyPrint.HughesPJClass hiding (sep, (<>))
import qualified Data.Text as Text

-- data Point'
--   = Point Point
--   | Gen   Expr
--   deriving stock (Eq, Ord)

-- instance Pretty Point' where
--   pPrint = \case
--     Point pt -> pPrint pt
--     Gen   ex -> pPrint ex

-- data Expr
--   = ExprPoint   Point
--   | ExprMany    Point
--   | ExprSome    Point
--   | ExprOpt     Point
--   | ExprSepSome Point Point
--   | ExprSepMany Point Point
--   deriving stock (Eq, Ord)

-- instance Pretty Expr where
--   pPrint = \case
--     ExprPoint   a   -> pPrint a
--     ExprMany    a   -> brackets (pPrint a)
--     ExprSome    a   -> braces   (pPrint a)
--     ExprOpt     a   -> pPrint a <> "?"
--     ExprSepSome a b -> parens (pPrint a <+> "/+" <+> pPrint b)
--     ExprSepMany a b -> parens (pPrint a <+> "/"  <+> pPrint b)

-- data Rule = Rule
--   { entity :: Entity
--   , exprs  :: [Expr]
--   , ctor   :: Text
--   , args   :: [Int]
--   }

-- instance Pretty Rule where
--   pPrint ru
--     =   pPrint ru.entity <+> "=" <+> fsep (map pPrint ru.exprs)
--     <+> braces (text (Text.unpack ru.ctor) <+> fsep (map ppArg ru.args))

-- ppArg :: Int -> Doc
-- ppArg i = "$" <> int i

-- data Compiled = Compiled
--   { points :: [Point']
--   , ctor   :: Text
--   , args   :: [(Maybe Text, Maybe Int)]
--   }
--   deriving stock (Eq, Ord)

-- instance Pretty Compiled where
--   pPrint ru
--     =   fsep (map pPrint ru.points)
--     <+> braces (text (Text.unpack ru.ctor) <+> fsep (map ppArg' ru.args))

-- ppArg' :: (Maybe Text, Maybe Int) -> Doc
-- ppArg' (ctor, arg) = parens ((foldMap (text . Text.unpack) ctor) <+> (foldMap ppArg arg))

-- type S = State (Expr ==> Set Compiled)
-- type M = LogicT S

-- run :: S (Expr ==> Set Compiled) -> Expr ==> Set Compiled
-- run s = do
--   let (res, st) = runState s mempty
--   res <> st

-- test :: [Rule]
-- test =
--   [ Rule "S" [ExprPoint   (E "E")]         ""  [1]
--   , Rule "E" [ExprOpt (E "X"), ExprSepSome (E "F") (T "+")] "Add" [1, 2]
--   , Rule "E" [ExprPoint   (E "F")]         ""  [1]
--   , Rule "F" [ExprSepSome (E "T") (T "*")] "Mul" [1]
--   , Rule "F" [ExprPoint   (E "T")]         ""  [1]
--   , Rule "T" [ExprPoint (T "("), ExprPoint (E "E"), ExprPoint (T ")")] "" [2]
--   , Rule "T" [ExprPoint   (T "num")]       "Number" [1]
--   ]

-- grammar :: [Rule] -> S (Expr ==> Set Compiled)
-- grammar = fmap fold . traverse rule

-- rule :: Rule -> S (Expr ==> Set Compiled)
-- rule ru = do
--   points <- observeAllT $ for ru.exprs expr
--   pure
--     (   ExprPoint (E ru.entity)
--     ==> foldMap (Set.singleton . compiled ru.args) points
--     )
--   where
--     compiled :: [Int] -> [(Maybe Point', Maybe Text)] -> Compiled
--     compiled args points = Compiled
--       { points = mapMaybe fst points
--       , ctor   = ru.ctor
--       , args   = collateArgs args points
--       }

--     renaming :: Int -> [(Maybe Point', Maybe Text)] -> [(Int, (Maybe Text, Maybe Int))]
--     renaming counter = \case
--       [] -> []
--       (Nothing, ctor) : rest -> (counter, (ctor, Nothing))      : renaming (counter + 1) rest
--       (Just _,  ctor) : rest -> (counter, (ctor, Just counter)) : renaming (counter + 1) rest

--     collateArgs :: [Int] -> [(Maybe Point', Maybe Text)] -> [(Maybe Text, Maybe Int)]
--     collateArgs args points = map (fromMaybe (Just "???", Just 0) . (`lookup` renaming 1 points)) args

-- expr :: Expr -> M (Maybe Point', Maybe Text)
-- expr ex = case ex of
--   ExprPoint pt -> pure (Just (Point pt), Nothing)
--   ExprOpt   pt -> asum
--     [ pure (Just (Point pt), Just "Just")
--     , pure (Nothing,         Just "Nothing")
--     ]

--   ExprMany pt -> do
--     emit (ExprSome pt)
--       [ Compiled [Point pt]                    "one"  [(Nothing, Just 1)]
--       , Compiled [Point pt, Gen (ExprSome pt)] "cons" [(Nothing, Just 1), (Nothing, Just 2)]
--       ]

--     asum
--       [ pure (Nothing,                  Just "[]")
--       , pure (Just (Gen (ExprSome pt)), Nothing)
--       ]

--   ExprSepMany pt sep -> do
--     emit (ExprSepSome pt sep)
--       [ Compiled [Point pt]                                      "one"     [(Nothing, Just 1)]
--       , Compiled [Point pt, Point sep, Gen (ExprSepSome pt sep)] "consSep" [(Nothing, Just 1), (Nothing, Just 3)]
--       ]

--     asum
--       [ pure (Nothing,                         Just "[]")
--       , pure (Just (Gen (ExprSepSome pt sep)), Nothing)
--       ]

--   ExprSome pt -> do
--     emit (ExprSome pt)
--       [ Compiled [Point pt]                    "one"  [(Nothing, Just 1)]
--       , Compiled [Point pt, Gen (ExprSome pt)] "cons" [(Nothing, Just 1), (Nothing, Just 2)]
--       ]

--     pure (Just (Gen (ExprSome pt)), Nothing)

--   ExprSepSome pt sep -> do
--     emit (ExprSepSome pt sep)
--       [ Compiled [Point pt]                                      "one"     [(Nothing, Just 1)]
--       , Compiled [Point pt, Point sep, Gen (ExprSepSome pt sep)] "consSep" [(Nothing, Just 1), (Nothing, Just 3)]
--       ]

--     pure (Just (Gen (ExprSepSome pt sep)), Nothing)

-- unlessCached :: Expr -> M a -> M ()
-- unlessCached ex k = do
--   done <- gets (Map.member ex)
--   unless done do
--     void k

-- emit :: Expr -> [Compiled] -> M ()
-- emit ex compiled = do
--   unlessCached ex do
--     modify \cache -> cache <> (ex ==> Set.fromList compiled)
