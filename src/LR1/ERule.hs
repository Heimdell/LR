{-# OPTIONS_GHC -Wno-orphans #-}

module LR1.ERule where

-- import LR1.Rule qualified as Rule
import LR1.NonTerm qualified as NonTerm
import LR1.Rule qualified as Rule
import qualified Data.Set as Set
import Control.Monad.Writer
import qualified LR1.Point as Point
import LR1.Func
import LR1.Func qualified as Func
import Data.String (fromString)
import Data.Set (Set)
import GHC.Types qualified

data ERule = ERule { ename :: NonTerm.T, epoints :: [EPoint], elabel :: Func.T }

data EPoint
  = Many   Point.T
  | Some   Point.T
  | Opt    Point.T
  | The    Point.T
  | The_   Point.T

compile :: [ERule] -> [Rule.T]
compile = Set.toList . translate

translate :: [ERule] -> Set Rule.T
translate = foldMap go
  where
    go :: ERule -> Set Rule.T
    go (ERule t eps (Func f _)) = do
      let (ptss, rules) = runWriter $ mapM explode eps
      Set.fromList rules <> implode t ptss f

    explode :: MonadWriter [Rule.T] m => EPoint -> m [(Act, [Point.T])]
    explode = \case
      Many ep -> do
        pt <- ruleOfSome ep
        return [(Id, [Point.NonTerm pt]), (None, [])]
      Some ep -> do
        pt <- ruleOfSome ep
        return [(Id, [Point.NonTerm pt])]
      Opt ep -> do
        return [(FJust, [ep]), (No, [])]
      The t -> do
        return [(Id, [t])]
      The_ t -> do
        return [(Drop, [t])]

    ruleOfSome :: MonadWriter [Rule.T] m => Point.T -> m NonTerm.T
    ruleOfSome = \case
      t -> do
        let entity = fromString (show t <> "-many")
        tell
          [ Rule.Rule entity [t] (func (: []) [Id])
          , Rule.Rule entity [t, Point.NonTerm entity] (func (:) [Id, Id])
          ]
        return entity

implode :: NonTerm.T -> [[(Act, [Point.T])]] -> GHC.Types.Any -> Set Rule.T
implode entity paths f = Set.fromList do
  (args, points) <- loop paths
  return $ Rule.Rule entity points (func f args)

loop :: [[(Act, [Point.T])]] -> [([Act], [Point.T])]
loop (point : rest) = do
  (act, pts) <- point
  (acts, points) <- loop rest
  return (act : acts, pts <> points)
loop [] = [([], [])]
