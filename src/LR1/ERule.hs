{-# OPTIONS_GHC -Wno-orphans #-}

{- |
  Untyped implementation of some extensions to basic LR(1) grammar.

  By default, each rule must be non-empty and each point can be either
  a term or a non-term.

  This module adds ability to augment points with
  @?@ (0 or 1, `Maybe`), @+@ (1 or more, `[]`) and @*@ (0 or more, `[]`) and also
  ability to ignore a term in the reducer function for a rule.

  Optionality will split the rule in two - one with the thing and one without.

  Kleene Plus will add a @ts = t ts; ts = t@ rule and Kleene Star is optional
  Kleene Plus.

  Each rule still must be non-empty.
-}

module LR1.ERule where

import Control.Monad.Writer qualified as MTL
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import GHC.Types qualified

import LR1.Func    qualified as Func
import LR1.NonTerm qualified as NonTerm
import LR1.Point   qualified as Point
import LR1.Rule    qualified as Rule

-- | Straight copy of `Rule.T`.
data ERule = ERule
  { name   :: NonTerm.T
  , points :: [EPoint]
  , label  :: Func.T
  }

-- | Extended point.
data EPoint
  = Many   Point.T
  | Some   Point.T
  | Opt    Point.T
  | The    Point.T
  | The_   Point.T

-- | Turn extended rules into normal ones.
compile :: [ERule] -> [Rule.T]
compile = Set.toList . foldMap translate

-- | Turn extended rule into a set of normal ones.
translate :: ERule -> Set Rule.T
translate = go
  where
    go :: ERule -> Set Rule.T
    go (ERule t eps (Func.Func f _)) = do
      let (ptss, rules) = MTL.runWriter $ mapM explode eps
      Set.fromList rules <> implode t ptss f

    explode :: MTL.MonadWriter [Rule.T] m => EPoint -> m [(Func.Act, [Point.T])]
    explode = \case
      Many ep -> do
        pt <- ruleOfSome ep
        return [(Func.Id, [Point.NonTerm pt]), (Func.None, [])]
      Some ep -> do
        pt <- ruleOfSome ep
        return [(Func.Id, [Point.NonTerm pt])]
      Opt ep -> do
        return [(Func.FJust, [ep]), (Func.No, [])]
      The t -> do
        return [(Func.Id, [t])]
      The_ t -> do
        return [(Func.Drop, [t])]

    ruleOfSome :: MTL.MonadWriter [Rule.T] m => Point.T -> m NonTerm.T
    ruleOfSome = \case
      t -> do
        let entity = fromString (show t <> "-many")
        MTL.tell
          [ Rule.Rule entity [t] (Func.func (: []) [Func.Id])
          , Rule.Rule entity [t, Point.NonTerm entity] (Func.func (:) [Func.Id, Func.Id])
          ]
        return entity

    implode :: NonTerm.T -> [[(Func.Act, [Point.T])]] -> GHC.Types.Any -> Set Rule.T
    implode entity paths f = Set.fromList do
      (args, points) <- loop paths
      return $ Rule.Rule entity points (Func.func f args)
      where
        loop :: [[(Func.Act, [Point.T])]] -> [([Func.Act], [Point.T])]
        loop (point : rest) = do
          (act, pts) <- point
          (acts, points) <- loop rest
          return (act : acts, pts <> points)
        loop [] = [([], [])]
