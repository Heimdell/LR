module LR1.Rule where

import LR1.NonTerm qualified as NonTerm
import LR1.Point   qualified as Point
import qualified LR1.Func as Func

data T = Rule
  { entity :: NonTerm.T
  , points :: [] Point.T
  , label  :: Func.T
  }
  deriving stock (Eq, Ord)

instance Show LR1.Rule.T where
  show Rule {entity, points, label} =
    show label
      <> "\t"  <> show entity
      <> " = " <> unwords (map show points)
