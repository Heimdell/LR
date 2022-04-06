{- |
  A parsing rule.
-}
module LR1.Rule where

import LR1.Point   qualified as Point
import LR1.Func    qualified as Func
import Data.Text (Text)

{- |
  A parsing rule.
-}
data T = Rule
  { entity :: Text   -- ^ Entity it reduces into.
  , points :: [] Point.T  -- ^ Rule points.
  , label  :: Func.T      -- ^ Reducing functions.
  }
  deriving stock (Eq, Ord)

instance Show LR1.Rule.T where
  show Rule {entity, points, label} =
    show label
      <> "\t"  <> show entity
      <> " = " <> unwords (map show points)
      <> " -> " <> show label
