module LR1.Rule where

import Data.Text (Text)
import Data.Text qualified as Text

import LR1.NonTerm qualified as NonTerm
import LR1.Point   qualified as Point

data T = Rule
  { entity :: NonTerm.T
  , points :: [] Point.T
  , label  :: Text
  }
  deriving stock (Eq, Ord)

instance Show LR1.Rule.T where
  show Rule {entity, points, label} =
    Text.unpack label
      <> ".\t" <> show entity
      <> " = " <> unwords (map show points)
