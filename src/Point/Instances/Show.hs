
module Point.Instances.Show where

import Data.Text qualified as Text

import Colored
import Point.Base

instance Show Term where
  show term = color [Green, Red] (Text.unpack term.raw)

instance Show Entity where
  show term = color [Green] (Text.unpack term.raw)

instance Show Point where
  show = \case
    Entity e -> show e
    Term   t -> show t
