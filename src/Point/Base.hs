
module Point.Base where

import Data.Text qualified as Text
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

newtype Term = MkTerm { raw :: Text.Text }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Hashable)

newtype Entity = MkEntity { raw :: Text.Text }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Hashable)

data Point
  = Entity Entity
  | Term   Term
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (Hashable)
