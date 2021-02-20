
-- | A grammar rule.
--
module Rule where

import Control.Arrow ((&&&))

import Data.Function (on)

import Name
import Point
import Pretty

-- | A rule.
data Rule term = Rule
  { rName   :: Name                -- name of the entity
  , rPoints :: [Point term]        -- contents of the rule
  , rReduce :: Name                -- semantic "action"
  }
  deriving stock (Eq, Ord)
  deriving Show via PP (Rule term)

instance Pretty term => Pretty (Rule term) where
  pretty (Rule name points _) =
    pretty name <+> "=" `indent` fsep (map pretty points)
