
-- | A grammar rule.
--
module Rule where

import Control.Arrow ((&&&))

import Data.Function (on)

import Name
import Point
import Pretty

-- | A rule.
data Rule term result = Rule
  { rName   :: Name                -- name of the entity
  , rPoints :: [Point term]        -- contents of the rule
  , rReduce :: [result] -> result  -- semantic action
  }
  deriving Show via PP (Rule term result)

instance Eq  term => Eq  (Rule term result) where
  (==) = (==) `on` (rName &&& rPoints)

instance Ord term => Ord (Rule term result) where
  compare = compare `on` (rName &&& rPoints)

instance Pretty term => Pretty (Rule term result) where
  pretty (Rule name points _) =
    pretty name <+> "=" `indent` fsep (map pretty points)
