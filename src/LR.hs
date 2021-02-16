-- | A facade for parser.
--
module LR
  ( Rule(..)
  , Table(..)
  , parse
  , Name(Start)
  ) where

import Point ()
import Rule  (Rule (..))
import Table (Table (..))
import Parse (parse)
import Name  (Name(Start))
