module Term (module M) where

import Term.Structure as M
  ( Point(..)
  , Entity(..)
  , Term(..)
  , pointTerminals
  , pointEntities
  )
import Term.Pretty ()