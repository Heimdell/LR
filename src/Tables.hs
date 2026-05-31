module Tables (module M) where

import Tables.Structure as M
    ( Decision(..), Table(..), Action(..), State(..), closure, makeTables )
import Tables.Pretty ()