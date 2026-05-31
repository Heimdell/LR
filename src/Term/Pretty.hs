module Term.Pretty where

import Text.PrettyPrint.HughesPJClass (text, Pretty(pPrint))

import Data.Text qualified as Text

import Term.Structure (Point(..), Entity(entity), Term(term))

instance Pretty Term   where pPrint = text . Text.unpack . (.term)
instance Pretty Entity where pPrint = text . Text.unpack . (.entity)

instance Pretty Point where
  pPrint = \case
    T term   -> pPrint term
    E entity -> pPrint entity

instance Show Term where
  show = show . pPrint
