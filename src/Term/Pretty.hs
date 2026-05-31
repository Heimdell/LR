module Term.Pretty where

import Term.Structure                 (Point(..), Entity(entity), Term(term))
import Text.PrettyPrint.HughesPJClass (text, Pretty(pPrint))
import qualified Data.Text as Text

instance Pretty Term   where pPrint = text . Text.unpack . (.term)
instance Pretty Entity where pPrint = text . Text.unpack . (.entity)

instance Pretty Point where
  pPrint = \case
    T term   -> pPrint term
    E entity -> pPrint entity

instance Show Term where
  show = show . pPrint