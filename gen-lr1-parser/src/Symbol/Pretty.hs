module Symbol.Pretty where

import Text.PrettyPrint.HughesPJClass (text, Pretty(pPrint))

import Data.Text qualified as Text

import Symbol.Structure
import Data.Text (Text)

instance Pretty Term   where pPrint = pPrint . (.term)
instance Pretty Entity where pPrint = pPrint . (.entity)

instance Pretty Symbol where
  pPrint = \case
    T mbName term   -> maybe (pPrint term)   (\name -> pPrint name <> ":" <> pPrint term  ) mbName
    E mbName entity -> maybe (pPrint entity) (\name -> pPrint name <> ":" <> pPrint entity) mbName

instance Show Term   where show = show . pPrint
instance Show Entity where show = show . pPrint

instance Pretty Text where
  pPrint = text . Text.unpack

instance Pretty Lookahead where
  pPrint = \case
    LookForTerm term -> pPrint term
    LookForEOF       -> "<eof>"
