module Pretty (module M, module Pretty) where

import Text.PrettyPrint.HughesPJClass as M hiding ((<>))
import Data.Text (Text)
import qualified Data.Text as Text

infixl 6 <.>
(<.>) :: Doc -> Doc -> Doc
(<.>) = (<>)

newtype PP a = PP {unPP :: a}

instance Pretty Text where
  pPrint = text . Text.unpack

instance Pretty a => Show (PP a) where
  show = show . pPrint . (.unPP)
