
-- | An extension of `Text.PrettyPrint`.
--
module Pretty
  ( -- * Added stuff
    module Pretty

    -- * Original stuff
  , module Text.PrettyPrint
  ) where

import Text.PrettyPrint hiding ((<>), empty)

-- | Class of things to be pretty-printed.
class Pretty p where
  pretty :: p -> Doc

-- | A wrapper to generate `Show` instances from `Pretty` ones.
newtype PP a = PP { unPP :: a }

instance Pretty a => Show (PP a) where
  show = show . pretty . unPP

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (a, b) = "(" `indent` pretty a `above` "," `indent` pretty b `above` ")"

-- | Useful combinators.
(<.>), indent, above :: Doc -> Doc -> Doc
(<.>) = (<>)
indent = flip hang 2
above  = flip hang 0

-- | Change color of a document.
color :: Int -> Doc -> Doc
color c s = code (30 + c) <.> s <.> code 0
  where
    code :: Int -> Doc
    code k = zeroWidthText ("\ESC[" ++ show k ++ "m")

-- instance {-# overlaps #-} Pretty String where pretty = text
instance Pretty Int    where pretty = int
instance Pretty Bool where pretty b = if b then "True" else "False"
instance Pretty a => Pretty [a] where pretty ls = "[" `indent` vcat (punctuate "," (map pretty ls)) `above` "]"

-- | A wrapper that makes pretty-printed list to be one-liner.
newtype ShortList a = ShortList [a]

instance Pretty a => Pretty (ShortList a) where
  pretty (ShortList ls) = "[" <.> fsep (punctuate "," (map pretty ls)) <.> "]"
