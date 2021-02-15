
module Pretty (module Pretty, module Text.PrettyPrint) where

import Text.PrettyPrint hiding ((<>), empty)

class Pretty p where
  pretty :: p -> Doc

newtype PP a = PP { unPP :: a }

instance Pretty a => Show (PP a) where
  show = show . pretty . unPP

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (a, b) = "(" `indent` pretty a `above` "," `indent` pretty b `above` ")"

(<.>), indent, above :: Doc -> Doc -> Doc
(<.>) = (<>)
indent = flip hang 2
above  = flip hang 0

color :: Int -> Doc -> Doc
color c s = code (30 + c) <.> s <.> code 0
  where
    code :: Int -> Doc
    code k = zeroWidthText ("\ESC[" ++ show k ++ "m")

-- instance {-# overlaps #-} Pretty String where pretty = text
instance Pretty Int    where pretty = int
instance Pretty Bool where pretty b = if b then "True" else "False"
instance Pretty a => Pretty [a] where pretty ls = "[" `indent` vcat (punctuate "," (map pretty ls)) `above` "]"

newtype ShortList a = ShortList [a]

instance Pretty a => Pretty (ShortList a) where
  pretty (ShortList ls) = "[" <.> fsep (punctuate "," (map pretty ls)) <.> "]"
