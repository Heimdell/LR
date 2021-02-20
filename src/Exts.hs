
module Exts where

import Data.Char   (isUpper)
import Data.String (IsString (..))

import Pretty
import Name
import Table

import S
import Tree

data ETable term result = ETable
  { etRules :: [ERule term result]
  }
  deriving Show via PP (ETable term result)

instance Pretty term => Pretty (ETable term result) where
  pretty = vcat . map pretty . etRules

data ERule term result = ERule
  { erName   ::  Name
  , erPoints :: [EPoint term]
  , erAction :: [result] -> result
  }
  deriving Show via PP (ERule term result)

instance Pretty term => Pretty (ERule term result) where
  pretty (ERule name points _) =
    pretty name <+> "=" `indent` fsep (map pretty points)

data EPoint term
  = ETerm term
  | EName Name
  | EPlus (EPoint term)
  | EMult (EPoint term)
  | EOpt  (EPoint term)
  | EOr   [EPoint term]
  | ESeq  [EPoint term]
  deriving Show via PP (EPoint term)

instance Pretty term => Pretty (EPoint term) where
  pretty = \case
    ETerm t -> pretty t
    EName n -> pretty n
    EPlus p -> parens (pretty p) <.> "+"
    EMult p -> parens (pretty p) <.> "*"
    EOpt  p -> parens (pretty p) <.> "?"
    EOr  ps -> parens (fsep $ punctuate " |" $ map pretty ps)
    ESeq ps -> fsep $ map pretty ps

instance IsString term => IsString (EPoint term) where
  fromString str@(fstChar : _)
    | isUpper fstChar = EName (Name       str)
    | otherwise       = ETerm (fromString str)

  fromString [] = error "Point.fromString: empty string is not allowed"

-- etableToTable :: ETable term result -> Table term result
-- etableToTable (ETable rules) = Table $ compile 0 rules
--   where
--     compile n [] = []
--     compile n (ERule name points action) =
--       [Rule name points' action | points']

test :: ETable S (Tree S)
test = ETable
  [ ERule  Start   ["Value"]                            head
  , ERule "Value"  [EOr ["Str", "Array", "Object"]]     head
  , ERule "Str"    ["str"]                              head
  , ERule "Array"  ["[", EOpt (ESeq ["Value", EMult (ESeq [",", "Value"])]), "]"] (!! 1)
  , ERule "Object" ["{", EOpt (ESeq ["Pair", EMult (ESeq [",", "Pair"])]), "}"] (!! 1)
  , ERule "Pair"   ["Str", ":", "Value"] \[a, _, b] -> Join ":" [a, b]
  ]