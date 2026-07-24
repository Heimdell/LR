module Rule where

import Data.Text     (Text)

import Symbol (Entity, NamedSymbol)
import Data.Foldable                  (toList)
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Data.List.NonEmpty (NonEmpty)
import Pretty

{- |
  Grammar rule.

  > entity = symbols... { reduce }

  Field `symbols` is non-empty. This greatly simplifies calculation of
  lookahead.
-}
data Rule = Rule
  { entity  :: Entity
  , symbols :: NonEmpty NamedSymbol
  , reduce  :: Text
  }
  deriving stock (Eq, Ord)
  deriving (Show) via PP Rule

instance Pretty Rule where
  pPrint rule = sep
    [ pPrint rule.entity
    , "="
    , sep $ map pPrint $ toList rule.symbols
    ]
