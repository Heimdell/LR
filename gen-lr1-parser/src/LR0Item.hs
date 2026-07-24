module LR0Item where


import Rule



import Symbol
import Data.Text (Text)
import Pretty
import Data.List.NonEmpty (NonEmpty((:|)))

{- |
  LR0 Position "in the process".

  > entity = parsed... • locus ahead... { reduce }
  >                    ~ ~~~~~
  >                    ^ ^
  >                    | +-- item to be parsed next
  >                    +---- current parser position

  "LR0" means we carry no `Lookahead`.
-}
data Shifting = Shifting
  { entity ::  Entity
  , parsed :: [NamedSymbol]
  , locus  ::  NamedSymbol
  , ahead  :: [NamedSymbol]
  , reduce ::  Text
  }
  deriving stock (Eq, Ord)
  deriving (Show) via PP Shifting

instance Pretty Shifting where
  pPrint sh = sep
    ( pPrint sh.entity
    : "="
    : map pPrint sh.parsed
    ++ ["•", pPrint sh.locus]
    ++ map pPrint sh.ahead
    )

{- |
  LR0-position in general.

  It is either "in process" or some rule ready to reduce.
-}
data LR0Item
  = Shifts  Shifting
  | Reduces Rule
  deriving stock (Eq, Ord)
  deriving (Show) via PP LR0Item

instance Pretty LR0Item where
  pPrint = \case
    Shifts sh -> pPrint sh
    Reduces rule -> pPrint rule <+> "•"

{- |
  Start parsing some rule.

  For rule

  > E = E '+' F

  it is

  > [E ← • E '+' F]
-}
startLR0 :: Rule -> LR0Item
startLR0 Rule {entity, symbols = locus :| ahead, reduce} = Shifts Shifting
  { entity
  , parsed = []
  , locus
  , ahead
  , reduce
  }
