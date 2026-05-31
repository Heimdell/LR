module Position.Pretty where

import Data.Foldable                  (toList)
import Data.Function                  ((&))
import Data.Map.Monoidal              ((==>))
import Data.Map.Monoidal qualified as Map
import Data.Set                       (Set)
import Data.Set qualified as Set
import Text.PrettyPrint.HughesPJClass (Pretty, pPrint, (<+>), fsep, braces)

import Position.Structure ( Position(offset, lookahead, rule) )
import Term               (Term)
import GHC.Records ( HasField(..) )
import Rule

instance HasField "prefix" Position (Rule, Int) where
  getField pos = (pos.rule, pos.offset)

groupPositionsByPrefices :: Set Position -> Set PrettyPosition
groupPositionsByPrefices set =
  foldMap (\a -> a.prefix ==> Set.singleton a.lookahead) set
    & Map.foldMapWithKey \(e, f) ts -> Set.singleton $ PrettyPosition e f ts

data PrettyPosition = PrettyPosition
  { rule      :: Rule
  , offset    :: Int
  , lookahead :: Set Term
  }
  deriving stock (Eq, Ord)

instance Pretty PrettyPosition where
  pPrint PrettyPosition {rule, offset, lookahead} =
    pPrint rule.entity <+> "=" <+> fsep (map pPrint (reverse front)) <+> (case rest of
        [] -> "."
        locus : other ->
          ("." <> pPrint locus)
            <+> fsep (map pPrint other)
      ) <+> braces (fsep (map pPrint (toList lookahead)))
    where
      (front, rest) = splitAt offset (toList rule.points)
