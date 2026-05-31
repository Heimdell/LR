module Position.Pretty where

import Data.Foldable                  (toList)
import Data.Function                  ((&))
import Data.Map.Monoidal              ((==>))
import Data.Map.Monoidal qualified as Map
import Data.Set                       (Set)
import Data.Set qualified as Set
import Text.PrettyPrint.HughesPJClass (Pretty, pPrint, (<+>), fsep, braces)

import Position.Structure (Position(..))
import Term               (Point, Entity, Term)
import GHC.Records ( HasField(..) )

instance HasField "prefix" Position (Entity, [Point], [Point]) where
  getField pos = (pos.entity, pos.front, pos.rest)

groupPositionsByPrefices :: Set Position -> Set PrettyPosition
groupPositionsByPrefices set =
  foldMap (\a -> a.prefix ==> Set.singleton a.lookahead) set
    & Map.foldMapWithKey \(e, f, r) ts -> Set.singleton $ PrettyPosition e f r ts

data PrettyPosition = PrettyPosition
  { entity    :: Entity
  , front     :: [Point]
  , rest      :: [Point]
  , lookahead :: Set Term
  }
  deriving stock (Eq, Ord)

instance Pretty PrettyPosition where
  pPrint PrettyPosition {entity, front, rest, lookahead} =
    pPrint entity <+> "=" <+> fsep (map pPrint (reverse front)) <+> (case rest of
        [] -> "."
        locus : other ->
          ("." <> pPrint locus)
            <+> fsep (map pPrint other)
      ) <+> braces (fsep (map pPrint (toList lookahead)))
