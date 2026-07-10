module Position.Pretty where

import Data.Foldable                  (toList)
-- import Data.Function                  ((&))
-- import Data.Set                       (Set)
-- import GHC.Records                    (HasField(..))
import Text.PrettyPrint.HughesPJClass (Pretty, pPrint, (<+>), fsep, braces)

-- import Data.Set qualified as Set

-- import Data.Map.Monoidal  ((==>))
import Position.Structure
import Rule
-- import Term               (Term)

-- import Data.Map.Monoidal qualified as Map

-- instance HasField "prefix" Position (Rule, Int) where
--   getField pos = (pos.rule, pos.offset)

-- groupPositionsByPrefices :: Set Position -> Set PrettyPosition
-- groupPositionsByPrefices set =
--   foldMap (\a -> a.prefix ==> Set.singleton a.lookahead) set
--     & Map.foldMapWithKey \(e, f) ts -> Set.singleton $ PrettyPosition e f ts

-- data PrettyPosition = PrettyPosition
--   { rule      :: Rule
--   , offset    :: Int
--   , lookahead :: Set Term
--   }
--   deriving stock (Eq, Ord)

-- instance Pretty PrettyPosition where
--   pPrint PrettyPosition {clause, entity, type_, offset, lookahead} =
--     pPrint rule.entity <+> "=" <+> fsep (map pPrint front) <+> (case rest of
--         [] -> "."
--         locus : other ->
--           ("." <> pPrint locus)
--             <+> fsep (map pPrint other)
--       ) <+> braces (fsep (map pPrint (toList lookahead)))
--     where
--       (front, rest) = splitAt offset (toList rule.points)

instance Pretty Position where
  pPrint Position {entity, clause, offset, lookahead} =
    pPrint entity <+> "=" <+> fsep (map pPrint front) <+> (case rest of
        [] -> "."
        locus : other ->
          ("." <> pPrint locus)
            <+> fsep (map pPrint other)
      ) <+> braces (pPrint lookahead)
    where
      (front, rest) = splitAt offset (toList clause.points)
