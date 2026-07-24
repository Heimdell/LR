{-# LANGUAGE DerivingVia #-}
module Data.Map.Monoidal.Pretty where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Monoidal.Structure (type (==>), assocs)
import Pretty ( Pretty(pPrint), (<.>), ($$), (<+>), PP(PP) )

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pPrint = foldr (\(k, v) rest -> (pPrint k <.> ":" <+> pPrint v) $$ rest) mempty . Map.assocs

instance (Pretty k, Pretty v) => Pretty (k ==> v) where
  pPrint = foldr (\(k, v) rest -> (pPrint k <.> ":" <+> pPrint v) $$ rest) mempty . assocs

deriving via PP (k ==> v) instance {-# OVERLAPS #-} (Pretty k, Pretty v) => Show (k ==> v)
