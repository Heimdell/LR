module Tables.Pretty where

import Data.Foldable                  (toList)
import Data.Function                  ((&))
import Text.PrettyPrint.HughesPJClass hiding ((<>))

import Tables.Structure (Action(..), Table(..), Conflict(..))
import Decision()

import Data.Map.Monoidal qualified as Map
import Term
import LR1Item
import Rule
import Data.Text (Text)

instance (Pretty state) => Pretty (Action state) where
  pPrint Action {goto, action} = vcat
    [ goto
        & Map.assocs
        & map (\(node, fan) -> hang (pPrint node) 2 (pPrint fan))
        & punctuate "\n" & vcat
    , "   "
    , action
        & Map.assocs
        & map (\(node, fan) -> hang (pPrint node) 2 (vcat (map pPrint (toList fan))))
        & punctuate "\n" & vcat
    ]

instance (Pretty state) => Pretty (Table state) where
  pPrint Table {actions} = pPrint actions

instance Pretty Conflict where
  pPrint Conflict {leading, positions, term} = vcat
    [ "There is a conflict for input sequence"
    , nest 2 do fsep (map pPrint leading) <+> pPrintShaded term <+> pPrintShaded ("..." :: Text)
    , "  "
    , hang ("Сonflicting positions of rules for lookahead" <+> pPrintShaded term <+> "in the same parsing state are") 2 do
      vcat do
        positions & foldMap \pos ->
          [positionLine leading pos]
    , "  "
    ]

positionLine :: [Point] -> LR1Item -> Doc
positionLine points pos@LR1Item {offset, clause, lookahead} = do
  let (before, after) = splitAt (length points - offset) points
  let additional = drop offset $ toList clause.points
  if null additional
  then do
    vcat [fsep
      [ fsep (map pPrint before)
      , zeroWidthText "\ESC[4m" <> fsep (map pPrint after) <> zeroWidthText "\ESC[0m"
      , zeroWidthText "\ESC[0m\ESC[2m" <> pPrint lookahead
      , "..." <> zeroWidthText "\ESC[0m"
      ], nest 2 (pPrintShaded pos)]
  else do
    vcat [fsep
      [ fsep (map pPrint before)
      , zeroWidthText "\ESC[4m" <> fsep (map pPrint after) <> zeroWidthText "\ESC[2;3;4m"
      , fsep (map pPrint additional) <> zeroWidthText "\ESC[0m"
      , zeroWidthText "\ESC[0m\ESC[2;3m" <> pPrint lookahead
      , "..." <> zeroWidthText "\ESC[0m"
      ], nest 2 (pPrintShaded pos)]

pPrintShaded :: Pretty a => a -> Doc
pPrintShaded point =
  zeroWidthText "\ESC[2m" <> pPrint point <> zeroWidthText "\ESC[0m"
