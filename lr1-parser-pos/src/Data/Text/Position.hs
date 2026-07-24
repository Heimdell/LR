{-# LANGUAGE DerivingVia #-}
module Data.Text.Position where
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Function (on)
import Control.Arrow ( Arrow((&&&)) )
import Pretty

data Pos = Pos
  { column, line :: Int
  , filename     :: FilePath
  , source       :: Text
  }
  deriving (Show) via PP Pos

instance Eq  Pos where (==)    = (==)    `on` ((.line) &&& (.column) &&& (.filename))
instance Ord Pos where compare = compare `on` ((.line) &&& (.column) &&& (.filename))

startPos :: FilePath -> Text -> Pos
startPos filename source = Pos {filename, line = 1, column = 1, source}

instance Pretty Pos where
  pPrint Pos {column, line, filename, source} =
    vcat
      [ "at" <+> text filename <.> ":" <.> text prefix <.> ":" <.> pPrint column
      , text (' ' <$ prefix) <+> "|"
      , text (       prefix) <+> "|" <+> pPrint (fromMaybe "" sourceLine)
      , text (' ' <$ prefix) <+> "|" <.> text (replicate column ' ') <.> "^"
      ]
    where
      prefix = show line
      sourceLine = listToMaybe $ drop (line - 1) (Text.lines source)
