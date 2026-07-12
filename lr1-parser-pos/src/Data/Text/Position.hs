module Data.Text.Position where
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Function (on)
import Control.Arrow

data Pos = Pos
  { column, line :: Int
  , filename     :: FilePath
  -- , sourceLine   :: Text
  }

instance Eq  Pos where (==)    = (==)    `on` ((.line) &&& (.column) &&& (.filename))
instance Ord Pos where compare = compare `on` ((.line) &&& (.column) &&& (.filename))

startPos :: FilePath -> Pos
startPos filename = Pos {filename, line = 1, column = 1}

showPos :: Pos -> Text -> String
showPos Pos {column, line, filename} sourceLines =
  unlines
    [ "at " <> filename <> ":" <> prefix <> ":" <> show column
    , (' ' <$ prefix) <> " | "
    , (       prefix) <> " | " <> Text.unpack (fromMaybe "<nowhere>" sourceLine)
    , (' ' <$ prefix) <> " |" <> replicate column ' ' <> "^"
    ]
  where
    prefix = show line
    sourceLine = listToMaybe $ drop (line - 1) (Text.lines sourceLines)
