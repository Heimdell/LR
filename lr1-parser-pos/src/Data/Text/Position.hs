module Data.Text.Position where
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (fromMaybe, listToMaybe)

data Pos = Pos
  { column, line :: Int
  , filename     :: FilePath
  -- , sourceLine   :: Text
  }

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
