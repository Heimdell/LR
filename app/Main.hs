
import Data.Text.IO qualified as Text
import System.IO
import System.Environment
import Shower

import E
import AST

main :: IO ()
main = do
  getArgs >>= \case
    [src] -> do
      hSetBuffering stdout NoBuffering
      txt <- Text.readFile src
      let e = test src txt
      putStrLn "\n==== Parsed ===="
      printer e

    _ -> do
      putStrLn "USAGE: bricc <filename>"
