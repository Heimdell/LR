
module Colored where

import Data.Set qualified as Set
import Data.Monoid

data ColorFlags
  = Red
  | Green
  | Blue
  deriving stock (Eq, Ord, Enum)

colorNumber :: Set.Set ColorFlags -> Int
colorNumber set = (foldMap (Sum . (2^) . fromEnum) set).getSum

setColorMode :: Set.Set ColorFlags -> Bool -> String
setColorMode flags light
  =  "\x1b["
  <> show (colorNumber flags + 30)
  <> do if light then ";1" else ""
  <> "m"

color :: [ColorFlags] -> String -> String
color flags str
  =  setColorMode (Set.fromList flags) False
  <> str
  <> setColorMode (Set.fromList [Red, Green, Blue]) False

bright :: [ColorFlags] -> String -> String
bright flags str
  =  setColorMode (Set.fromList flags) True
  <> str
  <> setColorMode (Set.fromList [Red, Green, Blue]) False

green   = color [Green]
yellow  = color [Green, Red]
magenta = color [Red, Blue]
blue    = color [Blue]
red     = color [Red]

indent = init' . unlines . map ("  " ++) . lines

init' [] = []
init' xs = init xs