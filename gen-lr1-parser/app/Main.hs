module Main where

import Data.List.Split (splitOn)
import Options.Applicative
import Backend.StateMachine

data Config = Config
  { pathToSrc   :: FilePath
  , moduleName  :: [String]
  , grammarFile :: FilePath
  }

config :: Parser Config
config = Config
  <$> strOption
    (  long    "src"
    <> metavar "PATH-TO-SRC"
    <> help    "path to root of haskell sources"
    <> value   "src"
    )
  <*> do
    splitOn "."
      <$> strOption
        (  long    "module"
        <> metavar "MODULE-NAME"
        <> help    "name of the module to generate"
        )
  <*> strOption
    (  long    "grammar"
    <> metavar "PATH-TO-GRAMMAR"
    <> help    "path to grammar"
    )

parser :: ParserInfo Config
parser = info (config <**> helper)
  (  fullDesc
  <> progDesc "Generate LR parser"
  <> header "LR parser generator"
  )

main :: IO ()
main = do
  Config {pathToSrc, moduleName, grammarFile} <- execParser parser
  createParserFile grammarFile pathToSrc moduleName
