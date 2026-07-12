
import Parser (parseProgram, parseTestSuite)
import Text.Lexer.Default
import System.Environment (getArgs)
import System.Exit

main :: IO ()
main = do
  getArgs >>= \case
    [file, test] -> do
      res <- parseProgram file >>= dieOnLexerError >>= dieOnParserError
      print res
      suite <- parseTestSuite test >>= dieOnLexerError >>= dieOnParserError
      print suite
    _ -> do
      putStrLn "Usage: gen-lr1-parser-example <file-to-parse>"
      exitFailure
