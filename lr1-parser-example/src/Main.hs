
import AST
import Parser (parseProgram, parseTestSuite)
import Text.Lexer.Default
import System.Environment (getArgs)
import System.Exit

main = do
  getArgs >>= \case
    [file, test] -> do
      res <- parseProgram file >>= dieOnLexerError >>= dieOnParserError
      print res
      res <- parseTestSuite test >>= dieOnLexerError >>= dieOnParserError
      print res
    _ -> do
      putStrLn "Usage: gen-lr1-parser-example <file-to-parse>"
      exitFailure
