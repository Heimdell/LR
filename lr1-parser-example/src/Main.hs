
import AST
import Parser
import Text.Lexer.Default
import System.Environment (getArgs)
import System.Exit

main = do
  getArgs >>= \case
    [file] -> do
      res <- parseProgram file >>= dieOnLexerError >>= dieOnParserError
      print res
    _ -> do
      putStrLn "Usage: gen-lr1-parser-example <file-to-parse>"
      exitFailure
