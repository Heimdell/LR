
import AST
import Parser
import Text.Lexer.Default

main = do
  res <- parse "example/test.dlog" >>= dieOnLexerError >>= dieOnParserError
  print res
