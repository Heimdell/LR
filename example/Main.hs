
import AST
import Parser
import Backend.DefaultLexer

main = do
  res <- parse "example/test.dlog" >>= dieOnLexerError >>= dieOnParserError
  print res
