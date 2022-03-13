import Data.Text qualified as Text
import Data.Maybe (fromMaybe)
import Text.Read ()

import LR1.Term qualified as Term
import LR1

data JSON
  = Array  [JSON]
  | Object [(String, JSON)]
  | String   String
  | Null
  deriving stock Show

lexer :: String -> [(Term.T, (), String)]
lexer = (<> [(Term.EndOfStream, (), "")]) . map lex' . words
  where
    lex' = \case
      s@('"'  : _) -> (Term.Term "string",      (), s)
      s@('\'' : _) -> (Term.Term "string",      (), s)
      s            -> (Term.Term (Text.pack s), (), s)

main :: IO ()
main = do
  let
    list = fromMaybe []

    json = LR1.compile $ grammar mdo
      start <- clauseS @JSON expr

      expr <- clause @JSON
        [ Reduce Array  &! array
        , Reduce Object &! object
        , Reduce String &# "string"
        , Reduce Null   &. "null"
        ]

      array <- clause @[JSON]
        [ Reduce list &. "[" &? exprs &. "]"
        ]

      object <- clause @[(String, JSON)]
        [ Reduce list &. "{" &? pairs &. "}"
        ]

      pair <- clause @(String, JSON)
        [ Reduce (,) &# "string" &. ":" &! expr
        ]

      exprs <- sepBy expr ","
      pairs <- sepBy pair ","

      return start

  -- lex input
  putStrLn "Input something, like \"[ 'a' , null , { 'b' : [ ] , 'c' : null } , 'd' ]\""
  str <- getLine
  let input = lexer str
  print input

  -- run parser
  res <- LR1.parse json input
  print res

  return ()
