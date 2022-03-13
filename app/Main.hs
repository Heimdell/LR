import Data.Maybe (fromMaybe)

import LR1.Term qualified as Term
import LR1
import Data.String (IsString(fromString))

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
      s@('"'  : _) -> (Term.Term "string", (), s)
      s@('\'' : _) -> (Term.Term "string", (), s)
      s            -> (fromString s,       (), s)

main :: IO ()
main = do
  let
    list = fromMaybe []

    json = LR1.compile $ LR1.grammar mdo
      expr <- LR1.clause @JSON
        [ Reduce Array  &! array
        , Reduce Object &! object
        , Reduce String &# "string"
        , Reduce Null   &. "null"
        ]

      array <- LR1.clause @[JSON]
        [ Reduce list &. "[" &? exprs &. "]"
        ]

      object <- LR1.clause @[(String, JSON)]
        [ Reduce list &. "{" &? pairs &. "}"
        ]

      pair <- LR1.clause @(String, JSON)
        [ Reduce (,) &# "string" &. ":" &! expr
        ]

      exprs <- LR1.sepBy expr ","
      pairs <- LR1.sepBy pair ","

      return expr

  -- lex input
  putStrLn "Input something, like \"[ 'a' , null , { 'b' : [ ] , 'c' : null } , 'd' ]\""
  str <- getLine
  let input = lexer str
  print input

  -- run parser
  res <- LR1.parse json input
  print res

  return ()
