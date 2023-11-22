
module Driver where

import Prelude hiding (or)

import Control.Monad (unless, foldM)
import Control.Applicative ((<|>))
import Data.Map.Monoidal qualified as Map
import Data.Set qualified as Set
import Data.Function
import Data.Foldable (for_)
import Data.String

import Point
import Goto
import Grammar
import State
import Colored
import Rule

compileGrammar :: Grammar -> (Action, Goto, Vertex)
compileGrammar grammar = do
  evalCacheM grammar do
    g <- goto
    i <- initial
    return (action g, g, i)


{- | A structure to pack result of the parse in.
-}
data ParseTree pos a
  = Atom pos a
  | Branch Int pos [ParseTree pos a]

{- | Does not show `Mark`-s; instead, dumps all tokens an arranges parentheses
     around them.

     Parentheses are omitted for one-item nodes for clarity.
-}
instance Show a => Show (ParseTree pos a) where
  show = \case
    Atom _ a -> show a
    Branch _ _ [x] -> show x
    Branch _ _ xs -> magenta "(" <> unwords (map show xs) <> magenta ")"

data ParseError pos = ParseError
  { unexpected :: Term
  , position   :: pos
  , expected   :: Set.Set Term
  , reductions :: [Vertex]
  }

{- | Run a parser using ACTION and GOTO table from a starting state.

     Is a left fold with `consume`.

     The error
-}
parse
  :: (Action, Goto, Vertex)
  -> [(Term, pos, lexeme)]
  -> Either (ParseError pos) (ParseTree pos lexeme)
parse (actions, goto, initial) input = do
  let allConflicts = conflicts actions
  unless (null allConflicts.table) do
    error (show allConflicts)
  (_, [(_, result)]) <- foldM (consume actions goto) ([initial], []) input
  return result

consume
  :: Action
  -> Goto
  -> ([Vertex], [(pos, ParseTree pos lexeme)])
  -> (Term, pos, lexeme)
  -> Either (ParseError pos) ([Vertex], [(pos, ParseTree pos lexeme)])
consume actions goto (top : states, stack) (term, pos, lexeme) = do
  let positionsibleActions = actions.table Map.! top

  unless (term `Map.member` positionsibleActions) do
    Left (ParseError term pos (Map.keys positionsibleActions) (top : states))

  case positionsibleActions Map.! term of
    Shift st      -> return (st : top : states, (pos, Atom pos lexeme) : stack)
    Accept        -> return (top : states, stack)
    Conflict set  -> error $ "LR(1) driver cannot resolve conflicts\n" <> indent (show set)
    Reduce   rule -> do
      let size             = length rule.points
      case drop size (top : states) of
        top' : states' -> do
          let (rtaken, stack') = splitAt size stack
          let taken            = reverse rtaken
          let top''            = goto.table Map.! top' Map.! Entity rule.entity
          let states''         = top'' : top' : states'
          let subtrees         = map snd taken
          let start            = fst (head taken)
          let subtree          = Branch rule.mark start subtrees
          let stack''          = (start, subtree) : stack'
          consume actions goto (states'', stack'') (term, pos, lexeme)

        _ -> error "parser state is corrupted"

consume _ _ _ _ = error "parser state is corrupted"

instance MonadFail (Either e) where
  fail = error

-- {- | Simple endpoint. Generate tables and run parser.

--      Will generate tables as soon as first argument is received.
-- -}
-- runLR1
--   :: (Action, Goto, Vertex)
--   -> [(Term, pos, lexeme)]
--   -> Either (ParseError pos) (ParseTree pos lexeme)
-- runLR1 (action, goto, start) = do
--   -- traceShow grammar $
--   --  traceShow (length goto.moves) $
--     parse action goto start

-- gotoStateCount :: Goto -> Int
-- gotoStateCount goto =
--   length $ Map.keysSet goto.moves <> (foldMap . foldMap) Set.singleton goto.moves

testLR :: Grammar -> String -> String -> IO ()
testLR g f s = do
  case parse (compileGrammar g) (lexer f s) of
    Left err -> putStrLn $ report s err
    Right tree -> do
      print tree

{- | Make parse error pretty.
-}
report
  :: IsPosition pos
  => String            -- ^ Parsed source.
  -> ParseError pos
  -> String
report src (ParseError term sp terms entities) = do
  let prefixLength = length (posName sp) + 1 + posColumn sp - 1
  let line = lines src !! (posLine sp - 1)
  let lineNo = show (posLine sp)
  "\n" <> posName sp <> ":" <> lineNo <> ":" <> line <> "\n"
    <> replicate prefixLength ' ' <> map (const ' ') lineNo <> " ^\n"
    <> "expected " <> makeList (map show (Set.toList terms))
    <> " but got " <> show term
    <> "\nwhile parsing\n"
    <> indent (unlines $ map show $ take 1 entities)

{- | Class to abstract out a position.
-}
class IsPosition p where
  posName   :: p -> String
  posLine   :: p -> Int
  posColumn :: p -> Int

makeList :: [String] -> String
makeList [] = "nothing (the grammar is incorrect, probably)"
makeList [x] = x
makeList [x, y] = x <> " or " <> y
makeList (x : xs) = x <> ", " <> makeList xs

-- conflicts :: Actions -> Actions
-- conflicts action = action
--   { act = action.act & Map.filter (any isConflict) & Map.map (Map.filter isConflict)
--   }

-- isConflict :: Action -> Bool
-- isConflict Conflict {} = True
-- isConflict _           = False

data Pos = Pos { name :: String, line :: Int, col :: Int }
  deriving stock Show

instance IsPosition Pos where
  posName   = (.name)
  posLine   = (.line)
  posColumn = (.col)

start :: String -> Pos
start name = Pos { name, line = 1, col = 1 }

stepChar :: Char -> Pos -> Pos
stepChar '\n' pos = pos { line = 1 + pos.line, col = 1 }
stepChar  _   pos = pos { col  = 1 + pos.col }

newtype Lexeme = Lexeme { lexeme :: String }
  deriving newtype IsString

instance Show Lexeme where
  show = green . (.lexeme)

type Lexer = [(Char, Pos)] -> Maybe ((Term, Pos, Lexeme), [(Char, Pos)])

lexer :: String -> String -> [(Term, Pos, Lexeme)]
lexer fname str = loop (zip str (init poses)) <> [("$", last poses, "")]
  where
    poses = scanl (flip stepChar) (start fname) str

thread :: String -> [(Char, Pos)]
thread str = zip str $ scanl (flip stepChar) (start "-") str

loop :: [(Char, Pos)] -> [(Term, Pos, Lexeme)]
loop =
  many do
    token do
      someOf "int"  (set ['0'.. '9'])
        `or` one    "l"    'l'
        `or` one    ";"    ';'
        `or` one    "="    '='
        `or` someOf "name" (set ['a'.. 'z'])
        `or` one    "("    '('
        `or` one    ")"    ')'
        `or` one    "{"    '{'
        `or` one    "}"    '}'
        `or` one    "+"    '+'
        `or` one    "-"    '-'
        `or` one    "*"    '*'
        `or` one    "/"    '/'
        `or` one    "\\"    '\\'
        `or` one    "."    '.'

many :: Lexer -> [(Char, Pos)] -> [(Term, Pos, Lexeme)]
many l input = case l input of
  Nothing -> []
  Just (t, rest) -> t : many l rest

token :: Lexer -> Lexer
token l = l . dropWhile ((' ' ==) . fst)

or :: Lexer -> Lexer -> Lexer
or l r input = l input <|> r input

set :: String -> Set.Set Char
set = Set.fromList

someOf :: Term -> Set.Set Char -> Lexer
someOf term charset input =
  case break (not . (`Set.member` charset) . fst) input of
    ([], _) -> Nothing
    ((c, pos) : s, rest) -> return ((term, pos, Lexeme (c : map fst s)), rest)

one :: Term -> Char -> Lexer
one term char ((c, pos) : rest)
  | char == c = Just ((term, pos, Lexeme [c]), rest)
one term char _ = Nothing

grammar :: Grammar
grammar = fromStrings
  [ "1 . S = L"
  , "1 . L = l name = L ; L"
  , "1 . L = R"
  , "1 . R = \\ name . R"
  , "1 . R = E"
  , "2 . E = E + F"
  , "3 . E = E - F"
  , "4 . E = F"
  , "5 . F = F * U"
  , "6 . F = F / U"
  , "7 . F = U"
  , "8 . U = - U"
  , "8 . U = A"
  , "8 . A = A T"
  , "8 . A = A { name = T }"
  , "8 . A = T"
  , "8 . T = name"
  , "8 . T = int"
  , "8 . T = ( L )"
  ]
