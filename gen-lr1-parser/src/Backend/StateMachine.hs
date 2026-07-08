{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
module Backend.StateMachine where

import Data.Foldable
import Data.Function ((&))
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Monoidal ((!))
import Data.Map.Monoidal qualified as Monoidal
import Data.Maybe (maybeToList)
import Data.Ord (comparing)
import Data.Set ((\\))
import Data.Text (Text)
import Decision
import Frontend.Parser0 (parseGrammar)
import Grammar
import Position
import qualified Data.Set as Set
import qualified Data.Text as Text
import State
import System.FilePath
import Tables
import Term
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Rule
import Data.Text.Position

enumerateStates :: Table State -> Map State Int
enumerateStates Table {actions} =
  Map.fromList do
    zip (Monoidal.keys actions) [0..]

invert :: (Ord a, Ord b) => Map a b -> Map b a
invert = Map.foldMapWithKey (flip Map.singleton)

func :: (Ord a) => Map a b -> a -> b
func = (Map.!)

instance Semigroup Int where
  (<>) = error "Semigroup Int"

instance Monoid Int where
  mempty = error "Monoid Int"

dematerialise :: Table State -> (Table Int, Map Int State)
dematerialise table =
  ( mapTableState (func stateNumbers) table
  , invert stateNumbers
  )
  where
    stateNumbers :: Map State Int
    stateNumbers = enumerateStates table

run :: FilePath -> FilePath -> [String] -> IO ()
run grammarFile srcPath moduleName = do
  (addendum, grammar) <- parseGrammar grammarFile
  generateParserModule
    addendum
    grammar
    srcPath
    moduleName

generateParserModule :: [Text] -> Grammar -> FilePath -> [String] -> IO ()
generateParserModule addendum grammar pathToSrc moduleName = do
  let tables = makeTables grammar (startingState grammar)
  let parser = makeParser grammar tables
  let fullPath = pathToSrc </> intercalate "/" moduleName <> ".hs"
  let
    terms = pPrint $ toList do
      grammar.terminals \\ Set.fromList
        ["<num>", "<str>", "<Name>", "<name>", "<op>", "<pun>"]
  let
    moduleBody = vcat
      [ "{-# language PatternSynonyms #-}"
      , "{-# OPTIONS_GHC -Wno-incomplete-patterns #-}"
      , "{-# OPTIONS_GHC -Wno-unused-matches #-}"
      , "module"
          <+> cat (punctuate "." (map text moduleName))
          <+> parens ("parse")
          <+> "where"
      , "  "
      , "import Data.Text.IO.Utf8 qualified as Text"
      , "import Data.Kind qualified as Kind"
      , vcat do
          map (text . Text.unpack) addendum
      , "  "
      , "data Stack' xs where"
      , "  Nil  ::      Stack' '[]"
      , "  (:>) :: x -> Stack xs -> Stack' (x : xs)"
      , "  "
      , "type Stack a = (St a, Pos, Stack' a)"
      , "  "
      , "pattern (:?) :: a -> Stack xs -> Stack (a : xs)"
      , "pattern a :? xs <- (_, _, a :> xs)"
      , "  "
      , "infixr 2 :>, :?"
      , "  "
      , parser
      , "  "
      , "currentPos :: ([Lexeme], Pos) -> Pos"
      , "currentPos = \\case"
      , "  ([],           end) -> end"
      , "  ((pos, _) : _, _)   -> pos"
      , "  "
      , "parse :: FilePath -> IO (Either LexerError (Either (Pos, [String]) " <> pPrint grammar.starter <> "))"
      , "parse filepath = do"
      , "  text <- Text.readFile filepath"
      , "  case lexText filepath text" <+> terms <+> "of"
      , "    Left  err   -> pure (Left err)"
      , "    Right input -> pure (Right (run S0 input Nil))"

      ]
  writeFile fullPath (show moduleBody)

makeParser :: Grammar -> Table State -> Doc
makeParser grammar raw = vcat
  [ genStateType states
  , "  "
  , vcat do
      punctuate "\n" do
        foldMap (pure . (\e -> gotoEntity grammar.starter e table)) do
          Set.delete "Start" grammar.entities
  , "  "
  , "run :: St a -> ([Lexeme], Pos) -> Stack' a -> Either (Pos, [String]) " <> pPrint grammar.starter
  , "run = \\cases {"
  , vcat $ foldMap (pure . uncurry stateShifts) (Monoidal.assocs table.actions)
  , vcat $ foldMap (pure . uncurry stateReducers) (Map.assocs states)
  , vcat $ foldMap (pure . uncurry stateErrors) (Map.assocs states)
  , "} where {"
  , vcat $ map (uncurry createReducer) $ Map.assocs reducerActions
  , "}"
  ]
  where
    (table, states) = dematerialise raw

    reducerActions :: Map Int (Int, FilePath, [Text], Text)
    reducerActions = (foldMap . foldMap) grabAction grammar.rules

    grabAction :: Rule -> Map Int (Int, FilePath, [Text], Text)
    grabAction rule =
      Map.singleton rule.pos.line
        ( rule.pos.column
        , rule.pos.filename
        , foldMap (maybeToList . (.name)) rule.points
        , rule.reducer
        )

    createReducer :: Int -> (Int, FilePath, [Text], Text) -> Doc
    createReducer line (column, filePath, params, body) = vcat
      [ ("; action" <> pPrint line) <+> "pos" <+> fsep (map pPrint params) <+> "="
      , "{-# LINE " <+> pPrint line <+> text (show filePath) <+> "#-}"
      , text (replicate (column - 1) ' ') <> pPrint body
      ]

gotoEntity :: Entity -> Entity -> Table Int -> Doc
gotoEntity starter entity Table {actions} = vcat
  [ gotoName <+> do ":: ([Lexeme], Pos) -> " <> pPrint entity <> " -> Stack a -> Either (Pos, [String]) " <> pPrint starter
  , gotoName <+> "toks term stk@(state, _, _) = case state of"
  , vcat do
      map stateTransition do
        Monoidal.assocs actions
  , "  _ -> error \"\""
  ]
  where
    gotoName :: Doc
    gotoName = "goto" <> pPrint entity

    stateTransition :: (Int, Action Int) -> Doc
    stateTransition (start, Action {goto})
      | Monoidal.member entity goto = do
        "  " <> st start <> " -> run " <> st (goto ! entity) <> " toks (term :> stk)"
      | otherwise = mempty

genStateType :: Map Int State -> Doc
genStateType states = vcat
  [ "data St :: [Kind.Type] -> Kind.Type where"
  , nest 2 do
      vcat $ map (uncurry genState) $ Map.assocs states
  ]

genState :: Int -> State -> Doc
genState number state =
  ("S" <> pPrint number) <+> "::" <+> "forall a. St" <+> do
    parens
      $ fsep
      $ punctuate " :"
      $ (++ ["a"])
      $ map pointToHaskellType -- and laugh
      $ maximumBy (comparing length)
      $ map chooseReduce
      $ toList state.positions
  where
    chooseReduce :: Position -> [Point]
    chooseReduce pos = reverse pos.parsed

pointToHaskellType :: Point -> Doc
pointToHaskellType = \case
  E _   e      -> pPrint e
  T _ "<num>"  -> "Integer"
  T _ "<str>"  -> "Text"
  T _ "<name>" -> "Text"
  T _ "<Name>" -> "Text"
  T _   _      -> "()"

stateBinders :: State -> Doc
stateBinders state = vcat do
  map positionBinders (toList state.positions)

positionBinders :: Position -> Doc
positionBinders pos = case pos.locus of
  Nothing -> parens (stackToBinders $ reverse pos.parsed)
  Just {} -> empty

-- (t :> (_, _, _ :> (_, _, f :> stk@(_, pos, _))))
stackToBinders :: [Point] -> Doc
stackToBinders (start : rest) = pointBinder start <+> ":>" <+> foldr decorate "stk@(_, pos, _)" rest
  where
    decorate :: Point -> Doc -> Doc
    decorate point k = pointBinder point <+> ":?" <+> k

stackToBinders [] = error ""

pair :: Doc -> Doc -> Doc
pair f s = parens ((f <> ",") <+> s)

pointToBinder :: Text -> Text -> Term -> Doc
pointToBinder range name = \case
  "<num>"      -> parens do (text (Text.unpack range) <> ", NumberLiteral") <+> text (Text.unpack name)
  "<str>"      -> parens do (text (Text.unpack range) <> ", StringLiteral") <+> text (Text.unpack name)
  "<name>"     -> parens do (text (Text.unpack range) <> ", LowercaseName") <+> text (Text.unpack name)
  "<Name>"     -> parens do (text (Text.unpack range) <> ", UppercaseName") <+> text (Text.unpack name)
  "<op>"       -> parens do (text (Text.unpack range) <> ", Operator")      <+> text (Text.unpack name)
  "<pun>"      -> parens do (text (Text.unpack range) <> ", Punctuator")    <+> text (Text.unpack name)
  Term keyword -> parens do (text (Text.unpack range) <> ", Reserved")      <+> doubleQuotes (pPrint keyword)

termIsBinding :: Term -> Bool
termIsBinding = \case
  "<num>"  -> True
  "<str>"  -> True
  "<name>" -> True
  "<Name>" -> True
  "<op>"   -> True
  "<pun>"  -> True
  Term _   -> False

pointBinder :: Point -> Doc
pointBinder pt = case pt.name of
  Nothing   -> "_"
  Just name -> pPrint name
  -- Just name -> parens (pPrint name <+> "::" <+> pointToHaskellType pt)

{-
  In grammar: store in rules line of reducing action.
              store (line => action) map.
-}
reduce :: Int -> Position -> Doc
reduce state pos = vcat
  [ ";" <+>  do
    hang ((("S" <> int state) <+> input <+> positionBinders pos) <+> "->") 2
      case pos.entity of
      "Start" -> "pure res"
      _       ->
        ("goto" <> pPrint pos.entity)
          <+> input
          <+> ("(action" <> pPrint pos.rule.pos.line) <+> "pos" <+> (fsep params <> ")")
          <+> "stk"
  ]
  where
    input :: Doc
    input = case pos.lookahead of
      "$" -> "([], end)"
      tok -> parens (pointToBinder "p" "tok" tok <+> ":" <+> ("input" <> ",") <+> "end")

    params = foldMap (maybeToList . fmap pPrint . (.name)) pos.rule.points

stateReducers :: Int -> State -> Doc
stateReducers number state =
  vcat $ state.positions & foldMap \pos -> do
    case pos.locus of
      Just {} -> []
      Nothing -> [reduce number pos]

stateErrors :: Int -> State -> Doc
stateErrors number state =
  ";" <+>  do
    hang (st number <+> "input" <+> "_" <+> "->") 2 do
      "Left " <+> parens
        ("currentPos input," <+> brackets
          (fsep $ punctuate "," $ map (text . show) stateTerminals))
  where
    stateTerminals :: [Term]
    stateTerminals = Set.toList $ state.positions & foldMap \pos ->
      case pos.locus of
        Just (T _ t) -> Set.singleton t
        Nothing      -> Set.singleton pos.lookahead
        _            -> Set.empty

{-
  S0  (TNumber n : toks) stk -> run S5   toks (n     :> (S0 , stk))
-}

shift :: Int -> Term -> Int -> Doc
shift from term to = do
  ";" <+> do
    hang (st from
      <+> parens (pointToBinder "p" "n" term <+> ":" <+> ("input" <> ",") <+> "end")
      <+> "stk" <+> "->") 2
      ("run" <+> st to <+> "(input, end)"
        <+> parens ((if termIsBinding term then "n" else "()") <+> ":>" <+> parens (st from <> ", p, stk")))

st :: Int -> Doc
st n = "S" <> int n

{-
  TODO: pull "to" from ACTION table.
-}
stateShifts :: Int -> Action Int -> Doc
stateShifts from Action {action} =
  vcat $ action & Monoidal.assocs & foldMap \(term, decisions) -> do
    decisions & foldMap \case
      Shift to -> [shift from term to]
      _        -> []
