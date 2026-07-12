{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
module Backend.StateMachine where

-- import qualified Data.Text as Text
import Control.Monad
import Data.Foldable
import Data.Function ((&))
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Monoidal ((!), (==>))
import Data.Map.Monoidal qualified as Monoidal
import Data.Maybe (maybeToList, fromMaybe)
import Data.Ord (comparing)
import Data.Set ((\\), Set)
import Data.Text (Text)
import Data.Text.Position
import Decision
import Frontend.Parser (parseGrammar)
import Grammar
import Position
import qualified Data.Set as Set
import qualified Grammar.Check
import qualified RawGrammar as Raw
import Rule
import State
import System.Exit (exitFailure)
import System.FilePath
import Tables
import Term
import Text.Lexer.Default (dieOnLexerError, dieOnParserError)
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import qualified RawGrammar as Raw
import Data.Array (listArray)

data Request = Request
  { addendum :: [Text]
  , starts   :: Set Entity
  , grammar  :: Grammar
  }

data Toolbox = Toolbox
  { grammar  :: Grammar
  , start    :: State
  , inverse  :: Map State Int
  , states   :: Map Int State
  , rawTable :: Table State
  , table    :: Table Int
  , addendum :: [Text]
  }

prepareToolboxes :: Request -> Map Entity Toolbox
prepareToolboxes Request {addendum, starts, grammar} =
  starts & foldMap \starter ->
    Map.singleton starter do
      let
        starterClause = Clause
          { reducer = "res"
          , points  = listArray (0, 0) [E (Just "res") starter]
          , mark    = -1
          , pos     = Pos 0 0 "<nowhere>"
          }

        starterRule = Rule "Start" Nothing [starterClause]
        startingPosition = Position.start
          starter
          (Just (typeOf grammar starter))
          starterClause
          Nothing

        grammar' = grammar
          { rules = grammar.rules <> do
              "Start" ==> Set.singleton starterRule
          }

        start = closure grammar' do Set.singleton startingPosition
      let rawTable = makeTables grammar start
      let (table, inverse, states) = dematerialise rawTable
      Toolbox
        { grammar = grammar'
        , start
        , inverse
        , states
        , rawTable
        , table
        , addendum
        }

enumerateStates :: Table State -> Map State Int
enumerateStates Table {actions} =
  Map.fromList do
    zip (Monoidal.keys actions) [0..]

invert :: (Ord a, Ord b) => Map a b -> Map b a
invert = Map.foldMapWithKey (flip Map.singleton)

instance Semigroup Int where
  (<>) = error "Semigroup Int"

instance Monoid Int where
  mempty = error "Monoid Int"

dematerialise :: Table State -> (Table Int, Map State Int, Map Int State)
dematerialise table =
  ( mapTableState (stateNumbers Map.!) table
  , stateNumbers
  , invert stateNumbers
  )
  where
    stateNumbers :: Map State Int
    stateNumbers = enumerateStates table

run :: FilePath -> FilePath -> [String] -> IO ()
run grammarFile srcPath moduleName = do
  (addendum, starts, rules) <- parseGrammar grammarFile >>= dieOnLexerError >>= dieOnParserError
  let grammar = Raw.Grammar starts rules
  case Grammar.Check.check starts grammar of
    Left errs -> do
      for_ errs (print . pPrint)
      exitFailure
    Right grammar -> do
      generateParserModule
        Request{addendum, starts, grammar}
        srcPath
        moduleName

generateParserModule :: Request -> FilePath -> [String] -> IO ()
generateParserModule request@Request{addendum, starts, grammar} pathToSrc moduleName = do
  let toolboxes = prepareToolboxes request
  let starting = startingState grammar
  let tables = makeTables grammar starting
  let (table, inv, states) = dematerialise tables
  let problems = conflicts tables starting
  unless (null problems) do
    for_ problems \problem -> do
      print (pPrint problem)
      putStrLn ""
    exitFailure
  let parser = Map.mapWithKey makeParser toolboxes
  let fullPath = pathToSrc </> intercalate "/" moduleName <> ".hs"
  let
    moduleBody = vcat
      [ "{-# language PatternSynonyms #-}"
      , "{-# OPTIONS_GHC -Wno-incomplete-patterns #-}"
      , "{-# OPTIONS_GHC -Wno-unused-matches #-}"
      , "module" <+> cat (punctuate "." (map text moduleName)) <+> "("
      , nest 2 do
          vcat do
            punctuate "," do
              map (\starter -> nest 2 do "parse" <> pPrint starter) do
                toList starts
      , ") where"
      , "  "
      , "import Data.Text.IO.Utf8 qualified as Text"
      , "import Data.Kind qualified as Kind"
      , vcat do
          map pPrint addendum
      , "  "
      , vcat $ toList parser
      , "  "
      , "currentPos :: ([Lexeme], Pos) -> Pos"
      , "currentPos = \\case"
      , "  ([],           end) -> end"
      , "  ((pos, _) : _, _)   -> pos"
      , "  "
      ]
  writeFile fullPath (show moduleBody)

typeOf :: Grammar -> Entity -> Text
typeOf grammar entity = case toList (grammar.types ! entity) of
  [ty_] -> ty_
  other -> error $ "unexpected type set " <> show (entity ==> other) <> ", expected single type"

makeParser :: Entity -> Toolbox -> Doc
makeParser starter Toolbox {grammar, start, table, states, inverse} = vcat
  [ "data Stack" <> pPrint starter <> "' xs where"
  , "  Nil" <> pPrint starter <> "  ::      Stack" <> pPrint starter <> "' '[]"
  , "  Push" <> pPrint starter <> " :: x -> Stack" <> pPrint starter <> " xs -> Stack" <> pPrint starter <> "' (x : xs)"
  , "  "
  , "type Stack" <> pPrint starter <> " a = (St" <> pPrint starter <> " a, Pos, Stack" <> pPrint starter <> "' a)"
  , "  "
  , "pattern Push" <> pPrint starter <> "' :: a -> Stack" <> pPrint starter <> " xs -> Stack" <> pPrint starter <> " (a : xs)"
  , "pattern Push" <> pPrint starter <> "' a xs <- (_, _, Push" <> pPrint starter <> " a xs)"
  , "  "

  , genStateType starter grammar states
  , "  "
  , vcat do
      punctuate "\n" do
        foldMap (pure . (\e -> gotoEntity starter (typeOf grammar starter) (typeOf grammar e) e table)) do
          Set.delete "Start" grammar.entities
  , "  "
  , "__run" <> pPrint starter <> " :: St" <> pPrint starter <> " a -> ([Lexeme], Pos) -> Stack" <> pPrint starter <> "' a -> Either (Pos, [String]) " <> pPrint (grammar.types ! starter)
  , "__run" <> pPrint starter <> " = \\cases {"
  , vcat $ foldMap (pure . uncurry (stateShifts   starter)) (Monoidal.assocs table.actions)
  , vcat $ foldMap (pure . uncurry (stateReducers starter)) (Map.assocs states)
  , vcat $ foldMap (pure . uncurry (stateErrors   starter)) (Map.assocs states)
  , "} where {"
  , vcat $ map (uncurry createReducer) $ Map.assocs reducerActions
  , "}"
  , "parse" <> pPrint starter <> " :: FilePath -> IO (Either LexerError (Either (Pos, [String]) " <> pPrint (grammar.types ! starter) <> "))"
  , "parse" <> pPrint starter <> " filepath = do"
  , "  text <- Text.readFile filepath"
  , "  case lexText filepath text" <+> terms <+> "of"
  , "    Left  err   -> pure (Left err)"
  , ("    Right input -> pure (Right (__run" <> pPrint starter) <+> (st starter (inverse Map.! start)) <+> ("input Nil" <> pPrint starter <> "))")
  ]
  where
    terms = brackets $ fsep $ punctuate "," $ map (doubleQuotes . pPrint) $ toList do
      grammar.terminals \\ Set.fromList
        ["<num>", "<str>", "<Name>", "<name>", "<op>", "<pun>"]

    reducerActions :: Map Int (Int, FilePath, [Text], Text)
    reducerActions =
      (foldMap . foldMap) (foldMap grabAction . (.clauses)) grammar.rules

    grabAction :: Clause -> Map Int (Int, FilePath, [Text], Text)
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
      -- , "{-# LINE " <+> pPrint line <+> text (show filePath) <+> "#-}"
      , text (replicate (column - 1) ' ') <> pPrint body
      ]

gotoEntity :: Entity -> Text -> Text -> Entity -> Table Int -> Doc
gotoEntity starter starterType entityType entity Table {actions} = vcat
  [ gotoName <+> do ":: ([Lexeme], Pos) -> " <> pPrint entityType <> " -> Stack" <> pPrint starter <> " a -> Either (Pos, [String]) " <> pPrint starterType
  , gotoName <+> "toks term stk@(state, _, _) = case state of"
  , vcat do
      map stateTransition do
        Monoidal.assocs actions
  , "  _ -> error \"\""
  ]
  where
    gotoName :: Doc
    gotoName = "__goto" <> pPrint entity <> "For" <> pPrint starter

    stateTransition :: (Int, Action Int) -> Doc
    stateTransition (start, Action {goto})
      | Monoidal.member entity goto = do
        "  " <> st starter start <> " -> __run"
             <> pPrint starter   <> " "
             <> st starter (goto ! entity) <> " toks (Push"
            <> pPrint starter <> " term stk)"
      | otherwise = mempty

genStateType :: Entity -> Grammar -> Map Int State -> Doc
genStateType starter grammar states = vcat
  [ "data St" <> pPrint starter <> " :: [Kind.Type] -> Kind.Type where"
  , nest 2 do
      vcat $ map (uncurry (genState starter grammar)) $ Map.assocs states
  ]

genState :: Entity -> Grammar -> Int -> State -> Doc
genState target grammar number state =
  st target number <+> "::" <+> ("St" <> pPrint target) <+> do
    parens
      $ fsep
      $ punctuate " :"
      $ (++ ["a"])
      $ map (pointToHaskellType {- and laugh -} grammar)
      $ maximumBy (comparing length)
      $ map chooseReduce
      $ toList state.positions
  where
    chooseReduce :: Position -> [Point]
    chooseReduce pos = reverse pos.parsed

pointToHaskellType :: Grammar -> Point -> Doc
pointToHaskellType grammar = \case
  E _   e      -> pPrint (typeOf grammar e)
  T _ "<num>"  -> "Integer"
  T _ "<str>"  -> "Text"
  T _ "<name>" -> "Text"
  T _ "<Name>" -> "Text"
  T _   _      -> "()"

stateBinders :: Entity -> State -> Doc
stateBinders target state = vcat do
  map (positionBinders target) (toList state.positions)

positionBinders :: Entity -> Position -> Doc
positionBinders target pos = case pos.locus of
  Nothing -> parens (stackToBinders target $ reverse pos.parsed)
  Just {} -> empty

stackToBinders :: Entity -> [Point] -> Doc
stackToBinders target (start : rest) = parens do
  ("Push" <> pPrint target)
    <+> pointBinder start
    <+> foldr decorate "__stk@(_, __pos, _)" rest
  where
    decorate :: Point -> Doc -> Doc
    decorate point k = parens do
      ("Push" <> pPrint target <> "'")
        <+> pointBinder point
        <+> k

stackToBinders _ [] = error ""

pair :: Doc -> Doc -> Doc
pair f s = parens ((f <> ",") <+> s)

pointToBinder :: Text -> Text -> Term -> Doc
pointToBinder range name = \case
  "<num>"      -> parens do (pPrint range <> ", NumberLiteral") <+> pPrint name
  "<str>"      -> parens do (pPrint range <> ", StringLiteral") <+> pPrint name
  "<name>"     -> parens do (pPrint range <> ", LowercaseName") <+> pPrint name
  "<Name>"     -> parens do (pPrint range <> ", UppercaseName") <+> pPrint name
  "<op>"       -> parens do (pPrint range <> ", Operator")      <+> pPrint name
  "<pun>"      -> parens do (pPrint range <> ", Punctuator")    <+> pPrint name
  Term keyword -> parens do (pPrint range <> ", ")              <+> doubleQuotes (pPrint keyword)

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
reduce :: Entity -> Int -> Position -> Doc
reduce target state pos = vcat
  [ "-- lookahead " <> pPrint pos.lookahead <> ", entity " <> pPrint pos.entity
  , ";" <+>  do
    hang ((st target state <+> input <+> positionBinders target pos) <+> "->") 2
      case pos.clause.pos.line of
      0 -> "pure res"
      _ ->
        ("__goto" <> pPrint pos.entity <> "For" <> pPrint target)
          <+> input
          <+> ("(action" <> pPrint pos.clause.pos.line) <+> "__pos" <+> (fsep params <> ")")
          <+> "__stk"
  ]
  where
    input :: Doc
    input = case pos.lookahead of
      Nothing  -> "([], __end)"
      Just tok -> parens (pointToBinder "__p" "tok" tok <+> ":" <+> ("__input" <> ",") <+> "__end")

    params = foldMap (maybeToList . fmap pPrint . (.name)) pos.clause.points

stateReducers :: Entity -> Int -> State -> Doc
stateReducers target number state =
  vcat $ state.positions & foldMap \pos -> do
    case pos.locus of
      Just {} -> []
      Nothing -> [reduce target number pos]

stateErrors :: Entity -> Int -> State -> Doc
stateErrors target number state =
  ";" <+>  do
    hang (st target number <+> "__input" <+> "_" <+> "->") 2 do
      "Left " <+> parens
        ("currentPos __input," <+> brackets
          (fsep $ punctuate "," $ map (doubleQuotes . text . show) stateTerminals))
  where
    stateTerminals :: [Term]
    stateTerminals = map (fromMaybe "<eof>") $ Set.toList $ state.positions & foldMap \pos ->
      case pos.locus of
        Just (T _ t) -> Set.singleton (Just t)
        Nothing      -> Set.singleton pos.lookahead
        _            -> Set.empty

shift :: Entity -> Int -> Term -> Int -> Doc
shift starter from term to = do
  ";" <+> do
    hang (st starter from
      <+> parens (pointToBinder "__p" "n" term <+> ":" <+> ("__input" <> ",") <+> "__end")
      <+> "__stk" <+> "->") 2
      (("__run" <> pPrint starter) <+> st starter to <+> "(__input, __end)"
        <+> parens (
          ("Push" <> pPrint starter) <+>
          (if termIsBinding term then "n" else "()") <+> parens (st starter from <> ", __p, __stk")
        ))

st :: Entity -> Int -> Doc
st target n = "S" <> pPrint target <> int n

{-
  TODO: pull "to" from ACTION table.
-}
stateShifts :: Entity -> Int -> Action Int -> Doc
stateShifts starter from Action {action} =
  vcat $ action & Monoidal.assocs & foldMap \(term, decisions) -> do
    decisions & foldMap \case
      Shift to -> case term of
        Just term -> [shift starter from term to]
        Nothing   -> []
      _ -> []
