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
  (addendum, start, rules) <- parseGrammar grammarFile >>= dieOnLexerError >>= dieOnParserError
  let grammar = Raw.Grammar start rules
  case Grammar.Check.check (Set.singleton start) grammar of
    Left errs -> do
      for_ errs (print . pPrint)
      exitFailure
    Right grammar -> do
      generateParserModule
        Request{addendum, starts = Set.singleton start, grammar}
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
            map (\starter -> nest 2 do "parse" <> pPrint starter) do
              toList starts
      , ") where"
      , "  "
      , "import Data.Text.IO.Utf8 qualified as Text"
      , "import Data.Kind qualified as Kind"
      , vcat do
          map pPrint addendum
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
  [ genStateType grammar states
  , "  "
  , vcat do
      punctuate "\n" do
        foldMap (pure . (\e -> gotoEntity starter (typeOf grammar starter) (typeOf grammar e) e table)) do
          Set.delete "Start" grammar.entities
  , "  "
  , "__run" <> pPrint starter <> " :: St a -> ([Lexeme], Pos) -> Stack' a -> Either (Pos, [String]) " <> pPrint (grammar.types ! starter)
  , "__run" <> pPrint starter <> " = \\cases {"
  , vcat $ foldMap (pure . uncurry (stateShifts starter)) (Monoidal.assocs table.actions)
  , vcat $ foldMap (pure . uncurry stateReducers) (Map.assocs states)
  , vcat $ foldMap (pure . uncurry stateErrors) (Map.assocs states)
  , "} where {"
  , vcat $ map (uncurry createReducer) $ Map.assocs reducerActions
  , "}"
  , "parse" <> pPrint starter <> " :: FilePath -> IO (Either LexerError (Either (Pos, [String]) " <> pPrint (grammar.types ! starter) <> "))"
  , "parse" <> pPrint starter <> " filepath = do"
  , "  text <- Text.readFile filepath"
  , "  case lexText filepath text" <+> terms <+> "of"
  , "    Left  err   -> pure (Left err)"
  , ("    Right input -> pure (Right (__run" <> pPrint starter) <+> (" S" <> pPrint (inverse Map.! start)) <+> "input Nil))"
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
  [ gotoName <+> do ":: ([Lexeme], Pos) -> " <> pPrint entityType <> " -> Stack a -> Either (Pos, [String]) " <> pPrint starterType
  , gotoName <+> "toks term stk@(state, _, _) = case state of"
  , vcat do
      map stateTransition do
        Monoidal.assocs actions
  , "  _ -> error \"\""
  ]
  where
    gotoName :: Doc
    gotoName = "__goto" <> pPrint entity

    stateTransition :: (Int, Action Int) -> Doc
    stateTransition (start, Action {goto})
      | Monoidal.member entity goto = do
        "  " <> st start <> " -> __run" <> pPrint starter <> " " <> st (goto ! entity) <> " toks (term :> stk)"
      | otherwise = mempty

genStateType :: Grammar -> Map Int State -> Doc
genStateType grammar states = vcat
  [ "data St :: [Kind.Type] -> Kind.Type where"
  , nest 2 do
      vcat $ map (uncurry (genState grammar)) $ Map.assocs states
  ]

genState :: Grammar -> Int -> State -> Doc
genState grammar number state =
  ("S" <> pPrint number) <+> "::" <+> "forall a. St" <+> do
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

stateBinders :: State -> Doc
stateBinders state = vcat do
  map positionBinders (toList state.positions)

positionBinders :: Position -> Doc
positionBinders pos = case pos.locus of
  Nothing -> parens (stackToBinders $ reverse pos.parsed)
  Just {} -> empty

-- (t :> (_, _, _ :> (_, _, f :> stk@(_, pos, _))))
stackToBinders :: [Point] -> Doc
stackToBinders (start : rest) = pointBinder start <+> ":>" <+> foldr decorate "__stk@(_, __pos, _)" rest
  where
    decorate :: Point -> Doc -> Doc
    decorate point k = pointBinder point <+> ":?" <+> k

stackToBinders [] = error ""

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
reduce :: Int -> Position -> Doc
reduce state pos = vcat
  [ "-- lookahead " <> pPrint pos.lookahead <> ", entity " <> pPrint pos.entity
  , ";" <+>  do
    hang ((("S" <> int state) <+> input <+> positionBinders pos) <+> "->") 2
      case pos.clause.pos.line of
      0 -> "pure res"
      _ ->
        ("__goto" <> pPrint pos.entity)
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

stateReducers :: Int -> State -> Doc
stateReducers number state =
  vcat $ state.positions & foldMap \pos -> do
    case pos.locus of
      Just {} -> []
      Nothing -> [reduce number pos]

stateErrors :: Int -> State -> Doc
stateErrors number state =
  ";" <+>  do
    hang (st number <+> "__input" <+> "_" <+> "->") 2 do
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

{-
  S0  (TNumber n : toks) stk -> run S5   toks (n     :> (S0 , stk))
-}

shift :: Entity -> Int -> Term -> Int -> Doc
shift starter from term to = do
  ";" <+> do
    hang (st from
      <+> parens (pointToBinder "__p" "n" term <+> ":" <+> ("__input" <> ",") <+> "__end")
      <+> "__stk" <+> "->") 2
      (("__run" <> pPrint starter) <+> st to <+> "(__input, __end)"
        <+> parens ((if termIsBinding term then "n" else "()") <+> ":>" <+> parens (st from <> ", __p, __stk")))

st :: Int -> Doc
st n = "S" <> int n

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
