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
import Data.Maybe (maybeToList)
import Data.Ord (comparing)
import Data.Set ((\\), Set)
import Data.Text (Text)
import Data.Text.Position
import Decision
import Frontend.Parser (parseGrammar)
import Grammar
import LR1Item
import qualified Data.Set as Set
import qualified Grammar.Check
import qualified RawGrammar as Raw
import Rule
import LR1State
import System.Exit (exitFailure)
import System.FilePath ((</>))
import Tables
import Symbol
import Text.Lexer.Default (dieOnLexerError, dieOnParserError)
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Data.Array (listArray)

infixl 6 <.>
(<.>) :: Doc -> Doc -> Doc
(<.>) = (<>)

data Request = Request
  { addendum :: [Text]
  , starts   :: Set Entity
  , grammar  :: Grammar
  }

data Toolbox = Toolbox
  { grammar  :: Grammar
  , start    :: LR1State
  , inverse  :: Map LR1State StateNum
  , states   :: Map StateNum LR1State
  , rawTable :: Table LR1State
  , table    :: Table StateNum
  , addendum :: [Text]
  }

type StateNum = Int

prepareToolboxes :: Request -> Map Entity Toolbox
prepareToolboxes Request {addendum, starts, grammar} =
  starts & foldMap \target ->
    Map.singleton target do
      let
        starterClause = Clause
          { reducer = "res"
          , points  = listArray (0, 0) [E (Just "res") target]
          , mark    = -1
          , pos     = Pos 0 0 "<nowhere>"
          }

        starterRule = Rule "Start" Nothing [starterClause]
        startingPosition = LR1Item.startRule
          target
          (Just (typeOf grammar target))
          starterClause
          LookForEOF

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

enumerateStates :: Table LR1State -> Map LR1State StateNum
enumerateStates Table {actions} =
  Map.fromList do
    zip (Monoidal.keys actions) [0..]

invert :: (Ord a, Ord b) => Map a b -> Map b a
invert = Map.foldMapWithKey (flip Map.singleton)

instance Semigroup StateNum where
  (<>) = error "Semigroup Int"

instance Monoid StateNum where
  mempty = error "Monoid Int"

dematerialise :: Table LR1State -> (Table StateNum, Map LR1State StateNum, Map StateNum LR1State)
dematerialise table =
  ( mapTableState (stateNumbers Map.!) table
  , stateNumbers
  , invert stateNumbers
  )
  where
    stateNumbers :: Map LR1State StateNum
    stateNumbers = enumerateStates table

createParserFile :: FilePath -> FilePath -> [String] -> IO ()
createParserFile grammarFile srcPath moduleName = do
  (addendum, starts, rules) <- parseGrammar grammarFile >>= dieOnLexerError >>= dieOnParserError
  case Grammar.Check.check starts Raw.Grammar {starts, rules} of
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
      , "{-# OPTIONS_GHC -Wno-unused-local-binds #-}"
      , "module" <+> cat (punctuate "." (map text moduleName)) <+> "("
      , nest 2 do
          vcat do
            punctuate "," do
              map (nest 2 . parse) do
                toList starts
      , ") where"
      , ""
      , "import Data.Text.IO.Utf8 qualified as Text"
      , "import Data.Kind qualified as Kind"
      , vcat do
          map pPrint addendum
      , ""
      , parserStackType
      , ""
      , vcat $ toList parser
      , ""
      , "currentPos :: ([Lexeme], Pos) -> Pos"
      , "currentPos = \\case"
      , "  ([],           end) -> end"
      , "  ((pos, _) : _, _)   -> pos"
      , ""
      ]
  writeFile fullPath (show moduleBody)

typeOf :: Grammar -> Entity -> Text
typeOf grammar entity = case toList (grammar.types ! entity) of
  [ty_] -> ty_
  other -> error $ "unexpected type set " <> show (entity ==> other) <> ", expected single type"

makeParser :: Entity -> Toolbox -> Doc
makeParser target toolbox@Toolbox {grammar, start, table, states, inverse} = vcat
  [ -- for each requested entrypoint TRG entity we generate:
    --
    -- Type for stack for that particular target entity.
    --
    -- TODO: stack have the same shape, parametrise with state type
    --
    genStateType target grammar states
  , ""
    -- generate GOTO table in form of set of functions, one per entity column in table
    --
  , vcat do
      punctuate "\n" do
        grammar.entities                 -- grab all entities
          & Set.delete "Start"           -- except "Start", it is auxillary
          & toList
          & map \entity ->               -- for each of them
              gotoEntity                 -- generate goto{Entity}For{Target} function
                target                   -- we generate separate set for each target entity
                (typeOf grammar target)
                (typeOf grammar entity)
                entity
                table
  , ""
    -- Generate ACTION table
    --
  , makeActionTable target toolbox
  , ""
  , parse target <+> ":: FilePath -> IO (Either LexerError (Either (Pos, [String])" <+> pPrint (grammar.types ! target) <.> "))"
  , parse target <+> "filepath = do"
  , "  text <- Text.readFile filepath"
  , "  case lexText filepath text" <+> terms <+> "of"
  , "    Left  err   -> pure (Left err)"
  , "    Right input -> pure (Right (" <.> run target <+> s target (inverse Map.! start) <+> "input Nil))"
  ]
  where
    terms = brackets $ fsep $ punctuate "," $ map (doubleQuotes . pPrint) $ toList do
      grammar.terminals \\ Set.fromList
        ["<num>", "<str>", "<Name>", "<name>", "<op>", "<pun>"]

makeActionTable :: Entity -> Toolbox -> Doc
makeActionTable target Toolbox {grammar, table, states} = vcat
  [ run target <+> "::" <+> st target <+> "a -> ([Lexeme], Pos) ->" <+> stack' target <+> "a -> Either (Pos, [String]) " <.> pPrint (grammar.types ! target)
  , run target <+> "= \\cases {"
  , vcat $ foldMap (pure . uncurry (stateShifts   target)) (Monoidal.assocs table.actions)
  , vcat $ foldMap (pure . uncurry (stateReducers target)) (Map.assocs states)
  , vcat $ foldMap (pure . uncurry (stateErrors   target)) (Map.assocs states)
  , "} where {"
  , vcat $ map (uncurry createReducer) $ Map.assocs reducerActions
  , "}"
  ]
  where
    reducerActions :: Map Pos (Int, FilePath, [Text], Text)
    reducerActions =
      (foldMap . foldMap) (foldMap grabAction . (.clauses)) grammar.rules

    grabAction :: Clause -> Map Pos (Int, FilePath, [Text], Text)
    grabAction rule =
      Map.singleton rule.pos
        ( rule.pos.column
        , rule.pos.filename
        , foldMap (maybeToList . (.name)) rule.points
        , rule.reducer
        )

    createReducer :: Pos -> (Int, FilePath, [Text], Text) -> Doc
    createReducer pos (column, _, params, body)
      | pos.line == 0 = empty
      | otherwise = vcat
      [ ("; " <.> actionMethod pos) <+> "pos" <+> fsep (map pPrint params) <+> "="
      -- , "{-# LINE " <+> pPrint line <+> text (show filePath) <+> "#-}"
      , text (replicate (column - 1) ' ') <.> pPrint body
      ]

{- |
  Generate stack type. It carries both state stack and data stack.

  > type Stack  xs = (St xs, Pos, Stack' xs)
  > data Stack' xs where
  >   Nil  ::                  Stack' '[]
  >   Push :: x -> Stack xs -> Stack'  (x : xs)

  Values of the stack will look like this:
  > (S123, pos, Push 'a' (S432, pos1, Push 1 (S0, pos2, Nil)))
  >  ~~~~  ~~~       ~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  >   ^     ^         ^                   ^
  >   |     |         |                   +-- rest of the stack
  >   |     |         +---------------------- top value
  >   |     +-------------------------------- top position
  >   +-------------------------------------- top state

  Stack might not have any values, but it always has at least 1 state:
  > (S0, pos2, Nil)

  Each name declared in this type will have the entrypoint name as suffix.
-}
parserStackType :: Doc
parserStackType = vcat
  [ "type Stack  st xs = (st xs, Pos, Stack' st xs)"
  , "data Stack' st xs where"
  , "  Nil  ::                     Stack' st '[]"
  , "  (:>) :: x -> Stack st xs -> Stack' st (x : xs)"
  , ""
  ]

{- |
  Generate a column of GOTO table for given entity to parse target entity.

__goto{Entity}For{Target} :: ([Lexeme], Pos) -> [Cond] -> Stack St{Target} a -> Either (Pos, [String]) {Target}
__goto{Entity}For{Target} toks term stk@(state, _, _) = case state of
  S{Target}9   -> __run{Target} S{Target}13  toks (term :> stk)
  S{Target}11  -> __run{Target} S{Target}14  toks (term :> stk)
  S{Target}176 -> __run{Target} S{Target}179 toks (term :> stk)
  S{Target}177 -> __run{Target} S{Target}180 toks (term :> stk)
  _ -> error ""

-}
gotoEntity :: Entity -> Text -> Text -> Entity -> Table StateNum -> Doc
gotoEntity target starterType entityType entity Table {actions} = vcat
  [ gotoMethod entity target <+> do ":: ([Lexeme], Pos) ->" <+> pPrint entityType <+> "->" <+> stack target <+> "a -> Either (Pos, [String])" <+> pPrint starterType
  , gotoMethod entity target <+> "toks term stk@(state, _, _) = case state of"
  , vcat do
      map stateTransition do
        Monoidal.assocs actions
  , "  _ -> error \"\""
  ]
  where
    stateTransition :: (StateNum, Action StateNum) -> Doc
    stateTransition (start, Action {goto})
      | Monoidal.member entity goto = do
        nest 2 do
          -- S{Target}{StateID} ->
          hang (s target start <+> "->") 2 do
            -- __run{Target} S{Target}{StateID'} toks (term :> stk)
            run target <+> s target (goto ! entity) <+> "toks (term :> stk)"
      | otherwise = mempty

{- |
  A type for parser state.

  > data St :: [Kind.Type] -> Kind.Type where
  >   S0 :: St (a)
  >   S1 :: St (([Text], Set Entity, [Rule]) : a)
  >   S2 :: St ([Symbol] : a)
  >   S3 :: St (Clause : a)
  >   S4 :: St (Entity : a)
  >   S5 :: St (Rule : a)
  >   ...

  Each name declared in this type will have the entrypoint name as suffix.
-}
genStateType :: Entity -> Grammar -> Map StateNum LR1State -> Doc
genStateType target grammar states = vcat
  [ "data " <.> st target <.> " :: [Kind.Type] -> Kind.Type where"
  , nest 2 do
      vcat $ map (uncurry (genState target grammar)) $ Map.assocs states
  ]

{- |
  A parser state.

  > data St :: [Kind.Type] -> Kind.Type where
  >   ...
  >   S1 :: St ([Text] : Set Entity : [Rule] : a)
  >   ...

  Each name declared in this type will have the entrypoint name as suffix.
-}
genState :: Entity -> Grammar -> StateNum -> LR1State -> Doc
genState target grammar number state =
  s target number <+> "::" <+> st target <+> do
    toList state.positions                -- grab all positions from state
      & map chooseReduce                  -- select already parsed points from each of them
      & maximumBy (comparing length)      -- take the longest sequence of points
      & map (pointToHaskellType grammar)  -- make it into a list of required types
      & (++ ["a"])                        -- make (Ty1 : Ty2 : Ty3 : ... : a) expression
      & punctuate " :"                    -- ...
      & fsep                              -- ...
      & parens                            -- ...
  where
    chooseReduce :: LR1Item -> [Symbol]
    chooseReduce pos = reverse pos.parsed

pointToHaskellType :: Grammar -> Symbol -> Doc
pointToHaskellType grammar = \case
  E _   e      -> pPrint (typeOf grammar e)
  T _ "<num>"  -> "Integer"
  T _ "<str>"  -> "Text"
  T _ "<name>" -> "Text"
  T _ "<Name>" -> "Text"
  T _   _      -> "()"

stateBinders :: LR1State -> Doc
stateBinders state = vcat do
  map positionBinders (toList state.positions)

positionBinders :: LR1Item -> Doc
positionBinders pos = case pos.locus of
  Nothing -> parens (stackToBinders $ reverse pos.parsed)
  Just {} -> empty

stackToBinders :: [Symbol] -> Doc
stackToBinders (start : rest) = parens do
  pointBinder start
    <+> ":>"
    <+> foldr decorate "__stk@(_, __pos, _)" rest
  where
    decorate :: Symbol -> Doc -> Doc
    decorate point k = parens do
      "_, _,"
        <+> pointBinder point
        <+> ":>"
        <+> k

stackToBinders [] = error ""

pair :: Doc -> Doc -> Doc
pair one another = parens ((one <.> ",") <+> another)

pointToBinder :: Text -> Text -> Term -> Doc
pointToBinder range name = \case
  "<num>"      -> parens do (pPrint range <.> ", NumberLiteral") <+> pPrint name
  "<str>"      -> parens do (pPrint range <.> ", StringLiteral") <+> pPrint name
  "<name>"     -> parens do (pPrint range <.> ", LowercaseName") <+> pPrint name
  "<Name>"     -> parens do (pPrint range <.> ", UppercaseName") <+> pPrint name
  "<op>"       -> parens do (pPrint range <.> ", Operator")      <+> pPrint name
  "<pun>"      -> parens do (pPrint range <.> ", Punctuator")    <+> pPrint name
  Term keyword -> parens do (pPrint range <.> ", ")              <+> doubleQuotes (pPrint keyword)

termIsBinding :: Term -> Bool
termIsBinding = \case
  "<num>"  -> True
  "<str>"  -> True
  "<name>" -> True
  "<Name>" -> True
  "<op>"   -> True
  "<pun>"  -> True
  Term _   -> False

pointBinder :: Symbol -> Doc
pointBinder pt = case pt.name of
  Nothing   -> "_"
  Just name -> pPrint name
  -- Just name -> parens (pPrint name <+> "::" <+> pointToHaskellType pt)

{-
  In grammar: store in rules line of reducing action.
              store (line => action) map.
-}
reduce :: Entity -> StateNum -> LR1Item -> Doc
reduce target state pos = vcat
  [ "-- lookahead " <.> pPrint pos.lookahead <.> ", entity " <.> pPrint pos.entity
  , ";" <+>  do
    hang ((s target state <+> input <+> positionBinders pos) <+> "->") 2
      case pos.clause.pos.line of
      0 -> "pure res"
      _ ->
        ("__goto" @: pos.entity <.> "For" @: target)
          <+> input
          <+> ("(" <.> actionMethod pos.clause.pos) <+> "__pos" <+> (fsep params <.> ")")
          <+> "__stk"
  ]
  where
    input :: Doc
    input = case pos.lookahead of
      LookForEOF  -> "([], __end)"
      LookForTerm tok -> parens (pointToBinder "__p" "tok" tok <+> ":" <+> ("__input" <.> ",") <+> "__end")

    params = foldMap (maybeToList . fmap pPrint . (.name)) pos.clause.points

stateReducers :: Entity -> StateNum -> LR1State -> Doc
stateReducers target number state =
  vcat $ state.positions & foldMap \pos -> do
    case pos.locus of
      Just {} -> []
      Nothing -> [reduce target number pos]

stateErrors :: Entity -> StateNum -> LR1State -> Doc
stateErrors target number state =
  ";" <+>  do
    hang (s target number <+> "__input" <+> "_" <+> "->") 2 do
      "Left " <+> parens
        ("currentPos __input," <+> brackets
          (fsep $ punctuate "," $ map (doubleQuotes . text . show) stateTerminals))
  where
    stateTerminals :: [Doc]
    stateTerminals = map pPrint $ Set.toList $ state.positions & foldMap \pos ->
      case pos.locus of
        Just (T _ t) -> Set.singleton (LookForTerm t)
        Nothing      -> Set.singleton pos.lookahead
        _            -> Set.empty

shift :: Entity -> StateNum -> Term -> StateNum -> Doc
shift target from term to = do
  ";" <+> do
    hang (s target from
      <+> parens (pointToBinder "__p" "n" term <+> ":" <+> ("__input" <.> ",") <+> "__end")
      <+> "__stk" <+> "->") 2
      (run target <+> s target to <+> "(__input, __end)"
        <+> parens (
          (if termIsBinding term then "n" else "()") <+> ":>" <+> parens (s target from <.> ", __p, __stk")
        ))

(@:) :: Doc -> Entity -> Doc
doc @: target = doc <.> pPrint target

parse :: Entity -> Doc
parse target = "parse" @: target

gotoMethod :: Entity -> Entity -> Doc
gotoMethod entity target = "__goto" @: entity <.> "For" @: target

actionMethod :: Pos -> Doc
actionMethod pos = "action" <.> pPrint pos.line

run :: Entity -> Doc
run target = "__run" @: target

s :: Entity -> StateNum -> Doc
s target n = "S" <.> pPrint target <.> int n

stack :: Entity -> Doc
stack target = "Stack St" @: target

stack' :: Entity -> Doc
stack' target = "Stack' St" @: target

-- push :: Entity -> Doc
-- push target = "Push" @: target

-- push' :: Entity -> Doc
-- push' target = "Push" @: target <.> "'"

-- nil :: Entity -> Doc
-- nil target = "Nil" @: target

st :: Entity -> Doc
st target = "St" @: target

{-
  TODO: pull "to" from ACTION table.
-}
stateShifts :: Entity -> StateNum -> Action StateNum -> Doc
stateShifts target from Action {action} =
  vcat $ action & Monoidal.assocs & foldMap \(lookahead, decisions) -> do
    decisions & foldMap \case
      Shift to -> case lookahead of
        LookForTerm term -> [shift target from term to]
        LookForEOF       -> []
      _ -> []
