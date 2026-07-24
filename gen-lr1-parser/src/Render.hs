{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{- |
  Parsing automata is rendered as follows:

  > module {name} (
  >   parse{targets}...
  > )
  >
  > // required imports
  > // user-specified imports
  >
  > // parsing stack type definiion
  >
  > {for each target}
  >   // GOTO-table for that specific target
  >   // ACTION-table for that specific target
  >   // parse{target} function
  > {end}

  Targets are the non-terminals, designated as "start" in the grammar.

  We generate independent parsing automatas for each of them.

  Parsing stack is the same for all parsers.
  Each record in it contains state, position, and parsed object.
  Except the first record, is has no object.

  GOTO-tables, which have dimensions of `[State, NonTerm] -> State`
  are rendered as sets of functions, one for each non-terminal in grammar.

  Each GOTO-function checks top state and pushes another state on top of it
  along with the value of parsed nonterminal.

  For instance, the transition between states with kernels

  > T <- ( .E ) {+}

  and

  > T <- ( E .) {+}

  is facilitated by clause like this

  > gotoEForX :: Stack StX xs -> EType -> [Lexeme] -> Pos -> Either (Pos, [String]) XType
  > gotoEForX stack@(state, pos, _) parsed = case state of
  >   ...
  >   StX_N -> actionForX (StX_M, pos, (parsed :> stack))
  >   ...

  where @X@ is a parsing target type.

  State is pushed on top of checked one so we can later return to the `StX_N` state to react
  on parsing on some other entity the @E@ is a part of.

  ACTION-table is rendered as a single function:

  > actionForX :: Stack StX xs -> [Lexeme] -> Pos -> Either (Pos, [String]) XType
  > actionForX = \cases

  There are cases for each state that has "shift" or "reduce" actions and error handling.

  Shift action is a clause like

  >   -- E <- A .<name> B
  >   stk@(StX_N, _, _) ((pos, LowercaseName n) : input) -> actionForX (StX_M, pos, n :> stk) input

  It pushes (new state, position, lexeme value) on top of the stack, and consumes the lexeme.

  Reduce action is a clause like

  >   -- E <- E + F . {+}
  >   ___@(StX_N, _, b :> (_, _, x :> (_, _, a :> stk@(_, pos, _)))) input@((_, "+") : _) ->
  >    gotoEForX stk (mkE pos a x b) input

  It removes required elements from stack and reaches to some older state before @a@, @x@ and @b@
  were pushed onto stack. Then we do the GOTO-action for than older state.

  Error clause is fired when no valid input for that state it present.
  It throws expected lexemes and position as error.

-}
module Render where

import Data.Foldable ( Foldable(toList), for_ )
import Data.Function ((&))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map ((!))
import Data.Map qualified as Map
import Data.Map.Monoidal qualified as Monoidal
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

import Decision
import Frontend.Parser (parseGrammar)
import Grammar
import Grammar.Check (check)
import LR1State
import Rule
import Symbol
import System.Exit (exitFailure)
import System.FilePath ((</>))
import Tables
import Text.Lexer.Default (dieOnLexerError, dieOnParserError)
import Pretty
import Transitions

{- |
  Type of object a non-terminal is parsed into.
-}
nonTermType :: Cache -> NonTerminal -> Text
nonTermType cache entity = cache.types ! entity

{- |
  Type of object a symbol is parsed into.
-}
symbolType :: Cache -> Symbol -> Text
symbolType cache = \case
  Term    term   -> termType term
  NonTerm entity -> nonTermType cache entity

{- |
  List of types given state requires to be on top of the parse stack.
-}
stateTypeString :: Cache -> LR1State -> Text
stateTypeString cache state = mconcat
  [ "("
  , Text.intercalate " : " do
      reverse do
        "a" : map (symbolType cache) (longestPrefix state)
  , ")"
  ]

{- |
  Definition of type for parser states.

  > data StFoo xs where
  >   S0 :: StFoo a
  >   S1 :: StFoo (Foo : a)
  >   ...
  >   S31 :: StFoo (Integer : () : Bar : a)
  >   ...
-}
stateTypeDecl :: Cache -> Target -> Text
stateTypeDecl cache target = Text.unlines
  ( "data " <> stType target <> " xs where"
  : do
      cache.states & Map.foldMapWithKey \number state -> do
        ["  " <> s target number <> " :: " <> stType target <> " " <> stateTypeString cache state]
  )

{- |
  Wrapper to prevent mixing target non-terminal and locally-parsed nonterminal.
-}
newtype Target = Target {entity :: NonTerminal}
  deriving newtype (Eq, Ord, IsString)

{- |
  Type of state.

  > StX (E : () : F : a)
-}
stType :: Target -> Text
stType target = "St" <> target.entity

{- |
  Stack type is parametrised by parsing state.

  > Stack StFoo xs
-}
stackType :: Target -> Text
stackType target = "Stack " <> stType target <> " xs"

{- |
  State constructor for target non-terminal with given number.
-}
s :: Target -> Int -> Text
s target index = "St" <> target.entity <> "_" <> Text.pack (show index)

{- |
  Type of object a non-terminal is parsed into.
-}
typeOf :: Cache -> NonTerminal -> Text
typeOf cache entity = "(" <> cache.types ! entity <> ")"

{- |
  Name for GOTO[-, NonTerm]-function.

  > gotoEForX

  We have separate GOTO-functions for each non-terminal being parsed
  /and/ each target non-terminal.

  The name means "handle E while eventually parsing an X".
-}
gotoFunctionName :: Target -> NonTerminal -> Text
gotoFunctionName target entity = "goto" <> entity <> "For" <> target.entity

{- |
  Name for ACTION-function for a targer terminal.

  Call of that function will possibly, eventually produce an object of target type.
-}
actionFunctionName :: Target -> Text
actionFunctionName target = "actionFor" <> target.entity

{- |
  Name fot the entrypoint function to parse the target.
-}
parseFunctionName :: Target -> Text
parseFunctionName target = "parse" <> target.entity

{- |
  Generates parser module at given location.

  >>> produceParser "grammar.txt" "src" ["My", "Parser"]

  > $ echo src/My/Parser.hs
  > module My.Parser (
  > ...
-}
produceParser
  :: FilePath  -- ^ full path to grammar file
  -> FilePath  -- ^ path to @hs-source-dir@
  -> [String]  -- ^ module name
  -> IO ()
produceParser grammarFile src moduleName = do
  rawGrammar <- parseGrammar grammarFile >>= dieOnLexerError >>= dieOnParserError
  case check rawGrammar of
    Left errs -> do
      for_ errs (print . pPrint)
      exitFailure

    Right scopedGrammar -> do
      let pathToParser = src </> intercalate "/" moduleName <> ".hs"
      Text.writeFile pathToParser (generateParserModule scopedGrammar moduleName)

{- |
  Renders given grammar into @Text@.
-}
generateParserModule
  :: Grammar
  -> [String] -- ^ module name
  -> Text
generateParserModule grammar moduleName = Text.unlines do
  mconcat
    [ modulePrefix
    , [""]
    , requiredImports
    , grammar.imports
    , [""]
    , stackTypeDecl
    , generateParserFor grammar <$> toList targets
    ]
  where
    targets :: [Target]
    targets = foldMap (pure . Target) grammar.targets

    modulePrefix :: [Text]
    modulePrefix =
      ( "{-# LANGUAGE OverloadedLists #-}"
      : "{-# OPTIONS_GHC -Wno-unused-matches #-}"
      : "{-# OPTIONS_GHC -Wno-unused-top-binds #-}"
      : "module " <> Text.intercalate "." (map Text.pack moduleName) <> " ("
      : "  " <> Text.intercalate ", " do map parseFunctionName do toList targets
      : [") where"]
      )

    requiredImports :: [Text]
    requiredImports =
      [ "import Data.Text.Position (Pos, startPos)"
      , "import Data.Lexeme"
      , "import Text.Lexer.Default"
      , "import Data.Text.IO as Text"
      ]

    stackTypeDecl :: [Text]
    stackTypeDecl =
      [ "type Stack st xs ="
      , "  ( st xs"
      , "  , Pos"
      , "  , Split st xs"
      , "  )"
      , ""
      , "data Split st xs where"
      , "  Nil  ::                     Split st '[]"
      , "  (:>) :: x -> Stack st xs -> Split st (x : xs)"
      , ""
      ]

{- |
  Generate parsing block for single target entity.

  Gives state type, GOTO-functions, ACTION-function and parse-function
  specific for that entity.
-}
generateParserFor :: Grammar -> Target -> Text
generateParserFor grammar target = do
  let
    startingRule = Rule
      { entity  = S
      , symbols = [Nothing :@ NonTerm target.entity]
      , reduce  = ""
      }

    grammar' = (grammar :: Grammar)
      { rules = Set.insert startingRule grammar.rules
      }

    cachedGrammar = grammarToCachedGrammar grammar'
    starting = startingKernel cachedGrammar
    (startingState, cache) = buildTable cachedGrammar starting

  Text.unlines do
    mconcat
      [ [stateTypeDecl cache target]
      , gotoTableFor cache cachedGrammar target
      , [actionTableFor cache target]
      , [parseFunction cache target startingState cachedGrammar.terminals]
      ]

{- |
  Generate parse-function for this target entity.
-}
parseFunction
  :: Cache         -- ^ compiled and compressed automata
  -> Target
  -> Int           -- ^ ID of starting state (it is always 0, though)
  -> Set Terminal  -- ^ keywords for the lexer
  -> Text
parseFunction cache target startingState terminals = Text.unlines
  [ parseFunctionName target
      <> " :: FilePath -> IO (Either LexerError (Either (Pos, [String]) "
      <> typeOf cache target.entity
      <> "))"
  , parseFunctionName target <> " filepath = do"
  , "  text <- Text.readFile filepath"
  , "  case lexText filepath text ["
          <> Text.intercalate ", " (map terminalToStringLiteral (toList terminals))
          <> "] of"
  , "    Left  err   -> pure (Left err)"
  , "    Right (input, end) -> "
  , "      pure (Right (" <> actionFunctionName target
                  <> " (" <> s target startingState <> ", startPos filepath text, Nil) input end))"
  ]

{- |
  Generates all GOTO-function for given parsing target.
-}
gotoTableFor :: Cache -> CachedGrammar -> Target -> [Text]
gotoTableFor cache grammar target =
  grammar.rules & Monoidal.keysSet & foldMap \case
    S -> []
    Named entity -> [gotoEntity cache target entity]

{- |
  Generates single GOTO[-, NonTerm]-function for given parsing target
  and fixed entity being locally parsed.
-}
gotoEntity :: Cache -> Target -> NonTerminal -> Text
gotoEntity cache target entity = Text.unlines
  ( mconcat
      [ gotoFunctionName target entity
      , " :: " <> stackType target
      , " -> " <> typeOf cache entity
      , " -> [Lexeme]"
      , " -> Pos"
      , " -> Either (Pos, [String]) " <> typeOf cache target.entity
      ]
  : gotoFunctionName target entity <> " stack@(state, pos, _) parsed = case state of"
  : statesToGotoLines
  ++ ["  _ -> error \"\""]
  )
  where
    statesToGotoLines :: [Text]
    statesToGotoLines  =
      cache.table & Map.foldMapWithKey \source TransitionsTo {gotos} -> do
        gotos & Monoidal.foldMapWithKey \nonTerm destination -> do
          if nonTerm /= entity then mempty else do
            ["  " <> s target source <> " -> "
              <> actionFunctionName target <> "(" <> s target destination <> ", pos, (parsed :> stack))"]

{- |
  Generate ACTION-function for given target.
-}
actionTableFor :: Cache -> Target -> Text
actionTableFor cache target = Text.unlines
  ( mconcat
      [ actionFunctionName target
      , " :: " <> stackType target
      , " -> [Lexeme]"
      , " -> Pos"
      , " -> Either (Pos, [String]) " <> typeOf cache target.entity
      ]
  : actionFunctionName target <> " = \\cases"
  : actionClauses
  )
  where
    actionClauses :: [Text]
    actionClauses =
      cache.table & Map.foldMapWithKey \source TransitionsTo {actions} -> do
        mconcat
          [ actions & Monoidal.foldMapWithKey \lookahead decisions -> do
              ("  " <>) <$> do
                mconcat
                  [ decisions & foldMap \case
                      Accept -> do
                        ["___@(" <> s target source <> ", _, e :> _) [] -> \\_ -> Right e"]
                      Reduce rule ->
                        [reduceBinders target source lookahead rule <> " -> " <> callToGoto target rule]
                      Shift destination -> do
                        case lookahead of
                          LookForEOF -> error ""
                          LookForTerm term -> do
                            ["stk@(" <> s target source <> ", _, _) ((pos, " <> binder "n" term <> ") : input) -> "
                                <> actionFunctionName target <> " (" <> s target destination <> ",  pos, " <> bound "n" term <> " :> stk) input"]
                  ]
          , ["  ___@(" <> s target source <> ", _, _) input -> \\end -> Left (currentPos input end, " <> expected source <> ")"]
          ]

    expected :: Int -> Text
    expected source = mconcat
      [ "["
      , (cache.table ! source).actions & Monoidal.keys & map lookaheadToStringLiteral & Text.intercalate ", "
      , "]"
      ]

    lookaheadToStringLiteral :: Lookahead -> Text
    lookaheadToStringLiteral = \case
      LookForEOF -> "\"EOF\""
      LookForTerm term -> terminalToStringLiteral term

terminalToStringLiteral :: Terminal -> Text
terminalToStringLiteral term = Text.pack (show term)

callToGoto :: Target -> Rule -> Text
callToGoto target rule = case rule.entity of
  S -> error ""
  Named nonTerm -> gotoFunctionName target nonTerm <> " stk (" <> rule.reduce <> ") input"

reduceBinders :: Target -> Int -> Lookahead -> Rule -> Text
reduceBinders target source lookahead rule =
    stackBinder <> " " <> inputBinder
  where
    symbol  ::  NamedSymbol
    symbols :: [NamedSymbol]
    symbol :| symbols = NonEmpty.reverse rule.symbols

    stackBinder :: Text
    stackBinder = "___@(" <> s target source <> ", _, " <> symbolBinder symbol <> " :> " <> otherSymbolBinders <> ")"

    otherSymbolBinders :: Text
    otherSymbolBinders = foldr addBinder "stk@(_, pos, _)" symbols
      where
        addBinder :: NamedSymbol -> Text -> Text
        addBinder sym rest = "(_, _, " <> symbolBinder sym <> " :> " <> rest <> ")"

    inputBinder :: Text
    inputBinder = "input@" <> case lookahead of
      LookForEOF -> "[]"
      LookForTerm term -> "((_, " <> binder "_" term <> ") : _)"

termType :: Terminal -> Text
termType = \case
  "<number>" -> "Integer"
  "<string>" -> "Text"
  "<Name>"   -> "Text"
  "<name>"   -> "Text"
  _          -> "()"

binder :: Text -> Terminal -> Text
binder name = \case
  "<number>" -> "NumberLiteral " <> name
  "<string>" -> "StringLiteral " <> name
  "<name>"   -> "LowercaseName " <> name
  "<Name>"   -> "UppercaseName " <> name
  term       -> Text.pack (show term)

bound :: Text -> Terminal -> Text
bound name = \case
  "<number>" -> name
  "<string>" -> name
  "<name>"   -> name
  "<Name>"   -> name
  _          -> "()"

symbolBinder :: NamedSymbol -> Text
symbolBinder (name :@ _) = fromMaybe "_" name
