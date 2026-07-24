{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Frontend.Parser (
  parseGrammar
) where

import Data.Text.Position (Pos, startPos)
import Data.Lexeme
import Text.Lexer.Default
import Data.Text.IO as Text
import RawGrammar
import Symbol
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty

type Stack st xs =
  ( st xs
  , Pos
  , Split st xs
  )

data Split st xs where
  Nil  ::                     Split st '[]
  (:>) :: x -> Stack st xs -> Split st (x : xs)

data StGrammar xs where
  StGrammar_0 :: StGrammar (a)
  StGrammar_1 :: StGrammar ([Text] : a)
  StGrammar_2 :: StGrammar (NonEmpty (Pos, NonTerminal) : [Text] : a)
  StGrammar_3 :: StGrammar (RawRule : a)
  StGrammar_4 :: StGrammar (RawSep : a)
  StGrammar_5 :: StGrammar (RawStmt : a)
  StGrammar_6 :: StGrammar (NonEmpty RawStmt : RawStmt : a)
  StGrammar_7 :: StGrammar (Text : a)
  StGrammar_8 :: StGrammar (() : Text : a)
  StGrammar_9 :: StGrammar (Text : () : Text : a)
  StGrammar_10 :: StGrammar (() : Text : () : Text : a)
  StGrammar_11 :: StGrammar (RawClause : a)
  StGrammar_12 :: StGrammar (() : RawClause : a)
  StGrammar_13 :: StGrammar (NonEmpty RawClause : () : RawClause : a)
  StGrammar_14 :: StGrammar ((Pos, NamedSymbol) : a)
  StGrammar_15 :: StGrammar (NonEmpty (Pos, NamedSymbol) : (Pos, NamedSymbol) : a)
  StGrammar_16 :: StGrammar (Symbol : a)
  StGrammar_17 :: StGrammar (Text : a)
  StGrammar_18 :: StGrammar (() : Text : a)
  StGrammar_19 :: StGrammar (Symbol : () : Text : a)
  StGrammar_20 :: StGrammar (Symbol : () : Text : a)
  StGrammar_21 :: StGrammar (Text : a)
  StGrammar_22 :: StGrammar (Text : a)
  StGrammar_23 :: StGrammar (NonEmpty (Pos, NamedSymbol) : a)
  StGrammar_24 :: StGrammar (() : NonEmpty (Pos, NamedSymbol) : a)
  StGrammar_25 :: StGrammar (Text : () : NonEmpty (Pos, NamedSymbol) : a)
  StGrammar_26 :: StGrammar (NonEmpty RawClause : () : Text : () : Text : a)
  StGrammar_27 :: StGrammar ((Pos, Symbol) : () : Text : () : Text : a)
  StGrammar_28 :: StGrammar (() : (Pos, Symbol) : () : Text : () : Text : a)
  StGrammar_29 :: StGrammar (() : () : (Pos, Symbol) : () : Text : () : Text : a)
  StGrammar_30 :: StGrammar (Text : () : () : (Pos, Symbol) : () : Text : () : Text : a)
  StGrammar_31 :: StGrammar (Text : a)
  StGrammar_32 :: StGrammar (Text : a)
  StGrammar_33 :: StGrammar (() : () : Text : () : Text : a)
  StGrammar_34 :: StGrammar ((Pos, Symbol) : () : () : Text : () : Text : a)
  StGrammar_35 :: StGrammar (Text : a)
  StGrammar_36 :: StGrammar (() : Text : a)
  StGrammar_37 :: StGrammar (NonEmpty RawClause : () : Text : a)
  StGrammar_38 :: StGrammar (NonEmpty RawStmt : NonEmpty (Pos, NonTerminal) : [Text] : a)
  StGrammar_39 :: StGrammar (() : a)
  StGrammar_40 :: StGrammar (NonEmpty (Pos, NonTerminal) : () : a)
  StGrammar_41 :: StGrammar ((Pos, NonTerminal) : a)
  StGrammar_42 :: StGrammar (() : (Pos, NonTerminal) : a)
  StGrammar_43 :: StGrammar (NonEmpty (Pos, NonTerminal) : () : (Pos, NonTerminal) : a)
  StGrammar_44 :: StGrammar (Text : a)
  StGrammar_45 :: StGrammar (RawGrammar : a)
  StGrammar_46 :: StGrammar (() : a)
  StGrammar_47 :: StGrammar (NonEmpty Text : () : a)
  StGrammar_48 :: StGrammar (Text : a)
  StGrammar_49 :: StGrammar (NonEmpty Text : Text : a)

gotoAdditionsForGrammar :: Stack StGrammar xs -> ([Text]) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoAdditionsForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_0 -> actionForGrammar(StGrammar_1, pos, (parsed :> stack))
  _ -> error ""

gotoClauseForGrammar :: Stack StGrammar xs -> (RawClause) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoClauseForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_10 -> actionForGrammar(StGrammar_11, pos, (parsed :> stack))
  StGrammar_12 -> actionForGrammar(StGrammar_11, pos, (parsed :> stack))
  StGrammar_36 -> actionForGrammar(StGrammar_11, pos, (parsed :> stack))
  _ -> error ""

gotoClausesForGrammar :: Stack StGrammar xs -> (NonEmpty RawClause) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoClausesForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_10 -> actionForGrammar(StGrammar_26, pos, (parsed :> stack))
  StGrammar_12 -> actionForGrammar(StGrammar_13, pos, (parsed :> stack))
  StGrammar_36 -> actionForGrammar(StGrammar_37, pos, (parsed :> stack))
  _ -> error ""

gotoGrammarForGrammar :: Stack StGrammar xs -> (RawGrammar) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoGrammarForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_0 -> actionForGrammar(StGrammar_45, pos, (parsed :> stack))
  _ -> error ""

gotoLinesForGrammar :: Stack StGrammar xs -> (NonEmpty Text) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoLinesForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_46 -> actionForGrammar(StGrammar_47, pos, (parsed :> stack))
  StGrammar_48 -> actionForGrammar(StGrammar_49, pos, (parsed :> stack))
  _ -> error ""

gotoNonTerminalForGrammar :: Stack StGrammar xs -> (Symbol) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoNonTerminalForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_18 -> actionForGrammar(StGrammar_19, pos, (parsed :> stack))
  _ -> error ""

gotoNonTerminalsForGrammar :: Stack StGrammar xs -> (NonEmpty (Pos, NonTerminal)) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoNonTerminalsForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_39 -> actionForGrammar(StGrammar_40, pos, (parsed :> stack))
  StGrammar_42 -> actionForGrammar(StGrammar_43, pos, (parsed :> stack))
  _ -> error ""

gotoRefForGrammar :: Stack StGrammar xs -> ((Pos, Symbol)) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoRefForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_10 -> actionForGrammar(StGrammar_27, pos, (parsed :> stack))
  StGrammar_33 -> actionForGrammar(StGrammar_34, pos, (parsed :> stack))
  _ -> error ""

gotoRuleForGrammar :: Stack StGrammar xs -> (RawRule) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoRuleForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_2 -> actionForGrammar(StGrammar_3, pos, (parsed :> stack))
  StGrammar_5 -> actionForGrammar(StGrammar_3, pos, (parsed :> stack))
  _ -> error ""

gotoSepForGrammar :: Stack StGrammar xs -> (RawSep) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoSepForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_2 -> actionForGrammar(StGrammar_4, pos, (parsed :> stack))
  StGrammar_5 -> actionForGrammar(StGrammar_4, pos, (parsed :> stack))
  _ -> error ""

gotoStarterForGrammar :: Stack StGrammar xs -> ((Pos, NonTerminal)) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoStarterForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_39 -> actionForGrammar(StGrammar_41, pos, (parsed :> stack))
  StGrammar_42 -> actionForGrammar(StGrammar_41, pos, (parsed :> stack))
  _ -> error ""

gotoStartersForGrammar :: Stack StGrammar xs -> (NonEmpty (Pos, NonTerminal)) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoStartersForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_1 -> actionForGrammar(StGrammar_2, pos, (parsed :> stack))
  _ -> error ""

gotoStmtForGrammar :: Stack StGrammar xs -> (RawStmt) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoStmtForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_2 -> actionForGrammar(StGrammar_5, pos, (parsed :> stack))
  StGrammar_5 -> actionForGrammar(StGrammar_5, pos, (parsed :> stack))
  _ -> error ""

gotoStmtsForGrammar :: Stack StGrammar xs -> (NonEmpty RawStmt) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoStmtsForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_2 -> actionForGrammar(StGrammar_38, pos, (parsed :> stack))
  StGrammar_5 -> actionForGrammar(StGrammar_6, pos, (parsed :> stack))
  _ -> error ""

gotoSymbolForGrammar :: Stack StGrammar xs -> ((Pos, NamedSymbol)) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoSymbolForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_10 -> actionForGrammar(StGrammar_14, pos, (parsed :> stack))
  StGrammar_12 -> actionForGrammar(StGrammar_14, pos, (parsed :> stack))
  StGrammar_14 -> actionForGrammar(StGrammar_14, pos, (parsed :> stack))
  StGrammar_36 -> actionForGrammar(StGrammar_14, pos, (parsed :> stack))
  _ -> error ""

gotoSymbolsForGrammar :: Stack StGrammar xs -> (NonEmpty (Pos, NamedSymbol)) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoSymbolsForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_10 -> actionForGrammar(StGrammar_23, pos, (parsed :> stack))
  StGrammar_12 -> actionForGrammar(StGrammar_23, pos, (parsed :> stack))
  StGrammar_14 -> actionForGrammar(StGrammar_15, pos, (parsed :> stack))
  StGrammar_36 -> actionForGrammar(StGrammar_23, pos, (parsed :> stack))
  _ -> error ""

gotoTerminalForGrammar :: Stack StGrammar xs -> (Symbol) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoTerminalForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_10 -> actionForGrammar(StGrammar_16, pos, (parsed :> stack))
  StGrammar_12 -> actionForGrammar(StGrammar_16, pos, (parsed :> stack))
  StGrammar_14 -> actionForGrammar(StGrammar_16, pos, (parsed :> stack))
  StGrammar_18 -> actionForGrammar(StGrammar_20, pos, (parsed :> stack))
  StGrammar_36 -> actionForGrammar(StGrammar_16, pos, (parsed :> stack))
  _ -> error ""

actionForGrammar :: Stack StGrammar xs -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
actionForGrammar = \cases
  stk@(StGrammar_0, _, _) ((pos, "imports") : input) -> actionForGrammar (StGrammar_46,  pos, () :> stk) input
  ___@(StGrammar_0, _, _) input -> \end -> Left (currentPos input end, ["imports"])
  stk@(StGrammar_1, _, _) ((pos, "start") : input) -> actionForGrammar (StGrammar_39,  pos, () :> stk) input
  ___@(StGrammar_1, _, _) input -> \end -> Left (currentPos input end, ["start"])
  stk@(StGrammar_2, _, _) ((pos, UppercaseName n) : input) -> actionForGrammar (StGrammar_7,  pos, n :> stk) input
  ___@(StGrammar_2, _, _) input -> \end -> Left (currentPos input end, ["<Name>"])
  ___@(StGrammar_3, _, ru :> stk@(_, pos, _)) input@[] -> gotoStmtForGrammar stk (StmtRule ru) input
  ___@(StGrammar_3, _, ru :> stk@(_, pos, _)) input@((_, UppercaseName _) : _) -> gotoStmtForGrammar stk (StmtRule ru) input
  ___@(StGrammar_3, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<Name>"])
  ___@(StGrammar_4, _, se :> stk@(_, pos, _)) input@[] -> gotoStmtForGrammar stk (StmtSep se) input
  ___@(StGrammar_4, _, se :> stk@(_, pos, _)) input@((_, UppercaseName _) : _) -> gotoStmtForGrammar stk (StmtSep se) input
  ___@(StGrammar_4, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<Name>"])
  ___@(StGrammar_5, _, x :> stk@(_, pos, _)) input@[] -> gotoStmtsForGrammar stk ([x]) input
  stk@(StGrammar_5, _, _) ((pos, UppercaseName n) : input) -> actionForGrammar (StGrammar_7,  pos, n :> stk) input
  ___@(StGrammar_5, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<Name>"])
  ___@(StGrammar_6, _, xs :> (_, _, x :> stk@(_, pos, _))) input@[] -> gotoStmtsForGrammar stk (NonEmpty.cons x xs) input
  ___@(StGrammar_6, _, _) input -> \end -> Left (currentPos input end, ["EOF"])
  stk@(StGrammar_7, _, _) ((pos, ":") : input) -> actionForGrammar (StGrammar_8,  pos, () :> stk) input
  stk@(StGrammar_7, _, _) ((pos, "=") : input) -> actionForGrammar (StGrammar_36,  pos, () :> stk) input
  ___@(StGrammar_7, _, _) input -> \end -> Left (currentPos input end, [":", "="])
  stk@(StGrammar_8, _, _) ((pos, StringLiteral n) : input) -> actionForGrammar (StGrammar_9,  pos, n :> stk) input
  ___@(StGrammar_8, _, _) input -> \end -> Left (currentPos input end, ["<string>"])
  stk@(StGrammar_9, _, _) ((pos, "=") : input) -> actionForGrammar (StGrammar_10,  pos, () :> stk) input
  ___@(StGrammar_9, _, _) input -> \end -> Left (currentPos input end, ["="])
  stk@(StGrammar_10, _, _) ((pos, UppercaseName n) : input) -> actionForGrammar (StGrammar_31,  pos, n :> stk) input
  stk@(StGrammar_10, _, _) ((pos, LowercaseName n) : input) -> actionForGrammar (StGrammar_17,  pos, n :> stk) input
  stk@(StGrammar_10, _, _) ((pos, StringLiteral n) : input) -> actionForGrammar (StGrammar_32,  pos, n :> stk) input
  stk@(StGrammar_10, _, _) ((pos, "some") : input) -> actionForGrammar (StGrammar_33,  pos, () :> stk) input
  ___@(StGrammar_10, _, _) input -> \end -> Left (currentPos input end, ["<Name>", "<name>", "<string>", "some"])
  ___@(StGrammar_11, _, x :> stk@(_, pos, _)) input@[] -> gotoClausesForGrammar stk ([x]) input
  ___@(StGrammar_11, _, x :> stk@(_, pos, _)) input@((_, UppercaseName _) : _) -> gotoClausesForGrammar stk ([x]) input
  stk@(StGrammar_11, _, _) ((pos, "|") : input) -> actionForGrammar (StGrammar_12,  pos, () :> stk) input
  ___@(StGrammar_11, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<Name>", "|"])
  stk@(StGrammar_12, _, _) ((pos, LowercaseName n) : input) -> actionForGrammar (StGrammar_17,  pos, n :> stk) input
  stk@(StGrammar_12, _, _) ((pos, StringLiteral n) : input) -> actionForGrammar (StGrammar_22,  pos, n :> stk) input
  ___@(StGrammar_12, _, _) input -> \end -> Left (currentPos input end, ["<name>", "<string>"])
  ___@(StGrammar_13, _, xs :> (_, _, _ :> (_, _, x :> stk@(_, pos, _)))) input@[] -> gotoClausesForGrammar stk (NonEmpty.cons x xs) input
  ___@(StGrammar_13, _, xs :> (_, _, _ :> (_, _, x :> stk@(_, pos, _)))) input@((_, UppercaseName _) : _) -> gotoClausesForGrammar stk (NonEmpty.cons x xs) input
  ___@(StGrammar_13, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<Name>"])
  stk@(StGrammar_14, _, _) ((pos, LowercaseName n) : input) -> actionForGrammar (StGrammar_17,  pos, n :> stk) input
  stk@(StGrammar_14, _, _) ((pos, StringLiteral n) : input) -> actionForGrammar (StGrammar_22,  pos, n :> stk) input
  ___@(StGrammar_14, _, x :> stk@(_, pos, _)) input@((_, "=>") : _) -> gotoSymbolsForGrammar stk ([x]) input
  ___@(StGrammar_14, _, _) input -> \end -> Left (currentPos input end, ["<name>", "<string>", "=>"])
  ___@(StGrammar_15, _, xs :> (_, _, x :> stk@(_, pos, _))) input@((_, "=>") : _) -> gotoSymbolsForGrammar stk (NonEmpty.cons x xs) input
  ___@(StGrammar_15, _, _) input -> \end -> Left (currentPos input end, ["=>"])
  ___@(StGrammar_16, _, t :> stk@(_, pos, _)) input@((_, LowercaseName _) : _) -> gotoSymbolForGrammar stk ((pos, Nothing :@ t)) input
  ___@(StGrammar_16, _, t :> stk@(_, pos, _)) input@((_, StringLiteral _) : _) -> gotoSymbolForGrammar stk ((pos, Nothing :@ t)) input
  ___@(StGrammar_16, _, t :> stk@(_, pos, _)) input@((_, "=>") : _) -> gotoSymbolForGrammar stk ((pos, Nothing :@ t)) input
  ___@(StGrammar_16, _, _) input -> \end -> Left (currentPos input end, ["<name>", "<string>", "=>"])
  stk@(StGrammar_17, _, _) ((pos, ":") : input) -> actionForGrammar (StGrammar_18,  pos, () :> stk) input
  ___@(StGrammar_17, _, _) input -> \end -> Left (currentPos input end, [":"])
  stk@(StGrammar_18, _, _) ((pos, UppercaseName n) : input) -> actionForGrammar (StGrammar_21,  pos, n :> stk) input
  stk@(StGrammar_18, _, _) ((pos, StringLiteral n) : input) -> actionForGrammar (StGrammar_22,  pos, n :> stk) input
  ___@(StGrammar_18, _, _) input -> \end -> Left (currentPos input end, ["<Name>", "<string>"])
  ___@(StGrammar_19, _, e :> (_, _, _ :> (_, _, n :> stk@(_, pos, _)))) input@((_, LowercaseName _) : _) -> gotoSymbolForGrammar stk ((pos, Just n  :@ e)) input
  ___@(StGrammar_19, _, e :> (_, _, _ :> (_, _, n :> stk@(_, pos, _)))) input@((_, StringLiteral _) : _) -> gotoSymbolForGrammar stk ((pos, Just n  :@ e)) input
  ___@(StGrammar_19, _, e :> (_, _, _ :> (_, _, n :> stk@(_, pos, _)))) input@((_, "=>") : _) -> gotoSymbolForGrammar stk ((pos, Just n  :@ e)) input
  ___@(StGrammar_19, _, _) input -> \end -> Left (currentPos input end, ["<name>", "<string>", "=>"])
  ___@(StGrammar_20, _, t :> (_, _, _ :> (_, _, n :> stk@(_, pos, _)))) input@((_, LowercaseName _) : _) -> gotoSymbolForGrammar stk ((pos, Just n  :@ t)) input
  ___@(StGrammar_20, _, t :> (_, _, _ :> (_, _, n :> stk@(_, pos, _)))) input@((_, StringLiteral _) : _) -> gotoSymbolForGrammar stk ((pos, Just n  :@ t)) input
  ___@(StGrammar_20, _, t :> (_, _, _ :> (_, _, n :> stk@(_, pos, _)))) input@((_, "=>") : _) -> gotoSymbolForGrammar stk ((pos, Just n  :@ t)) input
  ___@(StGrammar_20, _, _) input -> \end -> Left (currentPos input end, ["<name>", "<string>", "=>"])
  ___@(StGrammar_21, _, e :> stk@(_, pos, _)) input@((_, LowercaseName _) : _) -> gotoNonTerminalForGrammar stk (NonTerm e) input
  ___@(StGrammar_21, _, e :> stk@(_, pos, _)) input@((_, StringLiteral _) : _) -> gotoNonTerminalForGrammar stk (NonTerm e) input
  ___@(StGrammar_21, _, e :> stk@(_, pos, _)) input@((_, "=>") : _) -> gotoNonTerminalForGrammar stk (NonTerm e) input
  ___@(StGrammar_21, _, _) input -> \end -> Left (currentPos input end, ["<name>", "<string>", "=>"])
  ___@(StGrammar_22, _, t :> stk@(_, pos, _)) input@((_, LowercaseName _) : _) -> gotoTerminalForGrammar stk (Term t) input
  ___@(StGrammar_22, _, t :> stk@(_, pos, _)) input@((_, StringLiteral _) : _) -> gotoTerminalForGrammar stk (Term t) input
  ___@(StGrammar_22, _, t :> stk@(_, pos, _)) input@((_, "=>") : _) -> gotoTerminalForGrammar stk (Term t) input
  ___@(StGrammar_22, _, _) input -> \end -> Left (currentPos input end, ["<name>", "<string>", "=>"])
  stk@(StGrammar_23, _, _) ((pos, "=>") : input) -> actionForGrammar (StGrammar_24,  pos, () :> stk) input
  ___@(StGrammar_23, _, _) input -> \end -> Left (currentPos input end, ["=>"])
  stk@(StGrammar_24, _, _) ((pos, StringLiteral n) : input) -> actionForGrammar (StGrammar_25,  pos, n :> stk) input
  ___@(StGrammar_24, _, _) input -> \end -> Left (currentPos input end, ["<string>"])
  ___@(StGrammar_25, _, reduce :> (_, _, _ :> (_, _, symbols :> stk@(_, pos, _)))) input@[] -> gotoClauseForGrammar stk (RawClause {pos, symbols, reduce}) input
  ___@(StGrammar_25, _, reduce :> (_, _, _ :> (_, _, symbols :> stk@(_, pos, _)))) input@((_, UppercaseName _) : _) -> gotoClauseForGrammar stk (RawClause {pos, symbols, reduce}) input
  ___@(StGrammar_25, _, reduce :> (_, _, _ :> (_, _, symbols :> stk@(_, pos, _)))) input@((_, "|") : _) -> gotoClauseForGrammar stk (RawClause {pos, symbols, reduce}) input
  ___@(StGrammar_25, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<Name>", "|"])
  ___@(StGrammar_26, _, clauses :> (_, _, _ :> (_, _, type_ :> (_, _, _ :> (_, _, entity :> stk@(_, pos, _)))))) input@[] -> gotoRuleForGrammar stk (RawRule {pos, entity, type_,          clauses}) input
  ___@(StGrammar_26, _, clauses :> (_, _, _ :> (_, _, type_ :> (_, _, _ :> (_, _, entity :> stk@(_, pos, _)))))) input@((_, UppercaseName _) : _) -> gotoRuleForGrammar stk (RawRule {pos, entity, type_,          clauses}) input
  ___@(StGrammar_26, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<Name>"])
  stk@(StGrammar_27, _, _) ((pos, "sep") : input) -> actionForGrammar (StGrammar_28,  pos, () :> stk) input
  ___@(StGrammar_27, _, _) input -> \end -> Left (currentPos input end, ["sep"])
  stk@(StGrammar_28, _, _) ((pos, "by") : input) -> actionForGrammar (StGrammar_29,  pos, () :> stk) input
  ___@(StGrammar_28, _, _) input -> \end -> Left (currentPos input end, ["by"])
  stk@(StGrammar_29, _, _) ((pos, StringLiteral n) : input) -> actionForGrammar (StGrammar_30,  pos, n :> stk) input
  ___@(StGrammar_29, _, _) input -> \end -> Left (currentPos input end, ["<string>"])
  ___@(StGrammar_30, _, t :> (_, _, _ :> (_, _, _ :> (_, _, single :> (_, _, _ :> (_, _, type_ :> (_, _, _ :> (_, _, entity :> stk@(_, pos, _))))))))) input@[] -> gotoSepForGrammar stk (RawSep {pos, entity, type_, single, sep = Just t}) input
  ___@(StGrammar_30, _, t :> (_, _, _ :> (_, _, _ :> (_, _, single :> (_, _, _ :> (_, _, type_ :> (_, _, _ :> (_, _, entity :> stk@(_, pos, _))))))))) input@((_, UppercaseName _) : _) -> gotoSepForGrammar stk (RawSep {pos, entity, type_, single, sep = Just t}) input
  ___@(StGrammar_30, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<Name>"])
  ___@(StGrammar_31, _, nt :> stk@(_, pos, _)) input@[] -> gotoRefForGrammar stk ((pos, NonTerm nt)) input
  ___@(StGrammar_31, _, nt :> stk@(_, pos, _)) input@((_, UppercaseName _) : _) -> gotoRefForGrammar stk ((pos, NonTerm nt)) input
  ___@(StGrammar_31, _, nt :> stk@(_, pos, _)) input@((_, "sep") : _) -> gotoRefForGrammar stk ((pos, NonTerm nt)) input
  ___@(StGrammar_31, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<Name>", "sep"])
  ___@(StGrammar_32, _, t :> stk@(_, pos, _)) input@((_, LowercaseName _) : _) -> gotoTerminalForGrammar stk (Term t) input
  ___@(StGrammar_32, _, t :> stk@(_, pos, _)) input@((_, StringLiteral _) : _) -> gotoTerminalForGrammar stk (Term t) input
  ___@(StGrammar_32, _, t :> stk@(_, pos, _)) input@((_, "=>") : _) -> gotoTerminalForGrammar stk (Term t) input
  ___@(StGrammar_32, _, t :> stk@(_, pos, _)) input@((_, "sep") : _) -> gotoRefForGrammar stk ((pos, Term t)) input
  ___@(StGrammar_32, _, _) input -> \end -> Left (currentPos input end, ["<name>", "<string>", "=>", "sep"])
  stk@(StGrammar_33, _, _) ((pos, UppercaseName n) : input) -> actionForGrammar (StGrammar_31,  pos, n :> stk) input
  stk@(StGrammar_33, _, _) ((pos, StringLiteral n) : input) -> actionForGrammar (StGrammar_35,  pos, n :> stk) input
  ___@(StGrammar_33, _, _) input -> \end -> Left (currentPos input end, ["<Name>", "<string>"])
  ___@(StGrammar_34, _, single :> (_, _, _ :> (_, _, _ :> (_, _, type_ :> (_, _, _ :> (_, _, entity :> stk@(_, pos, _))))))) input@[] -> gotoSepForGrammar stk (RawSep {pos, entity, type_, single, sep = Nothing}) input
  ___@(StGrammar_34, _, single :> (_, _, _ :> (_, _, _ :> (_, _, type_ :> (_, _, _ :> (_, _, entity :> stk@(_, pos, _))))))) input@((_, UppercaseName _) : _) -> gotoSepForGrammar stk (RawSep {pos, entity, type_, single, sep = Nothing}) input
  ___@(StGrammar_34, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<Name>"])
  ___@(StGrammar_35, _, t :> stk@(_, pos, _)) input@[] -> gotoRefForGrammar stk ((pos, Term t)) input
  ___@(StGrammar_35, _, t :> stk@(_, pos, _)) input@((_, UppercaseName _) : _) -> gotoRefForGrammar stk ((pos, Term t)) input
  ___@(StGrammar_35, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<Name>"])
  stk@(StGrammar_36, _, _) ((pos, LowercaseName n) : input) -> actionForGrammar (StGrammar_17,  pos, n :> stk) input
  stk@(StGrammar_36, _, _) ((pos, StringLiteral n) : input) -> actionForGrammar (StGrammar_22,  pos, n :> stk) input
  ___@(StGrammar_36, _, _) input -> \end -> Left (currentPos input end, ["<name>", "<string>"])
  ___@(StGrammar_37, _, clauses :> (_, _, _ :> (_, _, entity :> stk@(_, pos, _)))) input@[] -> gotoRuleForGrammar stk (RawRule {pos, entity, type_ = entity, clauses}) input
  ___@(StGrammar_37, _, clauses :> (_, _, _ :> (_, _, entity :> stk@(_, pos, _)))) input@((_, UppercaseName _) : _) -> gotoRuleForGrammar stk (RawRule {pos, entity, type_ = entity, clauses}) input
  ___@(StGrammar_37, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<Name>"])
  ___@(StGrammar_38, _, rules :> (_, _, targets :> (_, _, adds :> stk@(_, pos, _)))) input@[] -> gotoGrammarForGrammar stk (RawGrammar {imports = adds, targets, rules}) input
  ___@(StGrammar_38, _, _) input -> \end -> Left (currentPos input end, ["EOF"])
  stk@(StGrammar_39, _, _) ((pos, UppercaseName n) : input) -> actionForGrammar (StGrammar_44,  pos, n :> stk) input
  ___@(StGrammar_39, _, _) input -> \end -> Left (currentPos input end, ["<Name>"])
  ___@(StGrammar_40, _, es :> (_, _, _ :> stk@(_, pos, _))) input@((_, UppercaseName _) : _) -> gotoStartersForGrammar stk (es) input
  ___@(StGrammar_40, _, _) input -> \end -> Left (currentPos input end, ["<Name>"])
  stk@(StGrammar_41, _, _) ((pos, ",") : input) -> actionForGrammar (StGrammar_42,  pos, () :> stk) input
  ___@(StGrammar_41, _, x :> stk@(_, pos, _)) input@((_, UppercaseName _) : _) -> gotoNonTerminalsForGrammar stk ([x]) input
  ___@(StGrammar_41, _, _) input -> \end -> Left (currentPos input end, [",", "<Name>"])
  stk@(StGrammar_42, _, _) ((pos, UppercaseName n) : input) -> actionForGrammar (StGrammar_44,  pos, n :> stk) input
  ___@(StGrammar_42, _, _) input -> \end -> Left (currentPos input end, ["<Name>"])
  ___@(StGrammar_43, _, xs :> (_, _, _ :> (_, _, x :> stk@(_, pos, _)))) input@((_, UppercaseName _) : _) -> gotoNonTerminalsForGrammar stk (NonEmpty.cons x xs) input
  ___@(StGrammar_43, _, _) input -> \end -> Left (currentPos input end, ["<Name>"])
  ___@(StGrammar_44, _, nt :> stk@(_, pos, _)) input@((_, ",") : _) -> gotoStarterForGrammar stk ((pos, nt)) input
  ___@(StGrammar_44, _, nt :> stk@(_, pos, _)) input@((_, UppercaseName _) : _) -> gotoStarterForGrammar stk ((pos, nt)) input
  ___@(StGrammar_44, _, _) input -> \end -> Left (currentPos input end, [",", "<Name>"])
  ___@(StGrammar_45, _, e :> _) [] -> \_ -> Right e
  ___@(StGrammar_45, _, _) input -> \end -> Left (currentPos input end, ["EOF"])
  stk@(StGrammar_46, _, _) ((pos, StringLiteral n) : input) -> actionForGrammar (StGrammar_48,  pos, n :> stk) input
  ___@(StGrammar_46, _, _ :> stk@(_, pos, _)) input@((_, "start") : _) -> gotoAdditionsForGrammar stk ([]) input
  ___@(StGrammar_46, _, _) input -> \end -> Left (currentPos input end, ["<string>", "start"])
  ___@(StGrammar_47, _, ls :> (_, _, _ :> stk@(_, pos, _))) input@((_, "start") : _) -> gotoAdditionsForGrammar stk (NonEmpty.toList ls) input
  ___@(StGrammar_47, _, _) input -> \end -> Left (currentPos input end, ["start"])
  stk@(StGrammar_48, _, _) ((pos, StringLiteral n) : input) -> actionForGrammar (StGrammar_48,  pos, n :> stk) input
  ___@(StGrammar_48, _, x :> stk@(_, pos, _)) input@((_, "start") : _) -> gotoLinesForGrammar stk ([x]) input
  ___@(StGrammar_48, _, _) input -> \end -> Left (currentPos input end, ["<string>", "start"])
  ___@(StGrammar_49, _, xs :> (_, _, x :> stk@(_, pos, _))) input@((_, "start") : _) -> gotoLinesForGrammar stk (NonEmpty.cons x xs) input
  ___@(StGrammar_49, _, _) input -> \end -> Left (currentPos input end, ["start"])

parseGrammar :: FilePath -> IO (Either LexerError (Either (Pos, [String]) (RawGrammar)))
parseGrammar filepath = do
  text <- Text.readFile filepath
  case lexText filepath text [",", ":", "<Name>", "<name>", "<string>", "=", "=>", "by", "imports", "sep", "some", "start", "|"] of
    Left  err   -> pure (Left err)
    Right (input, end) -> 
      pure (Right (actionForGrammar (StGrammar_0, startPos filepath text, Nil) input end))


