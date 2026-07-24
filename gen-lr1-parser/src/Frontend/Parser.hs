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
  StGrammar_4 :: StGrammar (NonEmpty RawRule : RawRule : a)
  StGrammar_5 :: StGrammar (Text : a)
  StGrammar_6 :: StGrammar (() : Text : a)
  StGrammar_7 :: StGrammar (Text : () : Text : a)
  StGrammar_8 :: StGrammar (() : Text : () : Text : a)
  StGrammar_9 :: StGrammar (RawClause : a)
  StGrammar_10 :: StGrammar (() : RawClause : a)
  StGrammar_11 :: StGrammar (NonEmpty RawClause : () : RawClause : a)
  StGrammar_12 :: StGrammar ((Pos, NamedSymbol) : a)
  StGrammar_13 :: StGrammar (NonEmpty (Pos, NamedSymbol) : (Pos, NamedSymbol) : a)
  StGrammar_14 :: StGrammar (Symbol : a)
  StGrammar_15 :: StGrammar (Text : a)
  StGrammar_16 :: StGrammar (() : Text : a)
  StGrammar_17 :: StGrammar (Symbol : () : Text : a)
  StGrammar_18 :: StGrammar (Symbol : () : Text : a)
  StGrammar_19 :: StGrammar (Text : a)
  StGrammar_20 :: StGrammar (Text : a)
  StGrammar_21 :: StGrammar (NonEmpty (Pos, NamedSymbol) : a)
  StGrammar_22 :: StGrammar (() : NonEmpty (Pos, NamedSymbol) : a)
  StGrammar_23 :: StGrammar (Text : () : NonEmpty (Pos, NamedSymbol) : a)
  StGrammar_24 :: StGrammar (NonEmpty RawClause : () : Text : () : Text : a)
  StGrammar_25 :: StGrammar (() : Text : a)
  StGrammar_26 :: StGrammar (NonEmpty RawClause : () : Text : a)
  StGrammar_27 :: StGrammar (NonEmpty RawRule : NonEmpty (Pos, NonTerminal) : [Text] : a)
  StGrammar_28 :: StGrammar (() : a)
  StGrammar_29 :: StGrammar (NonEmpty (Pos, NonTerminal) : () : a)
  StGrammar_30 :: StGrammar (Text : a)
  StGrammar_31 :: StGrammar (() : Text : a)
  StGrammar_32 :: StGrammar (NonEmpty (Pos, NonTerminal) : () : Text : a)
  StGrammar_33 :: StGrammar (RawGrammar : a)
  StGrammar_34 :: StGrammar (() : a)
  StGrammar_35 :: StGrammar ([Text] : () : a)
  StGrammar_36 :: StGrammar (Text : a)
  StGrammar_37 :: StGrammar ([Text] : Text : a)

gotoAdditionsForGrammar :: Stack StGrammar xs -> ([Text]) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoAdditionsForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_0 -> actionForGrammar(StGrammar_1, pos, (parsed :> stack))
  _ -> error ""

gotoClauseForGrammar :: Stack StGrammar xs -> (RawClause) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoClauseForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_8 -> actionForGrammar(StGrammar_9, pos, (parsed :> stack))
  StGrammar_10 -> actionForGrammar(StGrammar_9, pos, (parsed :> stack))
  StGrammar_25 -> actionForGrammar(StGrammar_9, pos, (parsed :> stack))
  _ -> error ""

gotoClausesForGrammar :: Stack StGrammar xs -> (NonEmpty RawClause) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoClausesForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_8 -> actionForGrammar(StGrammar_24, pos, (parsed :> stack))
  StGrammar_10 -> actionForGrammar(StGrammar_11, pos, (parsed :> stack))
  StGrammar_25 -> actionForGrammar(StGrammar_26, pos, (parsed :> stack))
  _ -> error ""

gotoGrammarForGrammar :: Stack StGrammar xs -> (RawGrammar) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoGrammarForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_0 -> actionForGrammar(StGrammar_33, pos, (parsed :> stack))
  _ -> error ""

gotoLinesForGrammar :: Stack StGrammar xs -> ([Text]) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoLinesForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_34 -> actionForGrammar(StGrammar_35, pos, (parsed :> stack))
  StGrammar_36 -> actionForGrammar(StGrammar_37, pos, (parsed :> stack))
  _ -> error ""

gotoNonTerminalForGrammar :: Stack StGrammar xs -> (Symbol) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoNonTerminalForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_16 -> actionForGrammar(StGrammar_17, pos, (parsed :> stack))
  _ -> error ""

gotoNonTerminalsForGrammar :: Stack StGrammar xs -> (NonEmpty (Pos, NonTerminal)) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoNonTerminalsForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_28 -> actionForGrammar(StGrammar_29, pos, (parsed :> stack))
  StGrammar_31 -> actionForGrammar(StGrammar_32, pos, (parsed :> stack))
  _ -> error ""

gotoRuleForGrammar :: Stack StGrammar xs -> (RawRule) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoRuleForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_2 -> actionForGrammar(StGrammar_3, pos, (parsed :> stack))
  StGrammar_3 -> actionForGrammar(StGrammar_3, pos, (parsed :> stack))
  _ -> error ""

gotoRulesForGrammar :: Stack StGrammar xs -> (NonEmpty RawRule) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoRulesForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_2 -> actionForGrammar(StGrammar_27, pos, (parsed :> stack))
  StGrammar_3 -> actionForGrammar(StGrammar_4, pos, (parsed :> stack))
  _ -> error ""

gotoStartersForGrammar :: Stack StGrammar xs -> (NonEmpty (Pos, NonTerminal)) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoStartersForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_1 -> actionForGrammar(StGrammar_2, pos, (parsed :> stack))
  _ -> error ""

gotoSymbolForGrammar :: Stack StGrammar xs -> ((Pos, NamedSymbol)) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoSymbolForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_8 -> actionForGrammar(StGrammar_12, pos, (parsed :> stack))
  StGrammar_10 -> actionForGrammar(StGrammar_12, pos, (parsed :> stack))
  StGrammar_12 -> actionForGrammar(StGrammar_12, pos, (parsed :> stack))
  StGrammar_25 -> actionForGrammar(StGrammar_12, pos, (parsed :> stack))
  _ -> error ""

gotoSymbolsForGrammar :: Stack StGrammar xs -> (NonEmpty (Pos, NamedSymbol)) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoSymbolsForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_8 -> actionForGrammar(StGrammar_21, pos, (parsed :> stack))
  StGrammar_10 -> actionForGrammar(StGrammar_21, pos, (parsed :> stack))
  StGrammar_12 -> actionForGrammar(StGrammar_13, pos, (parsed :> stack))
  StGrammar_25 -> actionForGrammar(StGrammar_21, pos, (parsed :> stack))
  _ -> error ""

gotoTerminalForGrammar :: Stack StGrammar xs -> (Symbol) -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
gotoTerminalForGrammar stack@(state, pos, _) parsed = case state of
  StGrammar_8 -> actionForGrammar(StGrammar_14, pos, (parsed :> stack))
  StGrammar_10 -> actionForGrammar(StGrammar_14, pos, (parsed :> stack))
  StGrammar_12 -> actionForGrammar(StGrammar_14, pos, (parsed :> stack))
  StGrammar_16 -> actionForGrammar(StGrammar_18, pos, (parsed :> stack))
  StGrammar_25 -> actionForGrammar(StGrammar_14, pos, (parsed :> stack))
  _ -> error ""

actionForGrammar :: Stack StGrammar xs -> [Lexeme] -> Pos -> Either (Pos, [String]) (RawGrammar)
actionForGrammar = \cases
  stk@(StGrammar_0, _, _) ((pos, "imports") : input) -> actionForGrammar (StGrammar_34,  pos, () :> stk) input
  ___@(StGrammar_0, _, _) input -> \end -> Left (currentPos input end, ["imports"])
  stk@(StGrammar_1, _, _) ((pos, "start") : input) -> actionForGrammar (StGrammar_28,  pos, () :> stk) input
  ___@(StGrammar_1, _, _) input -> \end -> Left (currentPos input end, ["start"])
  stk@(StGrammar_2, _, _) ((pos, UppercaseName n) : input) -> actionForGrammar (StGrammar_5,  pos, n :> stk) input
  ___@(StGrammar_2, _, _) input -> \end -> Left (currentPos input end, ["<Name>"])
  ___@(StGrammar_3, _, r :> stk@(_, pos, _)) input@[] -> gotoRulesForGrammar stk ([r]) input
  stk@(StGrammar_3, _, _) ((pos, UppercaseName n) : input) -> actionForGrammar (StGrammar_5,  pos, n :> stk) input
  ___@(StGrammar_3, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<Name>"])
  ___@(StGrammar_4, _, rs :> (_, _, r :> stk@(_, pos, _))) input@[] -> gotoRulesForGrammar stk (NonEmpty.cons r rs) input
  ___@(StGrammar_4, _, _) input -> \end -> Left (currentPos input end, ["EOF"])
  stk@(StGrammar_5, _, _) ((pos, ":") : input) -> actionForGrammar (StGrammar_6,  pos, () :> stk) input
  stk@(StGrammar_5, _, _) ((pos, "=") : input) -> actionForGrammar (StGrammar_25,  pos, () :> stk) input
  ___@(StGrammar_5, _, _) input -> \end -> Left (currentPos input end, [":", "="])
  stk@(StGrammar_6, _, _) ((pos, StringLiteral n) : input) -> actionForGrammar (StGrammar_7,  pos, n :> stk) input
  ___@(StGrammar_6, _, _) input -> \end -> Left (currentPos input end, ["<string>"])
  stk@(StGrammar_7, _, _) ((pos, "=") : input) -> actionForGrammar (StGrammar_8,  pos, () :> stk) input
  ___@(StGrammar_7, _, _) input -> \end -> Left (currentPos input end, ["="])
  stk@(StGrammar_8, _, _) ((pos, LowercaseName n) : input) -> actionForGrammar (StGrammar_15,  pos, n :> stk) input
  stk@(StGrammar_8, _, _) ((pos, StringLiteral n) : input) -> actionForGrammar (StGrammar_20,  pos, n :> stk) input
  ___@(StGrammar_8, _, _) input -> \end -> Left (currentPos input end, ["<name>", "<string>"])
  ___@(StGrammar_9, _, c :> stk@(_, pos, _)) input@[] -> gotoClausesForGrammar stk ([c]) input
  ___@(StGrammar_9, _, c :> stk@(_, pos, _)) input@((_, UppercaseName _) : _) -> gotoClausesForGrammar stk ([c]) input
  stk@(StGrammar_9, _, _) ((pos, "|") : input) -> actionForGrammar (StGrammar_10,  pos, () :> stk) input
  ___@(StGrammar_9, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<Name>", "|"])
  stk@(StGrammar_10, _, _) ((pos, LowercaseName n) : input) -> actionForGrammar (StGrammar_15,  pos, n :> stk) input
  stk@(StGrammar_10, _, _) ((pos, StringLiteral n) : input) -> actionForGrammar (StGrammar_20,  pos, n :> stk) input
  ___@(StGrammar_10, _, _) input -> \end -> Left (currentPos input end, ["<name>", "<string>"])
  ___@(StGrammar_11, _, cs :> (_, _, _ :> (_, _, c :> stk@(_, pos, _)))) input@[] -> gotoClausesForGrammar stk (NonEmpty.cons c cs) input
  ___@(StGrammar_11, _, cs :> (_, _, _ :> (_, _, c :> stk@(_, pos, _)))) input@((_, UppercaseName _) : _) -> gotoClausesForGrammar stk (NonEmpty.cons c cs) input
  ___@(StGrammar_11, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<Name>"])
  stk@(StGrammar_12, _, _) ((pos, LowercaseName n) : input) -> actionForGrammar (StGrammar_15,  pos, n :> stk) input
  stk@(StGrammar_12, _, _) ((pos, StringLiteral n) : input) -> actionForGrammar (StGrammar_20,  pos, n :> stk) input
  ___@(StGrammar_12, _, p :> stk@(_, pos, _)) input@((_, "=>") : _) -> gotoSymbolsForGrammar stk ([p]) input
  ___@(StGrammar_12, _, _) input -> \end -> Left (currentPos input end, ["<name>", "<string>", "=>"])
  ___@(StGrammar_13, _, ps :> (_, _, p :> stk@(_, pos, _))) input@((_, "=>") : _) -> gotoSymbolsForGrammar stk (NonEmpty.cons p ps) input
  ___@(StGrammar_13, _, _) input -> \end -> Left (currentPos input end, ["=>"])
  ___@(StGrammar_14, _, t :> stk@(_, pos, _)) input@((_, LowercaseName _) : _) -> gotoSymbolForGrammar stk ((pos, Nothing :@ t)) input
  ___@(StGrammar_14, _, t :> stk@(_, pos, _)) input@((_, StringLiteral _) : _) -> gotoSymbolForGrammar stk ((pos, Nothing :@ t)) input
  ___@(StGrammar_14, _, t :> stk@(_, pos, _)) input@((_, "=>") : _) -> gotoSymbolForGrammar stk ((pos, Nothing :@ t)) input
  ___@(StGrammar_14, _, _) input -> \end -> Left (currentPos input end, ["<name>", "<string>", "=>"])
  stk@(StGrammar_15, _, _) ((pos, ":") : input) -> actionForGrammar (StGrammar_16,  pos, () :> stk) input
  ___@(StGrammar_15, _, _) input -> \end -> Left (currentPos input end, [":"])
  stk@(StGrammar_16, _, _) ((pos, UppercaseName n) : input) -> actionForGrammar (StGrammar_19,  pos, n :> stk) input
  stk@(StGrammar_16, _, _) ((pos, StringLiteral n) : input) -> actionForGrammar (StGrammar_20,  pos, n :> stk) input
  ___@(StGrammar_16, _, _) input -> \end -> Left (currentPos input end, ["<Name>", "<string>"])
  ___@(StGrammar_17, _, e :> (_, _, _ :> (_, _, n :> stk@(_, pos, _)))) input@((_, LowercaseName _) : _) -> gotoSymbolForGrammar stk ((pos, Just n  :@ e)) input
  ___@(StGrammar_17, _, e :> (_, _, _ :> (_, _, n :> stk@(_, pos, _)))) input@((_, StringLiteral _) : _) -> gotoSymbolForGrammar stk ((pos, Just n  :@ e)) input
  ___@(StGrammar_17, _, e :> (_, _, _ :> (_, _, n :> stk@(_, pos, _)))) input@((_, "=>") : _) -> gotoSymbolForGrammar stk ((pos, Just n  :@ e)) input
  ___@(StGrammar_17, _, _) input -> \end -> Left (currentPos input end, ["<name>", "<string>", "=>"])
  ___@(StGrammar_18, _, t :> (_, _, _ :> (_, _, n :> stk@(_, pos, _)))) input@((_, LowercaseName _) : _) -> gotoSymbolForGrammar stk ((pos, Just n  :@ t)) input
  ___@(StGrammar_18, _, t :> (_, _, _ :> (_, _, n :> stk@(_, pos, _)))) input@((_, StringLiteral _) : _) -> gotoSymbolForGrammar stk ((pos, Just n  :@ t)) input
  ___@(StGrammar_18, _, t :> (_, _, _ :> (_, _, n :> stk@(_, pos, _)))) input@((_, "=>") : _) -> gotoSymbolForGrammar stk ((pos, Just n  :@ t)) input
  ___@(StGrammar_18, _, _) input -> \end -> Left (currentPos input end, ["<name>", "<string>", "=>"])
  ___@(StGrammar_19, _, e :> stk@(_, pos, _)) input@((_, LowercaseName _) : _) -> gotoNonTerminalForGrammar stk (NonTerm e) input
  ___@(StGrammar_19, _, e :> stk@(_, pos, _)) input@((_, StringLiteral _) : _) -> gotoNonTerminalForGrammar stk (NonTerm e) input
  ___@(StGrammar_19, _, e :> stk@(_, pos, _)) input@((_, "=>") : _) -> gotoNonTerminalForGrammar stk (NonTerm e) input
  ___@(StGrammar_19, _, _) input -> \end -> Left (currentPos input end, ["<name>", "<string>", "=>"])
  ___@(StGrammar_20, _, t :> stk@(_, pos, _)) input@((_, LowercaseName _) : _) -> gotoTerminalForGrammar stk (Term t) input
  ___@(StGrammar_20, _, t :> stk@(_, pos, _)) input@((_, StringLiteral _) : _) -> gotoTerminalForGrammar stk (Term t) input
  ___@(StGrammar_20, _, t :> stk@(_, pos, _)) input@((_, "=>") : _) -> gotoTerminalForGrammar stk (Term t) input
  ___@(StGrammar_20, _, _) input -> \end -> Left (currentPos input end, ["<name>", "<string>", "=>"])
  stk@(StGrammar_21, _, _) ((pos, "=>") : input) -> actionForGrammar (StGrammar_22,  pos, () :> stk) input
  ___@(StGrammar_21, _, _) input -> \end -> Left (currentPos input end, ["=>"])
  stk@(StGrammar_22, _, _) ((pos, StringLiteral n) : input) -> actionForGrammar (StGrammar_23,  pos, n :> stk) input
  ___@(StGrammar_22, _, _) input -> \end -> Left (currentPos input end, ["<string>"])
  ___@(StGrammar_23, _, reduce :> (_, _, _ :> (_, _, symbols :> stk@(_, pos, _)))) input@[] -> gotoClauseForGrammar stk (RawClause {pos, symbols, reduce}) input
  ___@(StGrammar_23, _, reduce :> (_, _, _ :> (_, _, symbols :> stk@(_, pos, _)))) input@((_, UppercaseName _) : _) -> gotoClauseForGrammar stk (RawClause {pos, symbols, reduce}) input
  ___@(StGrammar_23, _, reduce :> (_, _, _ :> (_, _, symbols :> stk@(_, pos, _)))) input@((_, "|") : _) -> gotoClauseForGrammar stk (RawClause {pos, symbols, reduce}) input
  ___@(StGrammar_23, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<Name>", "|"])
  ___@(StGrammar_24, _, clauses :> (_, _, _ :> (_, _, type_ :> (_, _, _ :> (_, _, entity :> stk@(_, pos, _)))))) input@[] -> gotoRuleForGrammar stk (RawRule {pos, entity, type_,          clauses}) input
  ___@(StGrammar_24, _, clauses :> (_, _, _ :> (_, _, type_ :> (_, _, _ :> (_, _, entity :> stk@(_, pos, _)))))) input@((_, UppercaseName _) : _) -> gotoRuleForGrammar stk (RawRule {pos, entity, type_,          clauses}) input
  ___@(StGrammar_24, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<Name>"])
  stk@(StGrammar_25, _, _) ((pos, LowercaseName n) : input) -> actionForGrammar (StGrammar_15,  pos, n :> stk) input
  stk@(StGrammar_25, _, _) ((pos, StringLiteral n) : input) -> actionForGrammar (StGrammar_20,  pos, n :> stk) input
  ___@(StGrammar_25, _, _) input -> \end -> Left (currentPos input end, ["<name>", "<string>"])
  ___@(StGrammar_26, _, clauses :> (_, _, _ :> (_, _, entity :> stk@(_, pos, _)))) input@[] -> gotoRuleForGrammar stk (RawRule {pos, entity, type_ = entity, clauses}) input
  ___@(StGrammar_26, _, clauses :> (_, _, _ :> (_, _, entity :> stk@(_, pos, _)))) input@((_, UppercaseName _) : _) -> gotoRuleForGrammar stk (RawRule {pos, entity, type_ = entity, clauses}) input
  ___@(StGrammar_26, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<Name>"])
  ___@(StGrammar_27, _, rules :> (_, _, targets :> (_, _, adds :> stk@(_, pos, _)))) input@[] -> gotoGrammarForGrammar stk (RawGrammar {imports = adds, targets, rules}) input
  ___@(StGrammar_27, _, _) input -> \end -> Left (currentPos input end, ["EOF"])
  stk@(StGrammar_28, _, _) ((pos, UppercaseName n) : input) -> actionForGrammar (StGrammar_30,  pos, n :> stk) input
  ___@(StGrammar_28, _, _) input -> \end -> Left (currentPos input end, ["<Name>"])
  ___@(StGrammar_29, _, es :> (_, _, _ :> stk@(_, pos, _))) input@((_, UppercaseName _) : _) -> gotoStartersForGrammar stk (es) input
  ___@(StGrammar_29, _, _) input -> \end -> Left (currentPos input end, ["<Name>"])
  stk@(StGrammar_30, _, _) ((pos, ",") : input) -> actionForGrammar (StGrammar_31,  pos, () :> stk) input
  ___@(StGrammar_30, _, e :> stk@(_, pos, _)) input@((_, UppercaseName _) : _) -> gotoNonTerminalsForGrammar stk ([(pos, e)]) input
  ___@(StGrammar_30, _, _) input -> \end -> Left (currentPos input end, [",", "<Name>"])
  stk@(StGrammar_31, _, _) ((pos, UppercaseName n) : input) -> actionForGrammar (StGrammar_30,  pos, n :> stk) input
  ___@(StGrammar_31, _, _) input -> \end -> Left (currentPos input end, ["<Name>"])
  ___@(StGrammar_32, _, es :> (_, _, _ :> (_, _, e :> stk@(_, pos, _)))) input@((_, UppercaseName _) : _) -> gotoNonTerminalsForGrammar stk (NonEmpty.cons (pos, e) es) input
  ___@(StGrammar_32, _, _) input -> \end -> Left (currentPos input end, ["<Name>"])
  ___@(StGrammar_33, _, e :> _) [] -> \_ -> Right e
  ___@(StGrammar_33, _, _) input -> \end -> Left (currentPos input end, ["EOF"])
  stk@(StGrammar_34, _, _) ((pos, StringLiteral n) : input) -> actionForGrammar (StGrammar_36,  pos, n :> stk) input
  ___@(StGrammar_34, _, _ :> stk@(_, pos, _)) input@((_, "start") : _) -> gotoAdditionsForGrammar stk ([]) input
  ___@(StGrammar_34, _, _) input -> \end -> Left (currentPos input end, ["<string>", "start"])
  ___@(StGrammar_35, _, ls :> (_, _, _ :> stk@(_, pos, _))) input@((_, "start") : _) -> gotoAdditionsForGrammar stk (ls) input
  ___@(StGrammar_35, _, _) input -> \end -> Left (currentPos input end, ["start"])
  stk@(StGrammar_36, _, _) ((pos, StringLiteral n) : input) -> actionForGrammar (StGrammar_36,  pos, n :> stk) input
  ___@(StGrammar_36, _, t :> stk@(_, pos, _)) input@((_, "start") : _) -> gotoLinesForGrammar stk ([t]) input
  ___@(StGrammar_36, _, _) input -> \end -> Left (currentPos input end, ["<string>", "start"])
  ___@(StGrammar_37, _, ts :> (_, _, t :> stk@(_, pos, _))) input@((_, "start") : _) -> gotoLinesForGrammar stk ( t : ts) input
  ___@(StGrammar_37, _, _) input -> \end -> Left (currentPos input end, ["start"])

parseGrammar :: FilePath -> IO (Either LexerError (Either (Pos, [String]) (RawGrammar)))
parseGrammar filepath = do
  text <- Text.readFile filepath
  case lexText filepath text [",", ":", "<Name>", "<name>", "<string>", "=", "=>", "imports", "start", "|"] of
    Left  err   -> pure (Left err)
    Right (input, end) -> 
      pure (Right (actionForGrammar (StGrammar_0, startPos filepath text, Nil) input end))


