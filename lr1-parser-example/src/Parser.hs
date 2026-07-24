{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Parser (
  parseProgram, parseTestSuite
) where

import Data.Text.Position (Pos, startPos)
import Data.Lexeme
import Text.Lexer.Default
import Data.Text.IO as Text
import AST
import Data.Text (Text)

type Stack st xs =
  ( st xs
  , Pos
  , Split st xs
  )

data Split st xs where
  Nil  ::                     Split st '[]
  (:>) :: x -> Stack st xs -> Split st (x : xs)

data StProgram xs where
  StProgram_0 :: StProgram (a)
  StProgram_1 :: StProgram (Call : a)
  StProgram_2 :: StProgram (() : Call : a)
  StProgram_3 :: StProgram (Expr : a)
  StProgram_4 :: StProgram (() : Expr : a)
  StProgram_5 :: StProgram (Const : a)
  StProgram_6 :: StProgram (Expr : () : Expr : a)
  StProgram_7 :: StProgram (() : Expr : a)
  StProgram_8 :: StProgram (Expr : () : Expr : a)
  StProgram_9 :: StProgram (() : a)
  StProgram_10 :: StProgram (Expr : () : a)
  StProgram_11 :: StProgram (() : Expr : () : a)
  StProgram_12 :: StProgram (Expr : a)
  StProgram_13 :: StProgram (Expr : a)
  StProgram_14 :: StProgram (Text : a)
  StProgram_15 :: StProgram (Text : a)
  StProgram_16 :: StProgram (Integer : a)
  StProgram_17 :: StProgram (() : Expr : a)
  StProgram_18 :: StProgram (Expr : () : Expr : a)
  StProgram_19 :: StProgram (Call : a)
  StProgram_20 :: StProgram (Cond : a)
  StProgram_21 :: StProgram (() : Cond : a)
  StProgram_22 :: StProgram ([Cond] : () : Cond : a)
  StProgram_23 :: StProgram (Expr : a)
  StProgram_24 :: StProgram (Text : a)
  StProgram_25 :: StProgram ([Expr] : Text : a)
  StProgram_26 :: StProgram (() : a)
  StProgram_27 :: StProgram (Expr : a)
  StProgram_28 :: StProgram (() : Expr : a)
  StProgram_29 :: StProgram ([Expr] : () : Expr : a)
  StProgram_30 :: StProgram ([Expr] : () : a)
  StProgram_31 :: StProgram (() : [Expr] : () : a)
  StProgram_32 :: StProgram (() : () : a)
  StProgram_33 :: StProgram (() : a)
  StProgram_34 :: StProgram (Call : () : a)
  StProgram_35 :: StProgram (Text : a)
  StProgram_36 :: StProgram ([Cond] : () : Call : a)
  StProgram_37 :: StProgram (() : [Cond] : () : Call : a)
  StProgram_38 :: StProgram (() : [Cond] : () : Call : a)
  StProgram_39 :: StProgram (Change : a)
  StProgram_40 :: StProgram (() : Change : a)
  StProgram_41 :: StProgram ([Change] : () : Change : a)
  StProgram_42 :: StProgram (() : a)
  StProgram_43 :: StProgram (Call : () : a)
  StProgram_44 :: StProgram (() : a)
  StProgram_45 :: StProgram (Call : () : a)
  StProgram_46 :: StProgram ([Change] : () : [Cond] : () : Call : a)
  StProgram_47 :: StProgram (() : [Change] : () : [Cond] : () : Call : a)
  StProgram_48 :: StProgram (() : Call : a)
  StProgram_49 :: StProgram (() : Call : a)
  StProgram_50 :: StProgram ([Cond] : () : Call : a)
  StProgram_51 :: StProgram (() : [Cond] : () : Call : a)
  StProgram_52 :: StProgram (() : Call : a)
  StProgram_53 :: StProgram ([Change] : () : Call : a)
  StProgram_54 :: StProgram (() : [Change] : () : Call : a)
  StProgram_55 :: StProgram (Clause : a)
  StProgram_56 :: StProgram (Effect : a)
  StProgram_57 :: StProgram (Program : a)
  StProgram_58 :: StProgram (Stmt : a)
  StProgram_59 :: StProgram ([Stmt] : Stmt : a)
  StProgram_60 :: StProgram ([Stmt] : a)

gotoAddForProgram :: Stack StProgram xs -> (Expr) -> [Lexeme] -> Pos -> Either (Pos, [String]) (Program)
gotoAddForProgram stack@(state, pos, _) parsed = case state of
  StProgram_2 -> actionForProgram(StProgram_3, pos, (parsed :> stack))
  StProgram_9 -> actionForProgram(StProgram_3, pos, (parsed :> stack))
  StProgram_17 -> actionForProgram(StProgram_18, pos, (parsed :> stack))
  StProgram_21 -> actionForProgram(StProgram_3, pos, (parsed :> stack))
  StProgram_26 -> actionForProgram(StProgram_3, pos, (parsed :> stack))
  StProgram_28 -> actionForProgram(StProgram_3, pos, (parsed :> stack))
  StProgram_49 -> actionForProgram(StProgram_3, pos, (parsed :> stack))
  _ -> error ""

gotoCallForProgram :: Stack StProgram xs -> (Call) -> [Lexeme] -> Pos -> Either (Pos, [String]) (Program)
gotoCallForProgram stack@(state, pos, _) parsed = case state of
  StProgram_0 -> actionForProgram(StProgram_1, pos, (parsed :> stack))
  StProgram_2 -> actionForProgram(StProgram_19, pos, (parsed :> stack))
  StProgram_21 -> actionForProgram(StProgram_19, pos, (parsed :> stack))
  StProgram_33 -> actionForProgram(StProgram_34, pos, (parsed :> stack))
  StProgram_42 -> actionForProgram(StProgram_43, pos, (parsed :> stack))
  StProgram_44 -> actionForProgram(StProgram_45, pos, (parsed :> stack))
  StProgram_49 -> actionForProgram(StProgram_19, pos, (parsed :> stack))
  StProgram_58 -> actionForProgram(StProgram_1, pos, (parsed :> stack))
  _ -> error ""

gotoChangeForProgram :: Stack StProgram xs -> (Change) -> [Lexeme] -> Pos -> Either (Pos, [String]) (Program)
gotoChangeForProgram stack@(state, pos, _) parsed = case state of
  StProgram_38 -> actionForProgram(StProgram_39, pos, (parsed :> stack))
  StProgram_40 -> actionForProgram(StProgram_39, pos, (parsed :> stack))
  StProgram_52 -> actionForProgram(StProgram_39, pos, (parsed :> stack))
  _ -> error ""

gotoChangesForProgram :: Stack StProgram xs -> ([Change]) -> [Lexeme] -> Pos -> Either (Pos, [String]) (Program)
gotoChangesForProgram stack@(state, pos, _) parsed = case state of
  StProgram_38 -> actionForProgram(StProgram_46, pos, (parsed :> stack))
  StProgram_40 -> actionForProgram(StProgram_41, pos, (parsed :> stack))
  StProgram_52 -> actionForProgram(StProgram_53, pos, (parsed :> stack))
  _ -> error ""

gotoClauseForProgram :: Stack StProgram xs -> (Clause) -> [Lexeme] -> Pos -> Either (Pos, [String]) (Program)
gotoClauseForProgram stack@(state, pos, _) parsed = case state of
  StProgram_0 -> actionForProgram(StProgram_55, pos, (parsed :> stack))
  StProgram_58 -> actionForProgram(StProgram_55, pos, (parsed :> stack))
  _ -> error ""

gotoCondForProgram :: Stack StProgram xs -> (Cond) -> [Lexeme] -> Pos -> Either (Pos, [String]) (Program)
gotoCondForProgram stack@(state, pos, _) parsed = case state of
  StProgram_2 -> actionForProgram(StProgram_20, pos, (parsed :> stack))
  StProgram_21 -> actionForProgram(StProgram_20, pos, (parsed :> stack))
  StProgram_49 -> actionForProgram(StProgram_20, pos, (parsed :> stack))
  _ -> error ""

gotoCondsForProgram :: Stack StProgram xs -> ([Cond]) -> [Lexeme] -> Pos -> Either (Pos, [String]) (Program)
gotoCondsForProgram stack@(state, pos, _) parsed = case state of
  StProgram_2 -> actionForProgram(StProgram_36, pos, (parsed :> stack))
  StProgram_21 -> actionForProgram(StProgram_22, pos, (parsed :> stack))
  StProgram_49 -> actionForProgram(StProgram_50, pos, (parsed :> stack))
  _ -> error ""

gotoConstForProgram :: Stack StProgram xs -> (Const) -> [Lexeme] -> Pos -> Either (Pos, [String]) (Program)
gotoConstForProgram stack@(state, pos, _) parsed = case state of
  StProgram_2 -> actionForProgram(StProgram_5, pos, (parsed :> stack))
  StProgram_4 -> actionForProgram(StProgram_5, pos, (parsed :> stack))
  StProgram_7 -> actionForProgram(StProgram_5, pos, (parsed :> stack))
  StProgram_9 -> actionForProgram(StProgram_5, pos, (parsed :> stack))
  StProgram_17 -> actionForProgram(StProgram_5, pos, (parsed :> stack))
  StProgram_21 -> actionForProgram(StProgram_5, pos, (parsed :> stack))
  StProgram_26 -> actionForProgram(StProgram_5, pos, (parsed :> stack))
  StProgram_28 -> actionForProgram(StProgram_5, pos, (parsed :> stack))
  StProgram_49 -> actionForProgram(StProgram_5, pos, (parsed :> stack))
  _ -> error ""

gotoEffectForProgram :: Stack StProgram xs -> (Effect) -> [Lexeme] -> Pos -> Either (Pos, [String]) (Program)
gotoEffectForProgram stack@(state, pos, _) parsed = case state of
  StProgram_0 -> actionForProgram(StProgram_56, pos, (parsed :> stack))
  StProgram_58 -> actionForProgram(StProgram_56, pos, (parsed :> stack))
  _ -> error ""

gotoExprForProgram :: Stack StProgram xs -> (Expr) -> [Lexeme] -> Pos -> Either (Pos, [String]) (Program)
gotoExprForProgram stack@(state, pos, _) parsed = case state of
  StProgram_2 -> actionForProgram(StProgram_23, pos, (parsed :> stack))
  StProgram_9 -> actionForProgram(StProgram_10, pos, (parsed :> stack))
  StProgram_21 -> actionForProgram(StProgram_23, pos, (parsed :> stack))
  StProgram_26 -> actionForProgram(StProgram_27, pos, (parsed :> stack))
  StProgram_28 -> actionForProgram(StProgram_27, pos, (parsed :> stack))
  StProgram_49 -> actionForProgram(StProgram_23, pos, (parsed :> stack))
  _ -> error ""

gotoExprs1ForProgram :: Stack StProgram xs -> ([Expr]) -> [Lexeme] -> Pos -> Either (Pos, [String]) (Program)
gotoExprs1ForProgram stack@(state, pos, _) parsed = case state of
  StProgram_26 -> actionForProgram(StProgram_30, pos, (parsed :> stack))
  StProgram_28 -> actionForProgram(StProgram_29, pos, (parsed :> stack))
  _ -> error ""

gotoMultForProgram :: Stack StProgram xs -> (Expr) -> [Lexeme] -> Pos -> Either (Pos, [String]) (Program)
gotoMultForProgram stack@(state, pos, _) parsed = case state of
  StProgram_2 -> actionForProgram(StProgram_12, pos, (parsed :> stack))
  StProgram_4 -> actionForProgram(StProgram_6, pos, (parsed :> stack))
  StProgram_9 -> actionForProgram(StProgram_12, pos, (parsed :> stack))
  StProgram_17 -> actionForProgram(StProgram_12, pos, (parsed :> stack))
  StProgram_21 -> actionForProgram(StProgram_12, pos, (parsed :> stack))
  StProgram_26 -> actionForProgram(StProgram_12, pos, (parsed :> stack))
  StProgram_28 -> actionForProgram(StProgram_12, pos, (parsed :> stack))
  StProgram_49 -> actionForProgram(StProgram_12, pos, (parsed :> stack))
  _ -> error ""

gotoProgramForProgram :: Stack StProgram xs -> (Program) -> [Lexeme] -> Pos -> Either (Pos, [String]) (Program)
gotoProgramForProgram stack@(state, pos, _) parsed = case state of
  StProgram_0 -> actionForProgram(StProgram_57, pos, (parsed :> stack))
  _ -> error ""

gotoStmtForProgram :: Stack StProgram xs -> (Stmt) -> [Lexeme] -> Pos -> Either (Pos, [String]) (Program)
gotoStmtForProgram stack@(state, pos, _) parsed = case state of
  StProgram_0 -> actionForProgram(StProgram_58, pos, (parsed :> stack))
  StProgram_58 -> actionForProgram(StProgram_58, pos, (parsed :> stack))
  _ -> error ""

gotoStmtsForProgram :: Stack StProgram xs -> ([Stmt]) -> [Lexeme] -> Pos -> Either (Pos, [String]) (Program)
gotoStmtsForProgram stack@(state, pos, _) parsed = case state of
  StProgram_0 -> actionForProgram(StProgram_60, pos, (parsed :> stack))
  StProgram_58 -> actionForProgram(StProgram_59, pos, (parsed :> stack))
  _ -> error ""

gotoTermForProgram :: Stack StProgram xs -> (Expr) -> [Lexeme] -> Pos -> Either (Pos, [String]) (Program)
gotoTermForProgram stack@(state, pos, _) parsed = case state of
  StProgram_2 -> actionForProgram(StProgram_13, pos, (parsed :> stack))
  StProgram_4 -> actionForProgram(StProgram_13, pos, (parsed :> stack))
  StProgram_7 -> actionForProgram(StProgram_8, pos, (parsed :> stack))
  StProgram_9 -> actionForProgram(StProgram_13, pos, (parsed :> stack))
  StProgram_17 -> actionForProgram(StProgram_13, pos, (parsed :> stack))
  StProgram_21 -> actionForProgram(StProgram_13, pos, (parsed :> stack))
  StProgram_26 -> actionForProgram(StProgram_13, pos, (parsed :> stack))
  StProgram_28 -> actionForProgram(StProgram_13, pos, (parsed :> stack))
  StProgram_49 -> actionForProgram(StProgram_13, pos, (parsed :> stack))
  _ -> error ""

gotoTestForProgram :: Stack StProgram xs -> (Test) -> [Lexeme] -> Pos -> Either (Pos, [String]) (Program)
gotoTestForProgram stack@(state, pos, _) parsed = case state of
  _ -> error ""

gotoTestSuiteForProgram :: Stack StProgram xs -> (TestSuite) -> [Lexeme] -> Pos -> Either (Pos, [String]) (Program)
gotoTestSuiteForProgram stack@(state, pos, _) parsed = case state of
  _ -> error ""

gotoTestsForProgram :: Stack StProgram xs -> ([Test]) -> [Lexeme] -> Pos -> Either (Pos, [String]) (Program)
gotoTestsForProgram stack@(state, pos, _) parsed = case state of
  _ -> error ""

gotoTupleForProgram :: Stack StProgram xs -> ([Expr]) -> [Lexeme] -> Pos -> Either (Pos, [String]) (Program)
gotoTupleForProgram stack@(state, pos, _) parsed = case state of
  StProgram_24 -> actionForProgram(StProgram_25, pos, (parsed :> stack))
  StProgram_35 -> actionForProgram(StProgram_25, pos, (parsed :> stack))
  _ -> error ""

actionForProgram :: Stack StProgram xs -> [Lexeme] -> Pos -> Either (Pos, [String]) (Program)
actionForProgram = \cases
  stk@(StProgram_0, _, _) ((pos, LowercaseName n) : input) -> actionForProgram (StProgram_35,  pos, n :> stk) input
  ___@(StProgram_0, _, _) input -> \end -> Left (currentPos input end, ["<name>"])
  stk@(StProgram_1, _, _) ((pos, "->") : input) -> actionForProgram (StProgram_2,  pos, () :> stk) input
  stk@(StProgram_1, _, _) ((pos, ".") : input) -> actionForProgram (StProgram_48,  pos, () :> stk) input
  stk@(StProgram_1, _, _) ((pos, "<-") : input) -> actionForProgram (StProgram_49,  pos, () :> stk) input
  stk@(StProgram_1, _, _) ((pos, "=>") : input) -> actionForProgram (StProgram_52,  pos, () :> stk) input
  ___@(StProgram_1, _, _) input -> \end -> Left (currentPos input end, ["->", ".", "<-", "=>"])
  stk@(StProgram_2, _, _) ((pos, "(") : input) -> actionForProgram (StProgram_9,  pos, () :> stk) input
  stk@(StProgram_2, _, _) ((pos, UppercaseName n) : input) -> actionForProgram (StProgram_14,  pos, n :> stk) input
  stk@(StProgram_2, _, _) ((pos, LowercaseName n) : input) -> actionForProgram (StProgram_24,  pos, n :> stk) input
  stk@(StProgram_2, _, _) ((pos, NumberLiteral n) : input) -> actionForProgram (StProgram_16,  pos, n :> stk) input
  stk@(StProgram_2, _, _) ((pos, "~") : input) -> actionForProgram (StProgram_33,  pos, () :> stk) input
  ___@(StProgram_2, _, _) input -> \end -> Left (currentPos input end, ["(", "<Name>", "<name>", "<number>", "~"])
  ___@(StProgram_3, _, a :> stk@(_, pos, _)) input@((_, ")") : _) -> gotoExprForProgram stk (a) input
  stk@(StProgram_3, _, _) ((pos, "+") : input) -> actionForProgram (StProgram_4,  pos, () :> stk) input
  ___@(StProgram_3, _, a :> stk@(_, pos, _)) input@((_, ",") : _) -> gotoExprForProgram stk (a) input
  ___@(StProgram_3, _, a :> stk@(_, pos, _)) input@((_, ".") : _) -> gotoExprForProgram stk (a) input
  stk@(StProgram_3, _, _) ((pos, "=") : input) -> actionForProgram (StProgram_17,  pos, () :> stk) input
  ___@(StProgram_3, _, a :> stk@(_, pos, _)) input@((_, "=>") : _) -> gotoExprForProgram stk (a) input
  ___@(StProgram_3, _, _) input -> \end -> Left (currentPos input end, [")", "+", ",", ".", "=", "=>"])
  stk@(StProgram_4, _, _) ((pos, "(") : input) -> actionForProgram (StProgram_9,  pos, () :> stk) input
  stk@(StProgram_4, _, _) ((pos, UppercaseName n) : input) -> actionForProgram (StProgram_14,  pos, n :> stk) input
  stk@(StProgram_4, _, _) ((pos, LowercaseName n) : input) -> actionForProgram (StProgram_15,  pos, n :> stk) input
  stk@(StProgram_4, _, _) ((pos, NumberLiteral n) : input) -> actionForProgram (StProgram_16,  pos, n :> stk) input
  ___@(StProgram_4, _, _) input -> \end -> Left (currentPos input end, ["(", "<Name>", "<name>", "<number>"])
  ___@(StProgram_5, _, n :> stk@(_, pos, _)) input@((_, ")") : _) -> gotoTermForProgram stk (ExprConst pos n) input
  ___@(StProgram_5, _, n :> stk@(_, pos, _)) input@((_, "*") : _) -> gotoTermForProgram stk (ExprConst pos n) input
  ___@(StProgram_5, _, n :> stk@(_, pos, _)) input@((_, "+") : _) -> gotoTermForProgram stk (ExprConst pos n) input
  ___@(StProgram_5, _, n :> stk@(_, pos, _)) input@((_, ",") : _) -> gotoTermForProgram stk (ExprConst pos n) input
  ___@(StProgram_5, _, n :> stk@(_, pos, _)) input@((_, ".") : _) -> gotoTermForProgram stk (ExprConst pos n) input
  ___@(StProgram_5, _, n :> stk@(_, pos, _)) input@((_, "=") : _) -> gotoTermForProgram stk (ExprConst pos n) input
  ___@(StProgram_5, _, n :> stk@(_, pos, _)) input@((_, "=>") : _) -> gotoTermForProgram stk (ExprConst pos n) input
  ___@(StProgram_5, _, _) input -> \end -> Left (currentPos input end, [")", "*", "+", ",", ".", "=", "=>"])
  ___@(StProgram_6, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, ")") : _) -> gotoAddForProgram stk (ExprBinary pos a Add b) input
  stk@(StProgram_6, _, _) ((pos, "*") : input) -> actionForProgram (StProgram_7,  pos, () :> stk) input
  ___@(StProgram_6, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "+") : _) -> gotoAddForProgram stk (ExprBinary pos a Add b) input
  ___@(StProgram_6, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, ",") : _) -> gotoAddForProgram stk (ExprBinary pos a Add b) input
  ___@(StProgram_6, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, ".") : _) -> gotoAddForProgram stk (ExprBinary pos a Add b) input
  ___@(StProgram_6, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "=") : _) -> gotoAddForProgram stk (ExprBinary pos a Add b) input
  ___@(StProgram_6, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "=>") : _) -> gotoAddForProgram stk (ExprBinary pos a Add b) input
  ___@(StProgram_6, _, _) input -> \end -> Left (currentPos input end, [")", "*", "+", ",", ".", "=", "=>"])
  stk@(StProgram_7, _, _) ((pos, "(") : input) -> actionForProgram (StProgram_9,  pos, () :> stk) input
  stk@(StProgram_7, _, _) ((pos, UppercaseName n) : input) -> actionForProgram (StProgram_14,  pos, n :> stk) input
  stk@(StProgram_7, _, _) ((pos, LowercaseName n) : input) -> actionForProgram (StProgram_15,  pos, n :> stk) input
  stk@(StProgram_7, _, _) ((pos, NumberLiteral n) : input) -> actionForProgram (StProgram_16,  pos, n :> stk) input
  ___@(StProgram_7, _, _) input -> \end -> Left (currentPos input end, ["(", "<Name>", "<name>", "<number>"])
  ___@(StProgram_8, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, ")") : _) -> gotoMultForProgram stk (ExprBinary pos a Mult b) input
  ___@(StProgram_8, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "*") : _) -> gotoMultForProgram stk (ExprBinary pos a Mult b) input
  ___@(StProgram_8, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "+") : _) -> gotoMultForProgram stk (ExprBinary pos a Mult b) input
  ___@(StProgram_8, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, ",") : _) -> gotoMultForProgram stk (ExprBinary pos a Mult b) input
  ___@(StProgram_8, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, ".") : _) -> gotoMultForProgram stk (ExprBinary pos a Mult b) input
  ___@(StProgram_8, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "=") : _) -> gotoMultForProgram stk (ExprBinary pos a Mult b) input
  ___@(StProgram_8, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "=>") : _) -> gotoMultForProgram stk (ExprBinary pos a Mult b) input
  ___@(StProgram_8, _, _) input -> \end -> Left (currentPos input end, [")", "*", "+", ",", ".", "=", "=>"])
  stk@(StProgram_9, _, _) ((pos, "(") : input) -> actionForProgram (StProgram_9,  pos, () :> stk) input
  stk@(StProgram_9, _, _) ((pos, UppercaseName n) : input) -> actionForProgram (StProgram_14,  pos, n :> stk) input
  stk@(StProgram_9, _, _) ((pos, LowercaseName n) : input) -> actionForProgram (StProgram_15,  pos, n :> stk) input
  stk@(StProgram_9, _, _) ((pos, NumberLiteral n) : input) -> actionForProgram (StProgram_16,  pos, n :> stk) input
  ___@(StProgram_9, _, _) input -> \end -> Left (currentPos input end, ["(", "<Name>", "<name>", "<number>"])
  stk@(StProgram_10, _, _) ((pos, ")") : input) -> actionForProgram (StProgram_11,  pos, () :> stk) input
  ___@(StProgram_10, _, _) input -> \end -> Left (currentPos input end, [")"])
  ___@(StProgram_11, _, _ :> (_, _, e :> (_, _, _ :> stk@(_, pos, _)))) input@((_, ")") : _) -> gotoTermForProgram stk (e) input
  ___@(StProgram_11, _, _ :> (_, _, e :> (_, _, _ :> stk@(_, pos, _)))) input@((_, "*") : _) -> gotoTermForProgram stk (e) input
  ___@(StProgram_11, _, _ :> (_, _, e :> (_, _, _ :> stk@(_, pos, _)))) input@((_, "+") : _) -> gotoTermForProgram stk (e) input
  ___@(StProgram_11, _, _ :> (_, _, e :> (_, _, _ :> stk@(_, pos, _)))) input@((_, ",") : _) -> gotoTermForProgram stk (e) input
  ___@(StProgram_11, _, _ :> (_, _, e :> (_, _, _ :> stk@(_, pos, _)))) input@((_, ".") : _) -> gotoTermForProgram stk (e) input
  ___@(StProgram_11, _, _ :> (_, _, e :> (_, _, _ :> stk@(_, pos, _)))) input@((_, "=") : _) -> gotoTermForProgram stk (e) input
  ___@(StProgram_11, _, _ :> (_, _, e :> (_, _, _ :> stk@(_, pos, _)))) input@((_, "=>") : _) -> gotoTermForProgram stk (e) input
  ___@(StProgram_11, _, _) input -> \end -> Left (currentPos input end, [")", "*", "+", ",", ".", "=", "=>"])
  ___@(StProgram_12, _, a :> stk@(_, pos, _)) input@((_, ")") : _) -> gotoAddForProgram stk (a) input
  stk@(StProgram_12, _, _) ((pos, "*") : input) -> actionForProgram (StProgram_7,  pos, () :> stk) input
  ___@(StProgram_12, _, a :> stk@(_, pos, _)) input@((_, "+") : _) -> gotoAddForProgram stk (a) input
  ___@(StProgram_12, _, a :> stk@(_, pos, _)) input@((_, ",") : _) -> gotoAddForProgram stk (a) input
  ___@(StProgram_12, _, a :> stk@(_, pos, _)) input@((_, ".") : _) -> gotoAddForProgram stk (a) input
  ___@(StProgram_12, _, a :> stk@(_, pos, _)) input@((_, "=") : _) -> gotoAddForProgram stk (a) input
  ___@(StProgram_12, _, a :> stk@(_, pos, _)) input@((_, "=>") : _) -> gotoAddForProgram stk (a) input
  ___@(StProgram_12, _, _) input -> \end -> Left (currentPos input end, [")", "*", "+", ",", ".", "=", "=>"])
  ___@(StProgram_13, _, a :> stk@(_, pos, _)) input@((_, ")") : _) -> gotoMultForProgram stk (a) input
  ___@(StProgram_13, _, a :> stk@(_, pos, _)) input@((_, "*") : _) -> gotoMultForProgram stk (a) input
  ___@(StProgram_13, _, a :> stk@(_, pos, _)) input@((_, "+") : _) -> gotoMultForProgram stk (a) input
  ___@(StProgram_13, _, a :> stk@(_, pos, _)) input@((_, ",") : _) -> gotoMultForProgram stk (a) input
  ___@(StProgram_13, _, a :> stk@(_, pos, _)) input@((_, ".") : _) -> gotoMultForProgram stk (a) input
  ___@(StProgram_13, _, a :> stk@(_, pos, _)) input@((_, "=") : _) -> gotoMultForProgram stk (a) input
  ___@(StProgram_13, _, a :> stk@(_, pos, _)) input@((_, "=>") : _) -> gotoMultForProgram stk (a) input
  ___@(StProgram_13, _, _) input -> \end -> Left (currentPos input end, [")", "*", "+", ",", ".", "=", "=>"])
  ___@(StProgram_14, _, n :> stk@(_, pos, _)) input@((_, ")") : _) -> gotoTermForProgram stk (ExprVar   pos n) input
  ___@(StProgram_14, _, n :> stk@(_, pos, _)) input@((_, "*") : _) -> gotoTermForProgram stk (ExprVar   pos n) input
  ___@(StProgram_14, _, n :> stk@(_, pos, _)) input@((_, "+") : _) -> gotoTermForProgram stk (ExprVar   pos n) input
  ___@(StProgram_14, _, n :> stk@(_, pos, _)) input@((_, ",") : _) -> gotoTermForProgram stk (ExprVar   pos n) input
  ___@(StProgram_14, _, n :> stk@(_, pos, _)) input@((_, ".") : _) -> gotoTermForProgram stk (ExprVar   pos n) input
  ___@(StProgram_14, _, n :> stk@(_, pos, _)) input@((_, "=") : _) -> gotoTermForProgram stk (ExprVar   pos n) input
  ___@(StProgram_14, _, n :> stk@(_, pos, _)) input@((_, "=>") : _) -> gotoTermForProgram stk (ExprVar   pos n) input
  ___@(StProgram_14, _, _) input -> \end -> Left (currentPos input end, [")", "*", "+", ",", ".", "=", "=>"])
  ___@(StProgram_15, _, n :> stk@(_, pos, _)) input@((_, ")") : _) -> gotoConstForProgram stk (ConstNamed pos n) input
  ___@(StProgram_15, _, n :> stk@(_, pos, _)) input@((_, "*") : _) -> gotoConstForProgram stk (ConstNamed pos n) input
  ___@(StProgram_15, _, n :> stk@(_, pos, _)) input@((_, "+") : _) -> gotoConstForProgram stk (ConstNamed pos n) input
  ___@(StProgram_15, _, n :> stk@(_, pos, _)) input@((_, ",") : _) -> gotoConstForProgram stk (ConstNamed pos n) input
  ___@(StProgram_15, _, n :> stk@(_, pos, _)) input@((_, ".") : _) -> gotoConstForProgram stk (ConstNamed pos n) input
  ___@(StProgram_15, _, n :> stk@(_, pos, _)) input@((_, "=") : _) -> gotoConstForProgram stk (ConstNamed pos n) input
  ___@(StProgram_15, _, n :> stk@(_, pos, _)) input@((_, "=>") : _) -> gotoConstForProgram stk (ConstNamed pos n) input
  ___@(StProgram_15, _, _) input -> \end -> Left (currentPos input end, [")", "*", "+", ",", ".", "=", "=>"])
  ___@(StProgram_16, _, n :> stk@(_, pos, _)) input@((_, ")") : _) -> gotoConstForProgram stk (ConstInt   pos n) input
  ___@(StProgram_16, _, n :> stk@(_, pos, _)) input@((_, "*") : _) -> gotoConstForProgram stk (ConstInt   pos n) input
  ___@(StProgram_16, _, n :> stk@(_, pos, _)) input@((_, "+") : _) -> gotoConstForProgram stk (ConstInt   pos n) input
  ___@(StProgram_16, _, n :> stk@(_, pos, _)) input@((_, ",") : _) -> gotoConstForProgram stk (ConstInt   pos n) input
  ___@(StProgram_16, _, n :> stk@(_, pos, _)) input@((_, ".") : _) -> gotoConstForProgram stk (ConstInt   pos n) input
  ___@(StProgram_16, _, n :> stk@(_, pos, _)) input@((_, "=") : _) -> gotoConstForProgram stk (ConstInt   pos n) input
  ___@(StProgram_16, _, n :> stk@(_, pos, _)) input@((_, "=>") : _) -> gotoConstForProgram stk (ConstInt   pos n) input
  ___@(StProgram_16, _, _) input -> \end -> Left (currentPos input end, [")", "*", "+", ",", ".", "=", "=>"])
  stk@(StProgram_17, _, _) ((pos, "(") : input) -> actionForProgram (StProgram_9,  pos, () :> stk) input
  stk@(StProgram_17, _, _) ((pos, UppercaseName n) : input) -> actionForProgram (StProgram_14,  pos, n :> stk) input
  stk@(StProgram_17, _, _) ((pos, LowercaseName n) : input) -> actionForProgram (StProgram_15,  pos, n :> stk) input
  stk@(StProgram_17, _, _) ((pos, NumberLiteral n) : input) -> actionForProgram (StProgram_16,  pos, n :> stk) input
  ___@(StProgram_17, _, _) input -> \end -> Left (currentPos input end, ["(", "<Name>", "<name>", "<number>"])
  ___@(StProgram_18, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, ")") : _) -> gotoExprForProgram stk (ExprBinary pos a Equals b) input
  stk@(StProgram_18, _, _) ((pos, "+") : input) -> actionForProgram (StProgram_4,  pos, () :> stk) input
  ___@(StProgram_18, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, ",") : _) -> gotoExprForProgram stk (ExprBinary pos a Equals b) input
  ___@(StProgram_18, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, ".") : _) -> gotoExprForProgram stk (ExprBinary pos a Equals b) input
  ___@(StProgram_18, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "=>") : _) -> gotoExprForProgram stk (ExprBinary pos a Equals b) input
  ___@(StProgram_18, _, _) input -> \end -> Left (currentPos input end, [")", "+", ",", ".", "=>"])
  ___@(StProgram_19, _, c :> stk@(_, pos, _)) input@((_, ",") : _) -> gotoCondForProgram stk (CondAssert pos c) input
  ___@(StProgram_19, _, c :> stk@(_, pos, _)) input@((_, ".") : _) -> gotoCondForProgram stk (CondAssert pos c) input
  ___@(StProgram_19, _, c :> stk@(_, pos, _)) input@((_, "=>") : _) -> gotoCondForProgram stk (CondAssert pos c) input
  ___@(StProgram_19, _, _) input -> \end -> Left (currentPos input end, [",", ".", "=>"])
  stk@(StProgram_20, _, _) ((pos, ",") : input) -> actionForProgram (StProgram_21,  pos, () :> stk) input
  ___@(StProgram_20, _, c :> stk@(_, pos, _)) input@((_, ".") : _) -> gotoCondsForProgram stk ([c]) input
  ___@(StProgram_20, _, c :> stk@(_, pos, _)) input@((_, "=>") : _) -> gotoCondsForProgram stk ([c]) input
  ___@(StProgram_20, _, _) input -> \end -> Left (currentPos input end, [",", ".", "=>"])
  stk@(StProgram_21, _, _) ((pos, "(") : input) -> actionForProgram (StProgram_9,  pos, () :> stk) input
  stk@(StProgram_21, _, _) ((pos, UppercaseName n) : input) -> actionForProgram (StProgram_14,  pos, n :> stk) input
  stk@(StProgram_21, _, _) ((pos, LowercaseName n) : input) -> actionForProgram (StProgram_24,  pos, n :> stk) input
  stk@(StProgram_21, _, _) ((pos, NumberLiteral n) : input) -> actionForProgram (StProgram_16,  pos, n :> stk) input
  stk@(StProgram_21, _, _) ((pos, "~") : input) -> actionForProgram (StProgram_33,  pos, () :> stk) input
  ___@(StProgram_21, _, _) input -> \end -> Left (currentPos input end, ["(", "<Name>", "<name>", "<number>", "~"])
  ___@(StProgram_22, _, cs :> (_, _, _ :> (_, _, c :> stk@(_, pos, _)))) input@((_, ".") : _) -> gotoCondsForProgram stk (c : cs) input
  ___@(StProgram_22, _, cs :> (_, _, _ :> (_, _, c :> stk@(_, pos, _)))) input@((_, "=>") : _) -> gotoCondsForProgram stk (c : cs) input
  ___@(StProgram_22, _, _) input -> \end -> Left (currentPos input end, [".", "=>"])
  ___@(StProgram_23, _, e :> stk@(_, pos, _)) input@((_, ",") : _) -> gotoCondForProgram stk (CondGuard  pos e) input
  ___@(StProgram_23, _, e :> stk@(_, pos, _)) input@((_, ".") : _) -> gotoCondForProgram stk (CondGuard  pos e) input
  ___@(StProgram_23, _, e :> stk@(_, pos, _)) input@((_, "=>") : _) -> gotoCondForProgram stk (CondGuard  pos e) input
  ___@(StProgram_23, _, _) input -> \end -> Left (currentPos input end, [",", ".", "=>"])
  stk@(StProgram_24, _, _) ((pos, "(") : input) -> actionForProgram (StProgram_26,  pos, () :> stk) input
  ___@(StProgram_24, _, n :> stk@(_, pos, _)) input@((_, "*") : _) -> gotoConstForProgram stk (ConstNamed pos n) input
  ___@(StProgram_24, _, n :> stk@(_, pos, _)) input@((_, "+") : _) -> gotoConstForProgram stk (ConstNamed pos n) input
  ___@(StProgram_24, _, n :> stk@(_, pos, _)) input@((_, ",") : _) -> gotoConstForProgram stk (ConstNamed pos n) input
  ___@(StProgram_24, _, n :> stk@(_, pos, _)) input@((_, ".") : _) -> gotoConstForProgram stk (ConstNamed pos n) input
  ___@(StProgram_24, _, n :> stk@(_, pos, _)) input@((_, "=") : _) -> gotoConstForProgram stk (ConstNamed pos n) input
  ___@(StProgram_24, _, n :> stk@(_, pos, _)) input@((_, "=>") : _) -> gotoConstForProgram stk (ConstNamed pos n) input
  ___@(StProgram_24, _, _) input -> \end -> Left (currentPos input end, ["(", "*", "+", ",", ".", "=", "=>"])
  ___@(StProgram_25, _, t :> (_, _, pre :> stk@(_, pos, _))) input@((_, ",") : _) -> gotoCallForProgram stk (Call pos pre t) input
  ___@(StProgram_25, _, t :> (_, _, pre :> stk@(_, pos, _))) input@((_, "->") : _) -> gotoCallForProgram stk (Call pos pre t) input
  ___@(StProgram_25, _, t :> (_, _, pre :> stk@(_, pos, _))) input@((_, ".") : _) -> gotoCallForProgram stk (Call pos pre t) input
  ___@(StProgram_25, _, t :> (_, _, pre :> stk@(_, pos, _))) input@((_, "<-") : _) -> gotoCallForProgram stk (Call pos pre t) input
  ___@(StProgram_25, _, t :> (_, _, pre :> stk@(_, pos, _))) input@((_, "=>") : _) -> gotoCallForProgram stk (Call pos pre t) input
  ___@(StProgram_25, _, _) input -> \end -> Left (currentPos input end, [",", "->", ".", "<-", "=>"])
  stk@(StProgram_26, _, _) ((pos, "(") : input) -> actionForProgram (StProgram_9,  pos, () :> stk) input
  stk@(StProgram_26, _, _) ((pos, ")") : input) -> actionForProgram (StProgram_32,  pos, () :> stk) input
  stk@(StProgram_26, _, _) ((pos, UppercaseName n) : input) -> actionForProgram (StProgram_14,  pos, n :> stk) input
  stk@(StProgram_26, _, _) ((pos, LowercaseName n) : input) -> actionForProgram (StProgram_15,  pos, n :> stk) input
  stk@(StProgram_26, _, _) ((pos, NumberLiteral n) : input) -> actionForProgram (StProgram_16,  pos, n :> stk) input
  ___@(StProgram_26, _, _) input -> \end -> Left (currentPos input end, ["(", ")", "<Name>", "<name>", "<number>"])
  ___@(StProgram_27, _, e :> stk@(_, pos, _)) input@((_, ")") : _) -> gotoExprs1ForProgram stk ([e]) input
  stk@(StProgram_27, _, _) ((pos, ",") : input) -> actionForProgram (StProgram_28,  pos, () :> stk) input
  ___@(StProgram_27, _, _) input -> \end -> Left (currentPos input end, [")", ","])
  stk@(StProgram_28, _, _) ((pos, "(") : input) -> actionForProgram (StProgram_9,  pos, () :> stk) input
  stk@(StProgram_28, _, _) ((pos, UppercaseName n) : input) -> actionForProgram (StProgram_14,  pos, n :> stk) input
  stk@(StProgram_28, _, _) ((pos, LowercaseName n) : input) -> actionForProgram (StProgram_15,  pos, n :> stk) input
  stk@(StProgram_28, _, _) ((pos, NumberLiteral n) : input) -> actionForProgram (StProgram_16,  pos, n :> stk) input
  ___@(StProgram_28, _, _) input -> \end -> Left (currentPos input end, ["(", "<Name>", "<name>", "<number>"])
  ___@(StProgram_29, _, es :> (_, _, _ :> (_, _, e :> stk@(_, pos, _)))) input@((_, ")") : _) -> gotoExprs1ForProgram stk (e : es) input
  ___@(StProgram_29, _, _) input -> \end -> Left (currentPos input end, [")"])
  stk@(StProgram_30, _, _) ((pos, ")") : input) -> actionForProgram (StProgram_31,  pos, () :> stk) input
  ___@(StProgram_30, _, _) input -> \end -> Left (currentPos input end, [")"])
  ___@(StProgram_31, _, _ :> (_, _, es :> (_, _, _ :> stk@(_, pos, _)))) input@((_, ",") : _) -> gotoTupleForProgram stk (es) input
  ___@(StProgram_31, _, _ :> (_, _, es :> (_, _, _ :> stk@(_, pos, _)))) input@((_, "->") : _) -> gotoTupleForProgram stk (es) input
  ___@(StProgram_31, _, _ :> (_, _, es :> (_, _, _ :> stk@(_, pos, _)))) input@((_, ".") : _) -> gotoTupleForProgram stk (es) input
  ___@(StProgram_31, _, _ :> (_, _, es :> (_, _, _ :> stk@(_, pos, _)))) input@((_, "<-") : _) -> gotoTupleForProgram stk (es) input
  ___@(StProgram_31, _, _ :> (_, _, es :> (_, _, _ :> stk@(_, pos, _)))) input@((_, "=>") : _) -> gotoTupleForProgram stk (es) input
  ___@(StProgram_31, _, _) input -> \end -> Left (currentPos input end, [",", "->", ".", "<-", "=>"])
  ___@(StProgram_32, _, _ :> (_, _, _ :> stk@(_, pos, _))) input@((_, ",") : _) -> gotoTupleForProgram stk ([]) input
  ___@(StProgram_32, _, _ :> (_, _, _ :> stk@(_, pos, _))) input@((_, "->") : _) -> gotoTupleForProgram stk ([]) input
  ___@(StProgram_32, _, _ :> (_, _, _ :> stk@(_, pos, _))) input@((_, ".") : _) -> gotoTupleForProgram stk ([]) input
  ___@(StProgram_32, _, _ :> (_, _, _ :> stk@(_, pos, _))) input@((_, "<-") : _) -> gotoTupleForProgram stk ([]) input
  ___@(StProgram_32, _, _ :> (_, _, _ :> stk@(_, pos, _))) input@((_, "=>") : _) -> gotoTupleForProgram stk ([]) input
  ___@(StProgram_32, _, _) input -> \end -> Left (currentPos input end, [",", "->", ".", "<-", "=>"])
  stk@(StProgram_33, _, _) ((pos, LowercaseName n) : input) -> actionForProgram (StProgram_35,  pos, n :> stk) input
  ___@(StProgram_33, _, _) input -> \end -> Left (currentPos input end, ["<name>"])
  ___@(StProgram_34, _, c :> (_, _, _ :> stk@(_, pos, _))) input@((_, ",") : _) -> gotoCondForProgram stk (CondRefute pos c) input
  ___@(StProgram_34, _, c :> (_, _, _ :> stk@(_, pos, _))) input@((_, ".") : _) -> gotoCondForProgram stk (CondRefute pos c) input
  ___@(StProgram_34, _, c :> (_, _, _ :> stk@(_, pos, _))) input@((_, "=>") : _) -> gotoCondForProgram stk (CondRefute pos c) input
  ___@(StProgram_34, _, _) input -> \end -> Left (currentPos input end, [",", ".", "=>"])
  stk@(StProgram_35, _, _) ((pos, "(") : input) -> actionForProgram (StProgram_26,  pos, () :> stk) input
  ___@(StProgram_35, _, _) input -> \end -> Left (currentPos input end, ["("])
  stk@(StProgram_36, _, _) ((pos, ".") : input) -> actionForProgram (StProgram_37,  pos, () :> stk) input
  stk@(StProgram_36, _, _) ((pos, "=>") : input) -> actionForProgram (StProgram_38,  pos, () :> stk) input
  ___@(StProgram_36, _, _) input -> \end -> Left (currentPos input end, [".", "=>"])
  ___@(StProgram_37, _, _ :> (_, _, cs :> (_, _, _ :> (_, _, c :> stk@(_, pos, _))))) input@[] -> gotoEffectForProgram stk (Effect pos c cs []) input
  ___@(StProgram_37, _, _ :> (_, _, cs :> (_, _, _ :> (_, _, c :> stk@(_, pos, _))))) input@((_, LowercaseName _) : _) -> gotoEffectForProgram stk (Effect pos c cs []) input
  ___@(StProgram_37, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<name>"])
  stk@(StProgram_38, _, _) ((pos, "+") : input) -> actionForProgram (StProgram_42,  pos, () :> stk) input
  stk@(StProgram_38, _, _) ((pos, "-") : input) -> actionForProgram (StProgram_44,  pos, () :> stk) input
  ___@(StProgram_38, _, _) input -> \end -> Left (currentPos input end, ["+", "-"])
  stk@(StProgram_39, _, _) ((pos, ",") : input) -> actionForProgram (StProgram_40,  pos, () :> stk) input
  ___@(StProgram_39, _, c :> stk@(_, pos, _)) input@((_, ".") : _) -> gotoChangesForProgram stk ([c]) input
  ___@(StProgram_39, _, _) input -> \end -> Left (currentPos input end, [",", "."])
  stk@(StProgram_40, _, _) ((pos, "+") : input) -> actionForProgram (StProgram_42,  pos, () :> stk) input
  stk@(StProgram_40, _, _) ((pos, "-") : input) -> actionForProgram (StProgram_44,  pos, () :> stk) input
  ___@(StProgram_40, _, _) input -> \end -> Left (currentPos input end, ["+", "-"])
  ___@(StProgram_41, _, cs :> (_, _, _ :> (_, _, c :> stk@(_, pos, _)))) input@((_, ".") : _) -> gotoChangesForProgram stk (c : cs) input
  ___@(StProgram_41, _, _) input -> \end -> Left (currentPos input end, ["."])
  stk@(StProgram_42, _, _) ((pos, LowercaseName n) : input) -> actionForProgram (StProgram_35,  pos, n :> stk) input
  ___@(StProgram_42, _, _) input -> \end -> Left (currentPos input end, ["<name>"])
  ___@(StProgram_43, _, c :> (_, _, _ :> stk@(_, pos, _))) input@((_, ",") : _) -> gotoChangeForProgram stk (Assert pos c) input
  ___@(StProgram_43, _, c :> (_, _, _ :> stk@(_, pos, _))) input@((_, ".") : _) -> gotoChangeForProgram stk (Assert pos c) input
  ___@(StProgram_43, _, _) input -> \end -> Left (currentPos input end, [",", "."])
  stk@(StProgram_44, _, _) ((pos, LowercaseName n) : input) -> actionForProgram (StProgram_35,  pos, n :> stk) input
  ___@(StProgram_44, _, _) input -> \end -> Left (currentPos input end, ["<name>"])
  ___@(StProgram_45, _, c :> (_, _, _ :> stk@(_, pos, _))) input@((_, ",") : _) -> gotoChangeForProgram stk (Refute pos c) input
  ___@(StProgram_45, _, c :> (_, _, _ :> stk@(_, pos, _))) input@((_, ".") : _) -> gotoChangeForProgram stk (Refute pos c) input
  ___@(StProgram_45, _, _) input -> \end -> Left (currentPos input end, [",", "."])
  stk@(StProgram_46, _, _) ((pos, ".") : input) -> actionForProgram (StProgram_47,  pos, () :> stk) input
  ___@(StProgram_46, _, _) input -> \end -> Left (currentPos input end, ["."])
  ___@(StProgram_47, _, _ :> (_, _, ds :> (_, _, _ :> (_, _, cs :> (_, _, _ :> (_, _, c :> stk@(_, pos, _))))))) input@[] -> gotoEffectForProgram stk (Effect pos c cs ds) input
  ___@(StProgram_47, _, _ :> (_, _, ds :> (_, _, _ :> (_, _, cs :> (_, _, _ :> (_, _, c :> stk@(_, pos, _))))))) input@((_, LowercaseName _) : _) -> gotoEffectForProgram stk (Effect pos c cs ds) input
  ___@(StProgram_47, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<name>"])
  ___@(StProgram_48, _, _ :> (_, _, c :> stk@(_, pos, _))) input@[] -> gotoClauseForProgram stk (Clause pos c []) input
  ___@(StProgram_48, _, _ :> (_, _, c :> stk@(_, pos, _))) input@((_, LowercaseName _) : _) -> gotoClauseForProgram stk (Clause pos c []) input
  ___@(StProgram_48, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<name>"])
  stk@(StProgram_49, _, _) ((pos, "(") : input) -> actionForProgram (StProgram_9,  pos, () :> stk) input
  stk@(StProgram_49, _, _) ((pos, UppercaseName n) : input) -> actionForProgram (StProgram_14,  pos, n :> stk) input
  stk@(StProgram_49, _, _) ((pos, LowercaseName n) : input) -> actionForProgram (StProgram_24,  pos, n :> stk) input
  stk@(StProgram_49, _, _) ((pos, NumberLiteral n) : input) -> actionForProgram (StProgram_16,  pos, n :> stk) input
  stk@(StProgram_49, _, _) ((pos, "~") : input) -> actionForProgram (StProgram_33,  pos, () :> stk) input
  ___@(StProgram_49, _, _) input -> \end -> Left (currentPos input end, ["(", "<Name>", "<name>", "<number>", "~"])
  stk@(StProgram_50, _, _) ((pos, ".") : input) -> actionForProgram (StProgram_51,  pos, () :> stk) input
  ___@(StProgram_50, _, _) input -> \end -> Left (currentPos input end, ["."])
  ___@(StProgram_51, _, _ :> (_, _, cs :> (_, _, _ :> (_, _, c :> stk@(_, pos, _))))) input@[] -> gotoClauseForProgram stk (Clause pos c cs) input
  ___@(StProgram_51, _, _ :> (_, _, cs :> (_, _, _ :> (_, _, c :> stk@(_, pos, _))))) input@((_, LowercaseName _) : _) -> gotoClauseForProgram stk (Clause pos c cs) input
  ___@(StProgram_51, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<name>"])
  stk@(StProgram_52, _, _) ((pos, "+") : input) -> actionForProgram (StProgram_42,  pos, () :> stk) input
  stk@(StProgram_52, _, _) ((pos, "-") : input) -> actionForProgram (StProgram_44,  pos, () :> stk) input
  ___@(StProgram_52, _, _) input -> \end -> Left (currentPos input end, ["+", "-"])
  stk@(StProgram_53, _, _) ((pos, ".") : input) -> actionForProgram (StProgram_54,  pos, () :> stk) input
  ___@(StProgram_53, _, _) input -> \end -> Left (currentPos input end, ["."])
  ___@(StProgram_54, _, _ :> (_, _, ds :> (_, _, _ :> (_, _, c :> stk@(_, pos, _))))) input@[] -> gotoEffectForProgram stk (Effect pos c [] ds) input
  ___@(StProgram_54, _, _ :> (_, _, ds :> (_, _, _ :> (_, _, c :> stk@(_, pos, _))))) input@((_, LowercaseName _) : _) -> gotoEffectForProgram stk (Effect pos c [] ds) input
  ___@(StProgram_54, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<name>"])
  ___@(StProgram_55, _, c :> stk@(_, pos, _)) input@[] -> gotoStmtForProgram stk (StmtClause pos c) input
  ___@(StProgram_55, _, c :> stk@(_, pos, _)) input@((_, LowercaseName _) : _) -> gotoStmtForProgram stk (StmtClause pos c) input
  ___@(StProgram_55, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<name>"])
  ___@(StProgram_56, _, e :> stk@(_, pos, _)) input@[] -> gotoStmtForProgram stk (StmtEffect pos e) input
  ___@(StProgram_56, _, e :> stk@(_, pos, _)) input@((_, LowercaseName _) : _) -> gotoStmtForProgram stk (StmtEffect pos e) input
  ___@(StProgram_56, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<name>"])
  ___@(StProgram_57, _, e :> _) [] -> \_ -> Right e
  ___@(StProgram_57, _, _) input -> \end -> Left (currentPos input end, ["EOF"])
  ___@(StProgram_58, _, c :> stk@(_, pos, _)) input@[] -> gotoStmtsForProgram stk ([c]) input
  stk@(StProgram_58, _, _) ((pos, LowercaseName n) : input) -> actionForProgram (StProgram_35,  pos, n :> stk) input
  ___@(StProgram_58, _, _) input -> \end -> Left (currentPos input end, ["EOF", "<name>"])
  ___@(StProgram_59, _, cs :> (_, _, c :> stk@(_, pos, _))) input@[] -> gotoStmtsForProgram stk (c : cs) input
  ___@(StProgram_59, _, _) input -> \end -> Left (currentPos input end, ["EOF"])
  ___@(StProgram_60, _, stmts :> stk@(_, pos, _)) input@[] -> gotoProgramForProgram stk (Program {stmts}) input
  ___@(StProgram_60, _, _) input -> \end -> Left (currentPos input end, ["EOF"])

parseProgram :: FilePath -> IO (Either LexerError (Either (Pos, [String]) (Program)))
parseProgram filepath = do
  text <- Text.readFile filepath
  case lexText filepath text ["(", ")", "*", "+", ",", "-", "->", ".", "<-", "<Name>", "<name>", "<number>", "=", "=>", "expect", "guard", "notify", "test", "~"] of
    Left  err   -> pure (Left err)
    Right (input, end) -> 
      pure (Right (actionForProgram (StProgram_0, startPos filepath text, Nil) input end))


data StTestSuite xs where
  StTestSuite_0 :: StTestSuite (a)
  StTestSuite_1 :: StTestSuite (TestSuite : a)
  StTestSuite_2 :: StTestSuite (() : a)
  StTestSuite_3 :: StTestSuite (Test : a)
  StTestSuite_4 :: StTestSuite ([Test] : Test : a)
  StTestSuite_5 :: StTestSuite (() : a)
  StTestSuite_6 :: StTestSuite (Call : () : a)
  StTestSuite_7 :: StTestSuite (Text : a)
  StTestSuite_8 :: StTestSuite ([Expr] : Text : a)
  StTestSuite_9 :: StTestSuite (() : a)
  StTestSuite_10 :: StTestSuite (Expr : a)
  StTestSuite_11 :: StTestSuite (() : Expr : a)
  StTestSuite_12 :: StTestSuite (Const : a)
  StTestSuite_13 :: StTestSuite (Expr : () : Expr : a)
  StTestSuite_14 :: StTestSuite (() : Expr : a)
  StTestSuite_15 :: StTestSuite (Expr : () : Expr : a)
  StTestSuite_16 :: StTestSuite (() : a)
  StTestSuite_17 :: StTestSuite (Expr : () : a)
  StTestSuite_18 :: StTestSuite (() : Expr : () : a)
  StTestSuite_19 :: StTestSuite (Expr : a)
  StTestSuite_20 :: StTestSuite (Expr : a)
  StTestSuite_21 :: StTestSuite (Text : a)
  StTestSuite_22 :: StTestSuite (Text : a)
  StTestSuite_23 :: StTestSuite (Integer : a)
  StTestSuite_24 :: StTestSuite (() : Expr : a)
  StTestSuite_25 :: StTestSuite (Expr : () : Expr : a)
  StTestSuite_26 :: StTestSuite (Expr : a)
  StTestSuite_27 :: StTestSuite (() : Expr : a)
  StTestSuite_28 :: StTestSuite ([Expr] : () : Expr : a)
  StTestSuite_29 :: StTestSuite ([Expr] : () : a)
  StTestSuite_30 :: StTestSuite (() : [Expr] : () : a)
  StTestSuite_31 :: StTestSuite (() : () : a)
  StTestSuite_32 :: StTestSuite (() : a)
  StTestSuite_33 :: StTestSuite (Expr : () : a)
  StTestSuite_34 :: StTestSuite (() : a)
  StTestSuite_35 :: StTestSuite (Call : () : a)
  StTestSuite_36 :: StTestSuite ([Test] : () : a)

gotoAddForTestSuite :: Stack StTestSuite xs -> (Expr) -> [Lexeme] -> Pos -> Either (Pos, [String]) (TestSuite)
gotoAddForTestSuite stack@(state, pos, _) parsed = case state of
  StTestSuite_9 -> actionForTestSuite(StTestSuite_10, pos, (parsed :> stack))
  StTestSuite_16 -> actionForTestSuite(StTestSuite_10, pos, (parsed :> stack))
  StTestSuite_24 -> actionForTestSuite(StTestSuite_25, pos, (parsed :> stack))
  StTestSuite_27 -> actionForTestSuite(StTestSuite_10, pos, (parsed :> stack))
  StTestSuite_32 -> actionForTestSuite(StTestSuite_10, pos, (parsed :> stack))
  _ -> error ""

gotoCallForTestSuite :: Stack StTestSuite xs -> (Call) -> [Lexeme] -> Pos -> Either (Pos, [String]) (TestSuite)
gotoCallForTestSuite stack@(state, pos, _) parsed = case state of
  StTestSuite_5 -> actionForTestSuite(StTestSuite_6, pos, (parsed :> stack))
  StTestSuite_34 -> actionForTestSuite(StTestSuite_35, pos, (parsed :> stack))
  _ -> error ""

gotoChangeForTestSuite :: Stack StTestSuite xs -> (Change) -> [Lexeme] -> Pos -> Either (Pos, [String]) (TestSuite)
gotoChangeForTestSuite stack@(state, pos, _) parsed = case state of
  _ -> error ""

gotoChangesForTestSuite :: Stack StTestSuite xs -> ([Change]) -> [Lexeme] -> Pos -> Either (Pos, [String]) (TestSuite)
gotoChangesForTestSuite stack@(state, pos, _) parsed = case state of
  _ -> error ""

gotoClauseForTestSuite :: Stack StTestSuite xs -> (Clause) -> [Lexeme] -> Pos -> Either (Pos, [String]) (TestSuite)
gotoClauseForTestSuite stack@(state, pos, _) parsed = case state of
  _ -> error ""

gotoCondForTestSuite :: Stack StTestSuite xs -> (Cond) -> [Lexeme] -> Pos -> Either (Pos, [String]) (TestSuite)
gotoCondForTestSuite stack@(state, pos, _) parsed = case state of
  _ -> error ""

gotoCondsForTestSuite :: Stack StTestSuite xs -> ([Cond]) -> [Lexeme] -> Pos -> Either (Pos, [String]) (TestSuite)
gotoCondsForTestSuite stack@(state, pos, _) parsed = case state of
  _ -> error ""

gotoConstForTestSuite :: Stack StTestSuite xs -> (Const) -> [Lexeme] -> Pos -> Either (Pos, [String]) (TestSuite)
gotoConstForTestSuite stack@(state, pos, _) parsed = case state of
  StTestSuite_9 -> actionForTestSuite(StTestSuite_12, pos, (parsed :> stack))
  StTestSuite_11 -> actionForTestSuite(StTestSuite_12, pos, (parsed :> stack))
  StTestSuite_14 -> actionForTestSuite(StTestSuite_12, pos, (parsed :> stack))
  StTestSuite_16 -> actionForTestSuite(StTestSuite_12, pos, (parsed :> stack))
  StTestSuite_24 -> actionForTestSuite(StTestSuite_12, pos, (parsed :> stack))
  StTestSuite_27 -> actionForTestSuite(StTestSuite_12, pos, (parsed :> stack))
  StTestSuite_32 -> actionForTestSuite(StTestSuite_12, pos, (parsed :> stack))
  _ -> error ""

gotoEffectForTestSuite :: Stack StTestSuite xs -> (Effect) -> [Lexeme] -> Pos -> Either (Pos, [String]) (TestSuite)
gotoEffectForTestSuite stack@(state, pos, _) parsed = case state of
  _ -> error ""

gotoExprForTestSuite :: Stack StTestSuite xs -> (Expr) -> [Lexeme] -> Pos -> Either (Pos, [String]) (TestSuite)
gotoExprForTestSuite stack@(state, pos, _) parsed = case state of
  StTestSuite_9 -> actionForTestSuite(StTestSuite_26, pos, (parsed :> stack))
  StTestSuite_16 -> actionForTestSuite(StTestSuite_17, pos, (parsed :> stack))
  StTestSuite_27 -> actionForTestSuite(StTestSuite_26, pos, (parsed :> stack))
  StTestSuite_32 -> actionForTestSuite(StTestSuite_33, pos, (parsed :> stack))
  _ -> error ""

gotoExprs1ForTestSuite :: Stack StTestSuite xs -> ([Expr]) -> [Lexeme] -> Pos -> Either (Pos, [String]) (TestSuite)
gotoExprs1ForTestSuite stack@(state, pos, _) parsed = case state of
  StTestSuite_9 -> actionForTestSuite(StTestSuite_29, pos, (parsed :> stack))
  StTestSuite_27 -> actionForTestSuite(StTestSuite_28, pos, (parsed :> stack))
  _ -> error ""

gotoMultForTestSuite :: Stack StTestSuite xs -> (Expr) -> [Lexeme] -> Pos -> Either (Pos, [String]) (TestSuite)
gotoMultForTestSuite stack@(state, pos, _) parsed = case state of
  StTestSuite_9 -> actionForTestSuite(StTestSuite_19, pos, (parsed :> stack))
  StTestSuite_11 -> actionForTestSuite(StTestSuite_13, pos, (parsed :> stack))
  StTestSuite_16 -> actionForTestSuite(StTestSuite_19, pos, (parsed :> stack))
  StTestSuite_24 -> actionForTestSuite(StTestSuite_19, pos, (parsed :> stack))
  StTestSuite_27 -> actionForTestSuite(StTestSuite_19, pos, (parsed :> stack))
  StTestSuite_32 -> actionForTestSuite(StTestSuite_19, pos, (parsed :> stack))
  _ -> error ""

gotoProgramForTestSuite :: Stack StTestSuite xs -> (Program) -> [Lexeme] -> Pos -> Either (Pos, [String]) (TestSuite)
gotoProgramForTestSuite stack@(state, pos, _) parsed = case state of
  _ -> error ""

gotoStmtForTestSuite :: Stack StTestSuite xs -> (Stmt) -> [Lexeme] -> Pos -> Either (Pos, [String]) (TestSuite)
gotoStmtForTestSuite stack@(state, pos, _) parsed = case state of
  _ -> error ""

gotoStmtsForTestSuite :: Stack StTestSuite xs -> ([Stmt]) -> [Lexeme] -> Pos -> Either (Pos, [String]) (TestSuite)
gotoStmtsForTestSuite stack@(state, pos, _) parsed = case state of
  _ -> error ""

gotoTermForTestSuite :: Stack StTestSuite xs -> (Expr) -> [Lexeme] -> Pos -> Either (Pos, [String]) (TestSuite)
gotoTermForTestSuite stack@(state, pos, _) parsed = case state of
  StTestSuite_9 -> actionForTestSuite(StTestSuite_20, pos, (parsed :> stack))
  StTestSuite_11 -> actionForTestSuite(StTestSuite_20, pos, (parsed :> stack))
  StTestSuite_14 -> actionForTestSuite(StTestSuite_15, pos, (parsed :> stack))
  StTestSuite_16 -> actionForTestSuite(StTestSuite_20, pos, (parsed :> stack))
  StTestSuite_24 -> actionForTestSuite(StTestSuite_20, pos, (parsed :> stack))
  StTestSuite_27 -> actionForTestSuite(StTestSuite_20, pos, (parsed :> stack))
  StTestSuite_32 -> actionForTestSuite(StTestSuite_20, pos, (parsed :> stack))
  _ -> error ""

gotoTestForTestSuite :: Stack StTestSuite xs -> (Test) -> [Lexeme] -> Pos -> Either (Pos, [String]) (TestSuite)
gotoTestForTestSuite stack@(state, pos, _) parsed = case state of
  StTestSuite_2 -> actionForTestSuite(StTestSuite_3, pos, (parsed :> stack))
  StTestSuite_3 -> actionForTestSuite(StTestSuite_3, pos, (parsed :> stack))
  _ -> error ""

gotoTestSuiteForTestSuite :: Stack StTestSuite xs -> (TestSuite) -> [Lexeme] -> Pos -> Either (Pos, [String]) (TestSuite)
gotoTestSuiteForTestSuite stack@(state, pos, _) parsed = case state of
  StTestSuite_0 -> actionForTestSuite(StTestSuite_1, pos, (parsed :> stack))
  _ -> error ""

gotoTestsForTestSuite :: Stack StTestSuite xs -> ([Test]) -> [Lexeme] -> Pos -> Either (Pos, [String]) (TestSuite)
gotoTestsForTestSuite stack@(state, pos, _) parsed = case state of
  StTestSuite_2 -> actionForTestSuite(StTestSuite_36, pos, (parsed :> stack))
  StTestSuite_3 -> actionForTestSuite(StTestSuite_4, pos, (parsed :> stack))
  _ -> error ""

gotoTupleForTestSuite :: Stack StTestSuite xs -> ([Expr]) -> [Lexeme] -> Pos -> Either (Pos, [String]) (TestSuite)
gotoTupleForTestSuite stack@(state, pos, _) parsed = case state of
  StTestSuite_7 -> actionForTestSuite(StTestSuite_8, pos, (parsed :> stack))
  _ -> error ""

actionForTestSuite :: Stack StTestSuite xs -> [Lexeme] -> Pos -> Either (Pos, [String]) (TestSuite)
actionForTestSuite = \cases
  stk@(StTestSuite_0, _, _) ((pos, "test") : input) -> actionForTestSuite (StTestSuite_2,  pos, () :> stk) input
  ___@(StTestSuite_0, _, _) input -> \end -> Left (currentPos input end, ["test"])
  ___@(StTestSuite_1, _, e :> _) [] -> \_ -> Right e
  ___@(StTestSuite_1, _, _) input -> \end -> Left (currentPos input end, ["EOF"])
  stk@(StTestSuite_2, _, _) ((pos, "expect") : input) -> actionForTestSuite (StTestSuite_5,  pos, () :> stk) input
  stk@(StTestSuite_2, _, _) ((pos, "guard") : input) -> actionForTestSuite (StTestSuite_32,  pos, () :> stk) input
  stk@(StTestSuite_2, _, _) ((pos, "notify") : input) -> actionForTestSuite (StTestSuite_34,  pos, () :> stk) input
  ___@(StTestSuite_2, _, _) input -> \end -> Left (currentPos input end, ["expect", "guard", "notify"])
  ___@(StTestSuite_3, _, t :> stk@(_, pos, _)) input@[] -> gotoTestsForTestSuite stk ([t]) input
  stk@(StTestSuite_3, _, _) ((pos, "expect") : input) -> actionForTestSuite (StTestSuite_5,  pos, () :> stk) input
  stk@(StTestSuite_3, _, _) ((pos, "guard") : input) -> actionForTestSuite (StTestSuite_32,  pos, () :> stk) input
  stk@(StTestSuite_3, _, _) ((pos, "notify") : input) -> actionForTestSuite (StTestSuite_34,  pos, () :> stk) input
  ___@(StTestSuite_3, _, _) input -> \end -> Left (currentPos input end, ["EOF", "expect", "guard", "notify"])
  ___@(StTestSuite_4, _, ts :> (_, _, t :> stk@(_, pos, _))) input@[] -> gotoTestsForTestSuite stk ( t : ts) input
  ___@(StTestSuite_4, _, _) input -> \end -> Left (currentPos input end, ["EOF"])
  stk@(StTestSuite_5, _, _) ((pos, LowercaseName n) : input) -> actionForTestSuite (StTestSuite_7,  pos, n :> stk) input
  ___@(StTestSuite_5, _, _) input -> \end -> Left (currentPos input end, ["<name>"])
  ___@(StTestSuite_6, _, c :> (_, _, _ :> stk@(_, pos, _))) input@[] -> gotoTestForTestSuite stk (Expect pos c) input
  ___@(StTestSuite_6, _, c :> (_, _, _ :> stk@(_, pos, _))) input@((_, "expect") : _) -> gotoTestForTestSuite stk (Expect pos c) input
  ___@(StTestSuite_6, _, c :> (_, _, _ :> stk@(_, pos, _))) input@((_, "guard") : _) -> gotoTestForTestSuite stk (Expect pos c) input
  ___@(StTestSuite_6, _, c :> (_, _, _ :> stk@(_, pos, _))) input@((_, "notify") : _) -> gotoTestForTestSuite stk (Expect pos c) input
  ___@(StTestSuite_6, _, _) input -> \end -> Left (currentPos input end, ["EOF", "expect", "guard", "notify"])
  stk@(StTestSuite_7, _, _) ((pos, "(") : input) -> actionForTestSuite (StTestSuite_9,  pos, () :> stk) input
  ___@(StTestSuite_7, _, _) input -> \end -> Left (currentPos input end, ["("])
  ___@(StTestSuite_8, _, t :> (_, _, pre :> stk@(_, pos, _))) input@[] -> gotoCallForTestSuite stk (Call pos pre t) input
  ___@(StTestSuite_8, _, t :> (_, _, pre :> stk@(_, pos, _))) input@((_, "expect") : _) -> gotoCallForTestSuite stk (Call pos pre t) input
  ___@(StTestSuite_8, _, t :> (_, _, pre :> stk@(_, pos, _))) input@((_, "guard") : _) -> gotoCallForTestSuite stk (Call pos pre t) input
  ___@(StTestSuite_8, _, t :> (_, _, pre :> stk@(_, pos, _))) input@((_, "notify") : _) -> gotoCallForTestSuite stk (Call pos pre t) input
  ___@(StTestSuite_8, _, _) input -> \end -> Left (currentPos input end, ["EOF", "expect", "guard", "notify"])
  stk@(StTestSuite_9, _, _) ((pos, "(") : input) -> actionForTestSuite (StTestSuite_16,  pos, () :> stk) input
  stk@(StTestSuite_9, _, _) ((pos, ")") : input) -> actionForTestSuite (StTestSuite_31,  pos, () :> stk) input
  stk@(StTestSuite_9, _, _) ((pos, UppercaseName n) : input) -> actionForTestSuite (StTestSuite_21,  pos, n :> stk) input
  stk@(StTestSuite_9, _, _) ((pos, LowercaseName n) : input) -> actionForTestSuite (StTestSuite_22,  pos, n :> stk) input
  stk@(StTestSuite_9, _, _) ((pos, NumberLiteral n) : input) -> actionForTestSuite (StTestSuite_23,  pos, n :> stk) input
  ___@(StTestSuite_9, _, _) input -> \end -> Left (currentPos input end, ["(", ")", "<Name>", "<name>", "<number>"])
  ___@(StTestSuite_10, _, a :> stk@(_, pos, _)) input@[] -> gotoExprForTestSuite stk (a) input
  ___@(StTestSuite_10, _, a :> stk@(_, pos, _)) input@((_, ")") : _) -> gotoExprForTestSuite stk (a) input
  stk@(StTestSuite_10, _, _) ((pos, "+") : input) -> actionForTestSuite (StTestSuite_11,  pos, () :> stk) input
  ___@(StTestSuite_10, _, a :> stk@(_, pos, _)) input@((_, ",") : _) -> gotoExprForTestSuite stk (a) input
  stk@(StTestSuite_10, _, _) ((pos, "=") : input) -> actionForTestSuite (StTestSuite_24,  pos, () :> stk) input
  ___@(StTestSuite_10, _, a :> stk@(_, pos, _)) input@((_, "expect") : _) -> gotoExprForTestSuite stk (a) input
  ___@(StTestSuite_10, _, a :> stk@(_, pos, _)) input@((_, "guard") : _) -> gotoExprForTestSuite stk (a) input
  ___@(StTestSuite_10, _, a :> stk@(_, pos, _)) input@((_, "notify") : _) -> gotoExprForTestSuite stk (a) input
  ___@(StTestSuite_10, _, _) input -> \end -> Left (currentPos input end, ["EOF", ")", "+", ",", "=", "expect", "guard", "notify"])
  stk@(StTestSuite_11, _, _) ((pos, "(") : input) -> actionForTestSuite (StTestSuite_16,  pos, () :> stk) input
  stk@(StTestSuite_11, _, _) ((pos, UppercaseName n) : input) -> actionForTestSuite (StTestSuite_21,  pos, n :> stk) input
  stk@(StTestSuite_11, _, _) ((pos, LowercaseName n) : input) -> actionForTestSuite (StTestSuite_22,  pos, n :> stk) input
  stk@(StTestSuite_11, _, _) ((pos, NumberLiteral n) : input) -> actionForTestSuite (StTestSuite_23,  pos, n :> stk) input
  ___@(StTestSuite_11, _, _) input -> \end -> Left (currentPos input end, ["(", "<Name>", "<name>", "<number>"])
  ___@(StTestSuite_12, _, n :> stk@(_, pos, _)) input@[] -> gotoTermForTestSuite stk (ExprConst pos n) input
  ___@(StTestSuite_12, _, n :> stk@(_, pos, _)) input@((_, ")") : _) -> gotoTermForTestSuite stk (ExprConst pos n) input
  ___@(StTestSuite_12, _, n :> stk@(_, pos, _)) input@((_, "*") : _) -> gotoTermForTestSuite stk (ExprConst pos n) input
  ___@(StTestSuite_12, _, n :> stk@(_, pos, _)) input@((_, "+") : _) -> gotoTermForTestSuite stk (ExprConst pos n) input
  ___@(StTestSuite_12, _, n :> stk@(_, pos, _)) input@((_, ",") : _) -> gotoTermForTestSuite stk (ExprConst pos n) input
  ___@(StTestSuite_12, _, n :> stk@(_, pos, _)) input@((_, "=") : _) -> gotoTermForTestSuite stk (ExprConst pos n) input
  ___@(StTestSuite_12, _, n :> stk@(_, pos, _)) input@((_, "expect") : _) -> gotoTermForTestSuite stk (ExprConst pos n) input
  ___@(StTestSuite_12, _, n :> stk@(_, pos, _)) input@((_, "guard") : _) -> gotoTermForTestSuite stk (ExprConst pos n) input
  ___@(StTestSuite_12, _, n :> stk@(_, pos, _)) input@((_, "notify") : _) -> gotoTermForTestSuite stk (ExprConst pos n) input
  ___@(StTestSuite_12, _, _) input -> \end -> Left (currentPos input end, ["EOF", ")", "*", "+", ",", "=", "expect", "guard", "notify"])
  ___@(StTestSuite_13, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@[] -> gotoAddForTestSuite stk (ExprBinary pos a Add b) input
  ___@(StTestSuite_13, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, ")") : _) -> gotoAddForTestSuite stk (ExprBinary pos a Add b) input
  stk@(StTestSuite_13, _, _) ((pos, "*") : input) -> actionForTestSuite (StTestSuite_14,  pos, () :> stk) input
  ___@(StTestSuite_13, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "+") : _) -> gotoAddForTestSuite stk (ExprBinary pos a Add b) input
  ___@(StTestSuite_13, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, ",") : _) -> gotoAddForTestSuite stk (ExprBinary pos a Add b) input
  ___@(StTestSuite_13, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "=") : _) -> gotoAddForTestSuite stk (ExprBinary pos a Add b) input
  ___@(StTestSuite_13, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "expect") : _) -> gotoAddForTestSuite stk (ExprBinary pos a Add b) input
  ___@(StTestSuite_13, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "guard") : _) -> gotoAddForTestSuite stk (ExprBinary pos a Add b) input
  ___@(StTestSuite_13, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "notify") : _) -> gotoAddForTestSuite stk (ExprBinary pos a Add b) input
  ___@(StTestSuite_13, _, _) input -> \end -> Left (currentPos input end, ["EOF", ")", "*", "+", ",", "=", "expect", "guard", "notify"])
  stk@(StTestSuite_14, _, _) ((pos, "(") : input) -> actionForTestSuite (StTestSuite_16,  pos, () :> stk) input
  stk@(StTestSuite_14, _, _) ((pos, UppercaseName n) : input) -> actionForTestSuite (StTestSuite_21,  pos, n :> stk) input
  stk@(StTestSuite_14, _, _) ((pos, LowercaseName n) : input) -> actionForTestSuite (StTestSuite_22,  pos, n :> stk) input
  stk@(StTestSuite_14, _, _) ((pos, NumberLiteral n) : input) -> actionForTestSuite (StTestSuite_23,  pos, n :> stk) input
  ___@(StTestSuite_14, _, _) input -> \end -> Left (currentPos input end, ["(", "<Name>", "<name>", "<number>"])
  ___@(StTestSuite_15, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@[] -> gotoMultForTestSuite stk (ExprBinary pos a Mult b) input
  ___@(StTestSuite_15, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, ")") : _) -> gotoMultForTestSuite stk (ExprBinary pos a Mult b) input
  ___@(StTestSuite_15, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "*") : _) -> gotoMultForTestSuite stk (ExprBinary pos a Mult b) input
  ___@(StTestSuite_15, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "+") : _) -> gotoMultForTestSuite stk (ExprBinary pos a Mult b) input
  ___@(StTestSuite_15, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, ",") : _) -> gotoMultForTestSuite stk (ExprBinary pos a Mult b) input
  ___@(StTestSuite_15, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "=") : _) -> gotoMultForTestSuite stk (ExprBinary pos a Mult b) input
  ___@(StTestSuite_15, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "expect") : _) -> gotoMultForTestSuite stk (ExprBinary pos a Mult b) input
  ___@(StTestSuite_15, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "guard") : _) -> gotoMultForTestSuite stk (ExprBinary pos a Mult b) input
  ___@(StTestSuite_15, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "notify") : _) -> gotoMultForTestSuite stk (ExprBinary pos a Mult b) input
  ___@(StTestSuite_15, _, _) input -> \end -> Left (currentPos input end, ["EOF", ")", "*", "+", ",", "=", "expect", "guard", "notify"])
  stk@(StTestSuite_16, _, _) ((pos, "(") : input) -> actionForTestSuite (StTestSuite_16,  pos, () :> stk) input
  stk@(StTestSuite_16, _, _) ((pos, UppercaseName n) : input) -> actionForTestSuite (StTestSuite_21,  pos, n :> stk) input
  stk@(StTestSuite_16, _, _) ((pos, LowercaseName n) : input) -> actionForTestSuite (StTestSuite_22,  pos, n :> stk) input
  stk@(StTestSuite_16, _, _) ((pos, NumberLiteral n) : input) -> actionForTestSuite (StTestSuite_23,  pos, n :> stk) input
  ___@(StTestSuite_16, _, _) input -> \end -> Left (currentPos input end, ["(", "<Name>", "<name>", "<number>"])
  stk@(StTestSuite_17, _, _) ((pos, ")") : input) -> actionForTestSuite (StTestSuite_18,  pos, () :> stk) input
  ___@(StTestSuite_17, _, _) input -> \end -> Left (currentPos input end, [")"])
  ___@(StTestSuite_18, _, _ :> (_, _, e :> (_, _, _ :> stk@(_, pos, _)))) input@[] -> gotoTermForTestSuite stk (e) input
  ___@(StTestSuite_18, _, _ :> (_, _, e :> (_, _, _ :> stk@(_, pos, _)))) input@((_, ")") : _) -> gotoTermForTestSuite stk (e) input
  ___@(StTestSuite_18, _, _ :> (_, _, e :> (_, _, _ :> stk@(_, pos, _)))) input@((_, "*") : _) -> gotoTermForTestSuite stk (e) input
  ___@(StTestSuite_18, _, _ :> (_, _, e :> (_, _, _ :> stk@(_, pos, _)))) input@((_, "+") : _) -> gotoTermForTestSuite stk (e) input
  ___@(StTestSuite_18, _, _ :> (_, _, e :> (_, _, _ :> stk@(_, pos, _)))) input@((_, ",") : _) -> gotoTermForTestSuite stk (e) input
  ___@(StTestSuite_18, _, _ :> (_, _, e :> (_, _, _ :> stk@(_, pos, _)))) input@((_, "=") : _) -> gotoTermForTestSuite stk (e) input
  ___@(StTestSuite_18, _, _ :> (_, _, e :> (_, _, _ :> stk@(_, pos, _)))) input@((_, "expect") : _) -> gotoTermForTestSuite stk (e) input
  ___@(StTestSuite_18, _, _ :> (_, _, e :> (_, _, _ :> stk@(_, pos, _)))) input@((_, "guard") : _) -> gotoTermForTestSuite stk (e) input
  ___@(StTestSuite_18, _, _ :> (_, _, e :> (_, _, _ :> stk@(_, pos, _)))) input@((_, "notify") : _) -> gotoTermForTestSuite stk (e) input
  ___@(StTestSuite_18, _, _) input -> \end -> Left (currentPos input end, ["EOF", ")", "*", "+", ",", "=", "expect", "guard", "notify"])
  ___@(StTestSuite_19, _, a :> stk@(_, pos, _)) input@[] -> gotoAddForTestSuite stk (a) input
  ___@(StTestSuite_19, _, a :> stk@(_, pos, _)) input@((_, ")") : _) -> gotoAddForTestSuite stk (a) input
  stk@(StTestSuite_19, _, _) ((pos, "*") : input) -> actionForTestSuite (StTestSuite_14,  pos, () :> stk) input
  ___@(StTestSuite_19, _, a :> stk@(_, pos, _)) input@((_, "+") : _) -> gotoAddForTestSuite stk (a) input
  ___@(StTestSuite_19, _, a :> stk@(_, pos, _)) input@((_, ",") : _) -> gotoAddForTestSuite stk (a) input
  ___@(StTestSuite_19, _, a :> stk@(_, pos, _)) input@((_, "=") : _) -> gotoAddForTestSuite stk (a) input
  ___@(StTestSuite_19, _, a :> stk@(_, pos, _)) input@((_, "expect") : _) -> gotoAddForTestSuite stk (a) input
  ___@(StTestSuite_19, _, a :> stk@(_, pos, _)) input@((_, "guard") : _) -> gotoAddForTestSuite stk (a) input
  ___@(StTestSuite_19, _, a :> stk@(_, pos, _)) input@((_, "notify") : _) -> gotoAddForTestSuite stk (a) input
  ___@(StTestSuite_19, _, _) input -> \end -> Left (currentPos input end, ["EOF", ")", "*", "+", ",", "=", "expect", "guard", "notify"])
  ___@(StTestSuite_20, _, a :> stk@(_, pos, _)) input@[] -> gotoMultForTestSuite stk (a) input
  ___@(StTestSuite_20, _, a :> stk@(_, pos, _)) input@((_, ")") : _) -> gotoMultForTestSuite stk (a) input
  ___@(StTestSuite_20, _, a :> stk@(_, pos, _)) input@((_, "*") : _) -> gotoMultForTestSuite stk (a) input
  ___@(StTestSuite_20, _, a :> stk@(_, pos, _)) input@((_, "+") : _) -> gotoMultForTestSuite stk (a) input
  ___@(StTestSuite_20, _, a :> stk@(_, pos, _)) input@((_, ",") : _) -> gotoMultForTestSuite stk (a) input
  ___@(StTestSuite_20, _, a :> stk@(_, pos, _)) input@((_, "=") : _) -> gotoMultForTestSuite stk (a) input
  ___@(StTestSuite_20, _, a :> stk@(_, pos, _)) input@((_, "expect") : _) -> gotoMultForTestSuite stk (a) input
  ___@(StTestSuite_20, _, a :> stk@(_, pos, _)) input@((_, "guard") : _) -> gotoMultForTestSuite stk (a) input
  ___@(StTestSuite_20, _, a :> stk@(_, pos, _)) input@((_, "notify") : _) -> gotoMultForTestSuite stk (a) input
  ___@(StTestSuite_20, _, _) input -> \end -> Left (currentPos input end, ["EOF", ")", "*", "+", ",", "=", "expect", "guard", "notify"])
  ___@(StTestSuite_21, _, n :> stk@(_, pos, _)) input@[] -> gotoTermForTestSuite stk (ExprVar   pos n) input
  ___@(StTestSuite_21, _, n :> stk@(_, pos, _)) input@((_, ")") : _) -> gotoTermForTestSuite stk (ExprVar   pos n) input
  ___@(StTestSuite_21, _, n :> stk@(_, pos, _)) input@((_, "*") : _) -> gotoTermForTestSuite stk (ExprVar   pos n) input
  ___@(StTestSuite_21, _, n :> stk@(_, pos, _)) input@((_, "+") : _) -> gotoTermForTestSuite stk (ExprVar   pos n) input
  ___@(StTestSuite_21, _, n :> stk@(_, pos, _)) input@((_, ",") : _) -> gotoTermForTestSuite stk (ExprVar   pos n) input
  ___@(StTestSuite_21, _, n :> stk@(_, pos, _)) input@((_, "=") : _) -> gotoTermForTestSuite stk (ExprVar   pos n) input
  ___@(StTestSuite_21, _, n :> stk@(_, pos, _)) input@((_, "expect") : _) -> gotoTermForTestSuite stk (ExprVar   pos n) input
  ___@(StTestSuite_21, _, n :> stk@(_, pos, _)) input@((_, "guard") : _) -> gotoTermForTestSuite stk (ExprVar   pos n) input
  ___@(StTestSuite_21, _, n :> stk@(_, pos, _)) input@((_, "notify") : _) -> gotoTermForTestSuite stk (ExprVar   pos n) input
  ___@(StTestSuite_21, _, _) input -> \end -> Left (currentPos input end, ["EOF", ")", "*", "+", ",", "=", "expect", "guard", "notify"])
  ___@(StTestSuite_22, _, n :> stk@(_, pos, _)) input@[] -> gotoConstForTestSuite stk (ConstNamed pos n) input
  ___@(StTestSuite_22, _, n :> stk@(_, pos, _)) input@((_, ")") : _) -> gotoConstForTestSuite stk (ConstNamed pos n) input
  ___@(StTestSuite_22, _, n :> stk@(_, pos, _)) input@((_, "*") : _) -> gotoConstForTestSuite stk (ConstNamed pos n) input
  ___@(StTestSuite_22, _, n :> stk@(_, pos, _)) input@((_, "+") : _) -> gotoConstForTestSuite stk (ConstNamed pos n) input
  ___@(StTestSuite_22, _, n :> stk@(_, pos, _)) input@((_, ",") : _) -> gotoConstForTestSuite stk (ConstNamed pos n) input
  ___@(StTestSuite_22, _, n :> stk@(_, pos, _)) input@((_, "=") : _) -> gotoConstForTestSuite stk (ConstNamed pos n) input
  ___@(StTestSuite_22, _, n :> stk@(_, pos, _)) input@((_, "expect") : _) -> gotoConstForTestSuite stk (ConstNamed pos n) input
  ___@(StTestSuite_22, _, n :> stk@(_, pos, _)) input@((_, "guard") : _) -> gotoConstForTestSuite stk (ConstNamed pos n) input
  ___@(StTestSuite_22, _, n :> stk@(_, pos, _)) input@((_, "notify") : _) -> gotoConstForTestSuite stk (ConstNamed pos n) input
  ___@(StTestSuite_22, _, _) input -> \end -> Left (currentPos input end, ["EOF", ")", "*", "+", ",", "=", "expect", "guard", "notify"])
  ___@(StTestSuite_23, _, n :> stk@(_, pos, _)) input@[] -> gotoConstForTestSuite stk (ConstInt   pos n) input
  ___@(StTestSuite_23, _, n :> stk@(_, pos, _)) input@((_, ")") : _) -> gotoConstForTestSuite stk (ConstInt   pos n) input
  ___@(StTestSuite_23, _, n :> stk@(_, pos, _)) input@((_, "*") : _) -> gotoConstForTestSuite stk (ConstInt   pos n) input
  ___@(StTestSuite_23, _, n :> stk@(_, pos, _)) input@((_, "+") : _) -> gotoConstForTestSuite stk (ConstInt   pos n) input
  ___@(StTestSuite_23, _, n :> stk@(_, pos, _)) input@((_, ",") : _) -> gotoConstForTestSuite stk (ConstInt   pos n) input
  ___@(StTestSuite_23, _, n :> stk@(_, pos, _)) input@((_, "=") : _) -> gotoConstForTestSuite stk (ConstInt   pos n) input
  ___@(StTestSuite_23, _, n :> stk@(_, pos, _)) input@((_, "expect") : _) -> gotoConstForTestSuite stk (ConstInt   pos n) input
  ___@(StTestSuite_23, _, n :> stk@(_, pos, _)) input@((_, "guard") : _) -> gotoConstForTestSuite stk (ConstInt   pos n) input
  ___@(StTestSuite_23, _, n :> stk@(_, pos, _)) input@((_, "notify") : _) -> gotoConstForTestSuite stk (ConstInt   pos n) input
  ___@(StTestSuite_23, _, _) input -> \end -> Left (currentPos input end, ["EOF", ")", "*", "+", ",", "=", "expect", "guard", "notify"])
  stk@(StTestSuite_24, _, _) ((pos, "(") : input) -> actionForTestSuite (StTestSuite_16,  pos, () :> stk) input
  stk@(StTestSuite_24, _, _) ((pos, UppercaseName n) : input) -> actionForTestSuite (StTestSuite_21,  pos, n :> stk) input
  stk@(StTestSuite_24, _, _) ((pos, LowercaseName n) : input) -> actionForTestSuite (StTestSuite_22,  pos, n :> stk) input
  stk@(StTestSuite_24, _, _) ((pos, NumberLiteral n) : input) -> actionForTestSuite (StTestSuite_23,  pos, n :> stk) input
  ___@(StTestSuite_24, _, _) input -> \end -> Left (currentPos input end, ["(", "<Name>", "<name>", "<number>"])
  ___@(StTestSuite_25, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@[] -> gotoExprForTestSuite stk (ExprBinary pos a Equals b) input
  ___@(StTestSuite_25, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, ")") : _) -> gotoExprForTestSuite stk (ExprBinary pos a Equals b) input
  stk@(StTestSuite_25, _, _) ((pos, "+") : input) -> actionForTestSuite (StTestSuite_11,  pos, () :> stk) input
  ___@(StTestSuite_25, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, ",") : _) -> gotoExprForTestSuite stk (ExprBinary pos a Equals b) input
  ___@(StTestSuite_25, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "expect") : _) -> gotoExprForTestSuite stk (ExprBinary pos a Equals b) input
  ___@(StTestSuite_25, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "guard") : _) -> gotoExprForTestSuite stk (ExprBinary pos a Equals b) input
  ___@(StTestSuite_25, _, b :> (_, _, _ :> (_, _, a :> stk@(_, pos, _)))) input@((_, "notify") : _) -> gotoExprForTestSuite stk (ExprBinary pos a Equals b) input
  ___@(StTestSuite_25, _, _) input -> \end -> Left (currentPos input end, ["EOF", ")", "+", ",", "expect", "guard", "notify"])
  ___@(StTestSuite_26, _, e :> stk@(_, pos, _)) input@((_, ")") : _) -> gotoExprs1ForTestSuite stk ([e]) input
  stk@(StTestSuite_26, _, _) ((pos, ",") : input) -> actionForTestSuite (StTestSuite_27,  pos, () :> stk) input
  ___@(StTestSuite_26, _, _) input -> \end -> Left (currentPos input end, [")", ","])
  stk@(StTestSuite_27, _, _) ((pos, "(") : input) -> actionForTestSuite (StTestSuite_16,  pos, () :> stk) input
  stk@(StTestSuite_27, _, _) ((pos, UppercaseName n) : input) -> actionForTestSuite (StTestSuite_21,  pos, n :> stk) input
  stk@(StTestSuite_27, _, _) ((pos, LowercaseName n) : input) -> actionForTestSuite (StTestSuite_22,  pos, n :> stk) input
  stk@(StTestSuite_27, _, _) ((pos, NumberLiteral n) : input) -> actionForTestSuite (StTestSuite_23,  pos, n :> stk) input
  ___@(StTestSuite_27, _, _) input -> \end -> Left (currentPos input end, ["(", "<Name>", "<name>", "<number>"])
  ___@(StTestSuite_28, _, es :> (_, _, _ :> (_, _, e :> stk@(_, pos, _)))) input@((_, ")") : _) -> gotoExprs1ForTestSuite stk (e : es) input
  ___@(StTestSuite_28, _, _) input -> \end -> Left (currentPos input end, [")"])
  stk@(StTestSuite_29, _, _) ((pos, ")") : input) -> actionForTestSuite (StTestSuite_30,  pos, () :> stk) input
  ___@(StTestSuite_29, _, _) input -> \end -> Left (currentPos input end, [")"])
  ___@(StTestSuite_30, _, _ :> (_, _, es :> (_, _, _ :> stk@(_, pos, _)))) input@[] -> gotoTupleForTestSuite stk (es) input
  ___@(StTestSuite_30, _, _ :> (_, _, es :> (_, _, _ :> stk@(_, pos, _)))) input@((_, "expect") : _) -> gotoTupleForTestSuite stk (es) input
  ___@(StTestSuite_30, _, _ :> (_, _, es :> (_, _, _ :> stk@(_, pos, _)))) input@((_, "guard") : _) -> gotoTupleForTestSuite stk (es) input
  ___@(StTestSuite_30, _, _ :> (_, _, es :> (_, _, _ :> stk@(_, pos, _)))) input@((_, "notify") : _) -> gotoTupleForTestSuite stk (es) input
  ___@(StTestSuite_30, _, _) input -> \end -> Left (currentPos input end, ["EOF", "expect", "guard", "notify"])
  ___@(StTestSuite_31, _, _ :> (_, _, _ :> stk@(_, pos, _))) input@[] -> gotoTupleForTestSuite stk ([]) input
  ___@(StTestSuite_31, _, _ :> (_, _, _ :> stk@(_, pos, _))) input@((_, "expect") : _) -> gotoTupleForTestSuite stk ([]) input
  ___@(StTestSuite_31, _, _ :> (_, _, _ :> stk@(_, pos, _))) input@((_, "guard") : _) -> gotoTupleForTestSuite stk ([]) input
  ___@(StTestSuite_31, _, _ :> (_, _, _ :> stk@(_, pos, _))) input@((_, "notify") : _) -> gotoTupleForTestSuite stk ([]) input
  ___@(StTestSuite_31, _, _) input -> \end -> Left (currentPos input end, ["EOF", "expect", "guard", "notify"])
  stk@(StTestSuite_32, _, _) ((pos, "(") : input) -> actionForTestSuite (StTestSuite_16,  pos, () :> stk) input
  stk@(StTestSuite_32, _, _) ((pos, UppercaseName n) : input) -> actionForTestSuite (StTestSuite_21,  pos, n :> stk) input
  stk@(StTestSuite_32, _, _) ((pos, LowercaseName n) : input) -> actionForTestSuite (StTestSuite_22,  pos, n :> stk) input
  stk@(StTestSuite_32, _, _) ((pos, NumberLiteral n) : input) -> actionForTestSuite (StTestSuite_23,  pos, n :> stk) input
  ___@(StTestSuite_32, _, _) input -> \end -> Left (currentPos input end, ["(", "<Name>", "<name>", "<number>"])
  ___@(StTestSuite_33, _, e :> (_, _, _ :> stk@(_, pos, _))) input@[] -> gotoTestForTestSuite stk (Guard  pos e) input
  ___@(StTestSuite_33, _, e :> (_, _, _ :> stk@(_, pos, _))) input@((_, "expect") : _) -> gotoTestForTestSuite stk (Guard  pos e) input
  ___@(StTestSuite_33, _, e :> (_, _, _ :> stk@(_, pos, _))) input@((_, "guard") : _) -> gotoTestForTestSuite stk (Guard  pos e) input
  ___@(StTestSuite_33, _, e :> (_, _, _ :> stk@(_, pos, _))) input@((_, "notify") : _) -> gotoTestForTestSuite stk (Guard  pos e) input
  ___@(StTestSuite_33, _, _) input -> \end -> Left (currentPos input end, ["EOF", "expect", "guard", "notify"])
  stk@(StTestSuite_34, _, _) ((pos, LowercaseName n) : input) -> actionForTestSuite (StTestSuite_7,  pos, n :> stk) input
  ___@(StTestSuite_34, _, _) input -> \end -> Left (currentPos input end, ["<name>"])
  ___@(StTestSuite_35, _, c :> (_, _, _ :> stk@(_, pos, _))) input@[] -> gotoTestForTestSuite stk (Notify pos c) input
  ___@(StTestSuite_35, _, c :> (_, _, _ :> stk@(_, pos, _))) input@((_, "expect") : _) -> gotoTestForTestSuite stk (Notify pos c) input
  ___@(StTestSuite_35, _, c :> (_, _, _ :> stk@(_, pos, _))) input@((_, "guard") : _) -> gotoTestForTestSuite stk (Notify pos c) input
  ___@(StTestSuite_35, _, c :> (_, _, _ :> stk@(_, pos, _))) input@((_, "notify") : _) -> gotoTestForTestSuite stk (Notify pos c) input
  ___@(StTestSuite_35, _, _) input -> \end -> Left (currentPos input end, ["EOF", "expect", "guard", "notify"])
  ___@(StTestSuite_36, _, tests :> (_, _, _ :> stk@(_, pos, _))) input@[] -> gotoTestSuiteForTestSuite stk (TestSuite {tests}) input
  ___@(StTestSuite_36, _, _) input -> \end -> Left (currentPos input end, ["EOF"])

parseTestSuite :: FilePath -> IO (Either LexerError (Either (Pos, [String]) (TestSuite)))
parseTestSuite filepath = do
  text <- Text.readFile filepath
  case lexText filepath text ["(", ")", "*", "+", ",", "-", "->", ".", "<-", "<Name>", "<name>", "<number>", "=", "=>", "expect", "guard", "notify", "test", "~"] of
    Left  err   -> pure (Left err)
    Right (input, end) -> 
      pure (Right (actionForTestSuite (StTestSuite_0, startPos filepath text, Nil) input end))


