{-# language PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
module Parser (
    parseProgram,
    parseTestSuite
) where

import Data.Text.IO.Utf8 qualified as Text
import Data.Kind qualified as Kind
import Text.Lexer.Default
import Data.Text.Position (Pos)
import Data.Lexeme
import AST
import Data.Text (Text)

type Stack  st xs = (st xs, Pos, Stack' st xs)
data Stack' st xs where
  Nil  ::                     Stack' st '[]
  (:>) :: x -> Stack st xs -> Stack' st (x : xs)


data StProgram :: [Kind.Type] -> Kind.Type where
  SProgram0 :: StProgram (Expr : a)
  SProgram1 :: StProgram (Expr : a)
  SProgram2 :: StProgram (Expr : a)
  SProgram3 :: StProgram (Expr : () : Expr : a)
  SProgram4 :: StProgram (Expr : () : Expr : a)
  SProgram5 :: StProgram (Expr : a)
  SProgram6 :: StProgram (Expr : a)
  SProgram7 :: StProgram (Expr : a)
  SProgram8 :: StProgram (Expr : a)
  SProgram9 :: StProgram (Expr : () : Expr : a)
  SProgram10 :: StProgram (Expr : () : Expr : a)
  SProgram11 :: StProgram (Expr : () : Expr : a)
  SProgram12 :: StProgram (Expr : () : Expr : a)
  SProgram13 :: StProgram (Expr : a)
  SProgram14 :: StProgram (Expr : a)
  SProgram15 :: StProgram (Expr : a)
  SProgram16 :: StProgram (Expr : a)
  SProgram17 :: StProgram (() : a)
  SProgram18 :: StProgram (() : a)
  SProgram19 :: StProgram (() : a)
  SProgram20 :: StProgram (() : a)
  SProgram21 :: StProgram (Text : a)
  SProgram22 :: StProgram (Text : a)
  SProgram23 :: StProgram (Text : a)
  SProgram24 :: StProgram (Text : a)
  SProgram25 :: StProgram (Const : a)
  SProgram26 :: StProgram (Const : a)
  SProgram27 :: StProgram (Const : a)
  SProgram28 :: StProgram (Const : a)
  SProgram29 :: StProgram (Text : a)
  SProgram30 :: StProgram (Text : a)
  SProgram31 :: StProgram (Text : a)
  SProgram32 :: StProgram (Text : a)
  SProgram33 :: StProgram (Integer : a)
  SProgram34 :: StProgram (Integer : a)
  SProgram35 :: StProgram (Integer : a)
  SProgram36 :: StProgram (Integer : a)
  SProgram37 :: StProgram (() : Expr : a)
  SProgram38 :: StProgram (() : Expr : a)
  SProgram39 :: StProgram (() : Expr : a)
  SProgram40 :: StProgram (() : Expr : a)
  SProgram41 :: StProgram (() : Expr : a)
  SProgram42 :: StProgram (() : Expr : a)
  SProgram43 :: StProgram (() : Expr : a)
  SProgram44 :: StProgram (() : Expr : a)
  SProgram45 :: StProgram (() : Expr : a)
  SProgram46 :: StProgram (() : Expr : a)
  SProgram47 :: StProgram (() : Expr : a)
  SProgram48 :: StProgram (Expr : () : a)
  SProgram49 :: StProgram (Expr : () : a)
  SProgram50 :: StProgram (Expr : () : a)
  SProgram51 :: StProgram (Expr : () : a)
  SProgram52 :: StProgram ([Expr] : () : Expr : a)
  SProgram53 :: StProgram (Expr : () : Expr : a)
  SProgram54 :: StProgram (Expr : () : Expr : a)
  SProgram55 :: StProgram (Expr : () : Expr : a)
  SProgram56 :: StProgram (Expr : () : Expr : a)
  SProgram57 :: StProgram (() : Expr : () : a)
  SProgram58 :: StProgram (() : Expr : () : a)
  SProgram59 :: StProgram (() : Expr : () : a)
  SProgram60 :: StProgram (() : Expr : () : a)
  SProgram61 :: StProgram (Expr : a)
  SProgram62 :: StProgram (Expr : a)
  SProgram63 :: StProgram (Expr : a)
  SProgram64 :: StProgram (Expr : a)
  SProgram65 :: StProgram (Expr : () : Expr : a)
  SProgram66 :: StProgram (Expr : () : Expr : a)
  SProgram67 :: StProgram (Expr : () : Expr : a)
  SProgram68 :: StProgram (Expr : () : Expr : a)
  SProgram69 :: StProgram (Expr : a)
  SProgram70 :: StProgram (Expr : a)
  SProgram71 :: StProgram (Expr : a)
  SProgram72 :: StProgram (Expr : a)
  SProgram73 :: StProgram (() : a)
  SProgram74 :: StProgram (() : a)
  SProgram75 :: StProgram (() : a)
  SProgram76 :: StProgram (() : a)
  SProgram77 :: StProgram (Text : a)
  SProgram78 :: StProgram (Text : a)
  SProgram79 :: StProgram (Text : a)
  SProgram80 :: StProgram (Text : a)
  SProgram81 :: StProgram (Const : a)
  SProgram82 :: StProgram (Const : a)
  SProgram83 :: StProgram (Const : a)
  SProgram84 :: StProgram (Const : a)
  SProgram85 :: StProgram (Text : a)
  SProgram86 :: StProgram (Text : a)
  SProgram87 :: StProgram (Text : a)
  SProgram88 :: StProgram (Text : a)
  SProgram89 :: StProgram (Text : a)
  SProgram90 :: StProgram (Text : a)
  SProgram91 :: StProgram (Integer : a)
  SProgram92 :: StProgram (Integer : a)
  SProgram93 :: StProgram (Integer : a)
  SProgram94 :: StProgram (Integer : a)
  SProgram95 :: StProgram (() : Expr : a)
  SProgram96 :: StProgram (() : Expr : a)
  SProgram97 :: StProgram (() : Expr : a)
  SProgram98 :: StProgram (() : Expr : a)
  SProgram99 :: StProgram (Expr : () : a)
  SProgram100 :: StProgram (Expr : () : a)
  SProgram101 :: StProgram (Expr : () : a)
  SProgram102 :: StProgram (Expr : () : a)
  SProgram103 :: StProgram (Expr : () : Expr : a)
  SProgram104 :: StProgram (Expr : () : Expr : a)
  SProgram105 :: StProgram (Expr : () : Expr : a)
  SProgram106 :: StProgram (Expr : () : Expr : a)
  SProgram107 :: StProgram (() : Expr : () : a)
  SProgram108 :: StProgram (() : Expr : () : a)
  SProgram109 :: StProgram (() : Expr : () : a)
  SProgram110 :: StProgram (() : Expr : () : a)
  SProgram111 :: StProgram (Expr : a)
  SProgram112 :: StProgram (Expr : a)
  SProgram113 :: StProgram (Expr : () : Expr : a)
  SProgram114 :: StProgram (Expr : () : Expr : a)
  SProgram115 :: StProgram (() : Expr : a)
  SProgram116 :: StProgram (() : Expr : a)
  SProgram117 :: StProgram (() : Expr : a)
  SProgram118 :: StProgram (() : Expr : a)
  SProgram119 :: StProgram (() : a)
  SProgram120 :: StProgram (() : a)
  SProgram121 :: StProgram (Call : a)
  SProgram122 :: StProgram (Call : a)
  SProgram123 :: StProgram (() : a)
  SProgram124 :: StProgram (() : a)
  SProgram125 :: StProgram (Expr : a)
  SProgram126 :: StProgram (Expr : a)
  SProgram127 :: StProgram (Text : a)
  SProgram128 :: StProgram (Text : a)
  SProgram129 :: StProgram (() : a)
  SProgram130 :: StProgram (() : a)
  SProgram131 :: StProgram (Call : () : a)
  SProgram132 :: StProgram (Call : () : a)
  SProgram133 :: StProgram (Call : () : a)
  SProgram134 :: StProgram (Call : () : a)
  SProgram135 :: StProgram ([Expr] : Text : a)
  SProgram136 :: StProgram ([Expr] : Text : a)
  SProgram137 :: StProgram (() : () : a)
  SProgram138 :: StProgram (() : () : a)
  SProgram139 :: StProgram ([Expr] : () : a)
  SProgram140 :: StProgram ([Expr] : () : a)
  SProgram141 :: StProgram (() : Expr : a)
  SProgram142 :: StProgram (() : Expr : a)
  SProgram143 :: StProgram (() : [Expr] : () : a)
  SProgram144 :: StProgram (() : [Expr] : () : a)
  SProgram145 :: StProgram (Text : a)
  SProgram146 :: StProgram (() : a)
  SProgram147 :: StProgram ([Expr] : Text : a)
  SProgram148 :: StProgram (() : () : a)
  SProgram149 :: StProgram ([Expr] : () : a)
  SProgram150 :: StProgram (() : [Expr] : () : a)
  SProgram151 :: StProgram (Change : a)
  SProgram152 :: StProgram (Cond : a)
  SProgram153 :: StProgram (Cond : a)
  SProgram154 :: StProgram (() : Change : a)
  SProgram155 :: StProgram (() : Cond : a)
  SProgram156 :: StProgram (() : Cond : a)
  SProgram157 :: StProgram ([Change] : () : Change : a)
  SProgram158 :: StProgram ([Cond] : () : Cond : a)
  SProgram159 :: StProgram ([Cond] : () : Cond : a)
  SProgram160 :: StProgram (Clause : a)
  SProgram161 :: StProgram (Effect : a)
  SProgram162 :: StProgram (Call : a)
  SProgram163 :: StProgram (() : Call : a)
  SProgram164 :: StProgram (() : Call : a)
  SProgram165 :: StProgram (() : Call : a)
  SProgram166 :: StProgram (() : Call : a)
  SProgram167 :: StProgram ([Change] : () : Call : a)
  SProgram168 :: StProgram ([Cond] : () : Call : a)
  SProgram169 :: StProgram ([Cond] : () : Call : a)
  SProgram170 :: StProgram (() : [Change] : () : Call : a)
  SProgram171 :: StProgram (() : [Cond] : () : Call : a)
  SProgram172 :: StProgram (() : [Cond] : () : Call : a)
  SProgram173 :: StProgram (() : [Cond] : () : Call : a)
  SProgram174 :: StProgram ([Change] : () : [Cond] : () : Call : a)
  SProgram175 :: StProgram (() : [Change] : () : [Cond] : () : Call :
                            a)
  SProgram176 :: StProgram (a)
  SProgram177 :: StProgram (Program : a)
  SProgram178 :: StProgram ([Stmt] : a)
  SProgram179 :: StProgram (Stmt : a)
  SProgram180 :: StProgram ([Stmt] : Stmt : a)

__gotoAddForProgram :: ([Lexeme], Pos) -> Expr -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoAddForProgram toks term stk@(state, _, _) = case state of
  SProgram17 -> __runProgram SProgram2 toks (term :> stk)
  SProgram18 -> __runProgram SProgram2 toks (term :> stk)
  SProgram19 -> __runProgram SProgram2 toks (term :> stk)
  SProgram20 -> __runProgram SProgram2 toks (term :> stk)
  SProgram37 -> __runProgram SProgram1 toks (term :> stk)
  SProgram38 -> __runProgram SProgram3 toks (term :> stk)
  SProgram39 -> __runProgram SProgram4 toks (term :> stk)
  SProgram73 -> __runProgram SProgram2 toks (term :> stk)
  SProgram74 -> __runProgram SProgram2 toks (term :> stk)
  SProgram75 -> __runProgram SProgram2 toks (term :> stk)
  SProgram76 -> __runProgram SProgram2 toks (term :> stk)
  SProgram129 -> __runProgram SProgram1 toks (term :> stk)
  SProgram130 -> __runProgram SProgram1 toks (term :> stk)
  SProgram141 -> __runProgram SProgram113 toks (term :> stk)
  SProgram142 -> __runProgram SProgram114 toks (term :> stk)
  SProgram146 -> __runProgram SProgram1 toks (term :> stk)
  SProgram155 -> __runProgram SProgram111 toks (term :> stk)
  SProgram156 -> __runProgram SProgram112 toks (term :> stk)
  SProgram164 -> __runProgram SProgram112 toks (term :> stk)
  SProgram166 -> __runProgram SProgram111 toks (term :> stk)
  _ -> error ""

__gotoCallForProgram :: ([Lexeme], Pos) -> Call -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoCallForProgram toks term stk@(state, _, _) = case state of
  SProgram119 -> __runProgram SProgram131 toks (term :> stk)
  SProgram120 -> __runProgram SProgram132 toks (term :> stk)
  SProgram123 -> __runProgram SProgram133 toks (term :> stk)
  SProgram124 -> __runProgram SProgram134 toks (term :> stk)
  SProgram155 -> __runProgram SProgram121 toks (term :> stk)
  SProgram156 -> __runProgram SProgram122 toks (term :> stk)
  SProgram164 -> __runProgram SProgram122 toks (term :> stk)
  SProgram166 -> __runProgram SProgram121 toks (term :> stk)
  SProgram176 -> __runProgram SProgram162 toks (term :> stk)
  SProgram179 -> __runProgram SProgram162 toks (term :> stk)
  _ -> error ""

__gotoChangeForProgram :: ([Lexeme], Pos) -> Change -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoChangeForProgram toks term stk@(state, _, _) = case state of
  SProgram154 -> __runProgram SProgram151 toks (term :> stk)
  SProgram163 -> __runProgram SProgram151 toks (term :> stk)
  SProgram171 -> __runProgram SProgram151 toks (term :> stk)
  _ -> error ""

__gotoChangesForProgram :: ([Lexeme], Pos) -> [Change] -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoChangesForProgram toks term stk@(state, _, _) = case state of
  SProgram154 -> __runProgram SProgram157 toks (term :> stk)
  SProgram163 -> __runProgram SProgram167 toks (term :> stk)
  SProgram171 -> __runProgram SProgram174 toks (term :> stk)
  _ -> error ""

__gotoClauseForProgram :: ([Lexeme], Pos) -> Clause -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoClauseForProgram toks term stk@(state, _, _) = case state of
  SProgram176 -> __runProgram SProgram160 toks (term :> stk)
  SProgram179 -> __runProgram SProgram160 toks (term :> stk)
  _ -> error ""

__gotoCondForProgram :: ([Lexeme], Pos) -> Cond -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoCondForProgram toks term stk@(state, _, _) = case state of
  SProgram155 -> __runProgram SProgram152 toks (term :> stk)
  SProgram156 -> __runProgram SProgram153 toks (term :> stk)
  SProgram164 -> __runProgram SProgram153 toks (term :> stk)
  SProgram166 -> __runProgram SProgram152 toks (term :> stk)
  _ -> error ""

__gotoCondsForProgram :: ([Lexeme], Pos) -> [Cond] -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoCondsForProgram toks term stk@(state, _, _) = case state of
  SProgram155 -> __runProgram SProgram158 toks (term :> stk)
  SProgram156 -> __runProgram SProgram159 toks (term :> stk)
  SProgram164 -> __runProgram SProgram168 toks (term :> stk)
  SProgram166 -> __runProgram SProgram169 toks (term :> stk)
  _ -> error ""

__gotoConstForProgram :: ([Lexeme], Pos) -> Const -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoConstForProgram toks term stk@(state, _, _) = case state of
  SProgram17 -> __runProgram SProgram28 toks (term :> stk)
  SProgram18 -> __runProgram SProgram28 toks (term :> stk)
  SProgram19 -> __runProgram SProgram28 toks (term :> stk)
  SProgram20 -> __runProgram SProgram28 toks (term :> stk)
  SProgram37 -> __runProgram SProgram27 toks (term :> stk)
  SProgram38 -> __runProgram SProgram25 toks (term :> stk)
  SProgram39 -> __runProgram SProgram26 toks (term :> stk)
  SProgram40 -> __runProgram SProgram25 toks (term :> stk)
  SProgram41 -> __runProgram SProgram26 toks (term :> stk)
  SProgram42 -> __runProgram SProgram27 toks (term :> stk)
  SProgram43 -> __runProgram SProgram28 toks (term :> stk)
  SProgram44 -> __runProgram SProgram25 toks (term :> stk)
  SProgram45 -> __runProgram SProgram26 toks (term :> stk)
  SProgram46 -> __runProgram SProgram27 toks (term :> stk)
  SProgram47 -> __runProgram SProgram28 toks (term :> stk)
  SProgram73 -> __runProgram SProgram28 toks (term :> stk)
  SProgram74 -> __runProgram SProgram28 toks (term :> stk)
  SProgram75 -> __runProgram SProgram28 toks (term :> stk)
  SProgram76 -> __runProgram SProgram28 toks (term :> stk)
  SProgram95 -> __runProgram SProgram81 toks (term :> stk)
  SProgram96 -> __runProgram SProgram82 toks (term :> stk)
  SProgram97 -> __runProgram SProgram83 toks (term :> stk)
  SProgram98 -> __runProgram SProgram84 toks (term :> stk)
  SProgram115 -> __runProgram SProgram81 toks (term :> stk)
  SProgram116 -> __runProgram SProgram82 toks (term :> stk)
  SProgram117 -> __runProgram SProgram83 toks (term :> stk)
  SProgram118 -> __runProgram SProgram84 toks (term :> stk)
  SProgram129 -> __runProgram SProgram27 toks (term :> stk)
  SProgram130 -> __runProgram SProgram27 toks (term :> stk)
  SProgram141 -> __runProgram SProgram81 toks (term :> stk)
  SProgram142 -> __runProgram SProgram84 toks (term :> stk)
  SProgram146 -> __runProgram SProgram27 toks (term :> stk)
  SProgram155 -> __runProgram SProgram82 toks (term :> stk)
  SProgram156 -> __runProgram SProgram83 toks (term :> stk)
  SProgram164 -> __runProgram SProgram83 toks (term :> stk)
  SProgram166 -> __runProgram SProgram82 toks (term :> stk)
  _ -> error ""

__gotoEffectForProgram :: ([Lexeme], Pos) -> Effect -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoEffectForProgram toks term stk@(state, _, _) = case state of
  SProgram176 -> __runProgram SProgram161 toks (term :> stk)
  SProgram179 -> __runProgram SProgram161 toks (term :> stk)
  _ -> error ""

__gotoExprForProgram :: ([Lexeme], Pos) -> Expr -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoExprForProgram toks term stk@(state, _, _) = case state of
  SProgram17 -> __runProgram SProgram48 toks (term :> stk)
  SProgram18 -> __runProgram SProgram49 toks (term :> stk)
  SProgram19 -> __runProgram SProgram50 toks (term :> stk)
  SProgram20 -> __runProgram SProgram51 toks (term :> stk)
  SProgram37 -> __runProgram SProgram0 toks (term :> stk)
  SProgram73 -> __runProgram SProgram99 toks (term :> stk)
  SProgram74 -> __runProgram SProgram100 toks (term :> stk)
  SProgram75 -> __runProgram SProgram101 toks (term :> stk)
  SProgram76 -> __runProgram SProgram102 toks (term :> stk)
  SProgram129 -> __runProgram SProgram0 toks (term :> stk)
  SProgram130 -> __runProgram SProgram0 toks (term :> stk)
  SProgram146 -> __runProgram SProgram0 toks (term :> stk)
  SProgram155 -> __runProgram SProgram125 toks (term :> stk)
  SProgram156 -> __runProgram SProgram126 toks (term :> stk)
  SProgram164 -> __runProgram SProgram126 toks (term :> stk)
  SProgram166 -> __runProgram SProgram125 toks (term :> stk)
  _ -> error ""

__gotoExprs1ForProgram :: ([Lexeme], Pos) -> [Expr] -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoExprs1ForProgram toks term stk@(state, _, _) = case state of
  SProgram37 -> __runProgram SProgram52 toks (term :> stk)
  SProgram129 -> __runProgram SProgram139 toks (term :> stk)
  SProgram130 -> __runProgram SProgram140 toks (term :> stk)
  SProgram146 -> __runProgram SProgram149 toks (term :> stk)
  _ -> error ""

__gotoMultForProgram :: ([Lexeme], Pos) -> Expr -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoMultForProgram toks term stk@(state, _, _) = case state of
  SProgram17 -> __runProgram SProgram8 toks (term :> stk)
  SProgram18 -> __runProgram SProgram8 toks (term :> stk)
  SProgram19 -> __runProgram SProgram8 toks (term :> stk)
  SProgram20 -> __runProgram SProgram8 toks (term :> stk)
  SProgram37 -> __runProgram SProgram7 toks (term :> stk)
  SProgram38 -> __runProgram SProgram5 toks (term :> stk)
  SProgram39 -> __runProgram SProgram6 toks (term :> stk)
  SProgram40 -> __runProgram SProgram9 toks (term :> stk)
  SProgram41 -> __runProgram SProgram10 toks (term :> stk)
  SProgram42 -> __runProgram SProgram11 toks (term :> stk)
  SProgram43 -> __runProgram SProgram12 toks (term :> stk)
  SProgram73 -> __runProgram SProgram8 toks (term :> stk)
  SProgram74 -> __runProgram SProgram8 toks (term :> stk)
  SProgram75 -> __runProgram SProgram8 toks (term :> stk)
  SProgram76 -> __runProgram SProgram8 toks (term :> stk)
  SProgram115 -> __runProgram SProgram65 toks (term :> stk)
  SProgram116 -> __runProgram SProgram66 toks (term :> stk)
  SProgram117 -> __runProgram SProgram67 toks (term :> stk)
  SProgram118 -> __runProgram SProgram68 toks (term :> stk)
  SProgram129 -> __runProgram SProgram7 toks (term :> stk)
  SProgram130 -> __runProgram SProgram7 toks (term :> stk)
  SProgram141 -> __runProgram SProgram61 toks (term :> stk)
  SProgram142 -> __runProgram SProgram64 toks (term :> stk)
  SProgram146 -> __runProgram SProgram7 toks (term :> stk)
  SProgram155 -> __runProgram SProgram62 toks (term :> stk)
  SProgram156 -> __runProgram SProgram63 toks (term :> stk)
  SProgram164 -> __runProgram SProgram63 toks (term :> stk)
  SProgram166 -> __runProgram SProgram62 toks (term :> stk)
  _ -> error ""

__gotoProgramForProgram :: ([Lexeme], Pos) -> Program -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoProgramForProgram toks term stk@(state, _, _) = case state of
  SProgram176 -> __runProgram SProgram177 toks (term :> stk)
  _ -> error ""

__gotoStmtForProgram :: ([Lexeme], Pos) -> Stmt -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoStmtForProgram toks term stk@(state, _, _) = case state of
  SProgram176 -> __runProgram SProgram179 toks (term :> stk)
  SProgram179 -> __runProgram SProgram179 toks (term :> stk)
  _ -> error ""

__gotoStmtsForProgram :: ([Lexeme], Pos) -> [Stmt] -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoStmtsForProgram toks term stk@(state, _, _) = case state of
  SProgram176 -> __runProgram SProgram178 toks (term :> stk)
  SProgram179 -> __runProgram SProgram180 toks (term :> stk)
  _ -> error ""

__gotoTermForProgram :: ([Lexeme], Pos) -> Expr -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoTermForProgram toks term stk@(state, _, _) = case state of
  SProgram17 -> __runProgram SProgram16 toks (term :> stk)
  SProgram18 -> __runProgram SProgram16 toks (term :> stk)
  SProgram19 -> __runProgram SProgram16 toks (term :> stk)
  SProgram20 -> __runProgram SProgram16 toks (term :> stk)
  SProgram37 -> __runProgram SProgram15 toks (term :> stk)
  SProgram38 -> __runProgram SProgram13 toks (term :> stk)
  SProgram39 -> __runProgram SProgram14 toks (term :> stk)
  SProgram40 -> __runProgram SProgram13 toks (term :> stk)
  SProgram41 -> __runProgram SProgram14 toks (term :> stk)
  SProgram42 -> __runProgram SProgram15 toks (term :> stk)
  SProgram43 -> __runProgram SProgram16 toks (term :> stk)
  SProgram44 -> __runProgram SProgram53 toks (term :> stk)
  SProgram45 -> __runProgram SProgram54 toks (term :> stk)
  SProgram46 -> __runProgram SProgram55 toks (term :> stk)
  SProgram47 -> __runProgram SProgram56 toks (term :> stk)
  SProgram73 -> __runProgram SProgram16 toks (term :> stk)
  SProgram74 -> __runProgram SProgram16 toks (term :> stk)
  SProgram75 -> __runProgram SProgram16 toks (term :> stk)
  SProgram76 -> __runProgram SProgram16 toks (term :> stk)
  SProgram95 -> __runProgram SProgram103 toks (term :> stk)
  SProgram96 -> __runProgram SProgram104 toks (term :> stk)
  SProgram97 -> __runProgram SProgram105 toks (term :> stk)
  SProgram98 -> __runProgram SProgram106 toks (term :> stk)
  SProgram115 -> __runProgram SProgram69 toks (term :> stk)
  SProgram116 -> __runProgram SProgram70 toks (term :> stk)
  SProgram117 -> __runProgram SProgram71 toks (term :> stk)
  SProgram118 -> __runProgram SProgram72 toks (term :> stk)
  SProgram129 -> __runProgram SProgram15 toks (term :> stk)
  SProgram130 -> __runProgram SProgram15 toks (term :> stk)
  SProgram141 -> __runProgram SProgram69 toks (term :> stk)
  SProgram142 -> __runProgram SProgram72 toks (term :> stk)
  SProgram146 -> __runProgram SProgram15 toks (term :> stk)
  SProgram155 -> __runProgram SProgram70 toks (term :> stk)
  SProgram156 -> __runProgram SProgram71 toks (term :> stk)
  SProgram164 -> __runProgram SProgram71 toks (term :> stk)
  SProgram166 -> __runProgram SProgram70 toks (term :> stk)
  _ -> error ""

__gotoTestForProgram :: ([Lexeme], Pos) -> Test -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoTestForProgram toks term stk@(state, _, _) = case state of
  _ -> error ""

__gotoTestSuiteForProgram :: ([Lexeme], Pos) -> TestSuite -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoTestSuiteForProgram toks term stk@(state, _, _) = case state of
  _ -> error ""

__gotoTestsForProgram :: ([Lexeme], Pos) -> [Test] -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoTestsForProgram toks term stk@(state, _, _) = case state of
  _ -> error ""

__gotoTupleForProgram :: ([Lexeme], Pos) -> [Expr] -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoTupleForProgram toks term stk@(state, _, _) = case state of
  SProgram85 -> __runProgram SProgram135 toks (term :> stk)
  SProgram86 -> __runProgram SProgram136 toks (term :> stk)
  SProgram127 -> __runProgram SProgram135 toks (term :> stk)
  SProgram128 -> __runProgram SProgram136 toks (term :> stk)
  SProgram145 -> __runProgram SProgram147 toks (term :> stk)
  _ -> error ""

__runProgram :: StProgram a -> ([Lexeme], Pos) -> Stack' StProgram a -> Either (Pos, [String]) Program
__runProgram = \cases {
; SProgram0 ((__p,  ",") : __input, __end) __stk ->
    __runProgram SProgram37 (__input, __end) (() :> (SProgram0, __p, __stk))
; SProgram1 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram42 (__input, __end) (() :> (SProgram1, __p, __stk))
; SProgram1 ((__p,  "=") : __input, __end) __stk ->
    __runProgram SProgram39 (__input, __end) (() :> (SProgram1, __p, __stk))
; SProgram2 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram43 (__input, __end) (() :> (SProgram2, __p, __stk))
; SProgram2 ((__p,  "=") : __input, __end) __stk ->
    __runProgram SProgram38 (__input, __end) (() :> (SProgram2, __p, __stk))
; SProgram3 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram40 (__input, __end) (() :> (SProgram3, __p, __stk))
; SProgram4 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram41 (__input, __end) (() :> (SProgram4, __p, __stk))
; SProgram5 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram44 (__input, __end) (() :> (SProgram5, __p, __stk))
; SProgram6 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram45 (__input, __end) (() :> (SProgram6, __p, __stk))
; SProgram7 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram46 (__input, __end) (() :> (SProgram7, __p, __stk))
; SProgram8 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram47 (__input, __end) (() :> (SProgram8, __p, __stk))
; SProgram9 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram44 (__input, __end) (() :> (SProgram9, __p, __stk))
; SProgram10 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram45 (__input, __end) (() :> (SProgram10, __p, __stk))
; SProgram11 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram46 (__input, __end) (() :> (SProgram11, __p, __stk))
; SProgram12 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram47 (__input, __end) (() :> (SProgram12, __p, __stk))
; SProgram17 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram20 (__input, __end) (() :> (SProgram17, __p, __stk))
; SProgram17 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram24 (__input, __end) (n :> (SProgram17, __p, __stk))
; SProgram17 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram32 (__input, __end) (n :> (SProgram17, __p, __stk))
; SProgram17 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram36 (__input, __end) (n :> (SProgram17, __p, __stk))
; SProgram18 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram20 (__input, __end) (() :> (SProgram18, __p, __stk))
; SProgram18 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram24 (__input, __end) (n :> (SProgram18, __p, __stk))
; SProgram18 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram32 (__input, __end) (n :> (SProgram18, __p, __stk))
; SProgram18 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram36 (__input, __end) (n :> (SProgram18, __p, __stk))
; SProgram19 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram20 (__input, __end) (() :> (SProgram19, __p, __stk))
; SProgram19 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram24 (__input, __end) (n :> (SProgram19, __p, __stk))
; SProgram19 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram32 (__input, __end) (n :> (SProgram19, __p, __stk))
; SProgram19 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram36 (__input, __end) (n :> (SProgram19, __p, __stk))
; SProgram20 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram20 (__input, __end) (() :> (SProgram20, __p, __stk))
; SProgram20 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram24 (__input, __end) (n :> (SProgram20, __p, __stk))
; SProgram20 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram32 (__input, __end) (n :> (SProgram20, __p, __stk))
; SProgram20 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram36 (__input, __end) (n :> (SProgram20, __p, __stk))
; SProgram37 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram19 (__input, __end) (() :> (SProgram37, __p, __stk))
; SProgram37 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram23 (__input, __end) (n :> (SProgram37, __p, __stk))
; SProgram37 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram31 (__input, __end) (n :> (SProgram37, __p, __stk))
; SProgram37 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram35 (__input, __end) (n :> (SProgram37, __p, __stk))
; SProgram38 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram17 (__input, __end) (() :> (SProgram38, __p, __stk))
; SProgram38 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram21 (__input, __end) (n :> (SProgram38, __p, __stk))
; SProgram38 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram29 (__input, __end) (n :> (SProgram38, __p, __stk))
; SProgram38 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram33 (__input, __end) (n :> (SProgram38, __p, __stk))
; SProgram39 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram18 (__input, __end) (() :> (SProgram39, __p, __stk))
; SProgram39 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram22 (__input, __end) (n :> (SProgram39, __p, __stk))
; SProgram39 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram30 (__input, __end) (n :> (SProgram39, __p, __stk))
; SProgram39 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram34 (__input, __end) (n :> (SProgram39, __p, __stk))
; SProgram40 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram17 (__input, __end) (() :> (SProgram40, __p, __stk))
; SProgram40 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram21 (__input, __end) (n :> (SProgram40, __p, __stk))
; SProgram40 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram29 (__input, __end) (n :> (SProgram40, __p, __stk))
; SProgram40 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram33 (__input, __end) (n :> (SProgram40, __p, __stk))
; SProgram41 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram18 (__input, __end) (() :> (SProgram41, __p, __stk))
; SProgram41 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram22 (__input, __end) (n :> (SProgram41, __p, __stk))
; SProgram41 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram30 (__input, __end) (n :> (SProgram41, __p, __stk))
; SProgram41 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram34 (__input, __end) (n :> (SProgram41, __p, __stk))
; SProgram42 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram19 (__input, __end) (() :> (SProgram42, __p, __stk))
; SProgram42 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram23 (__input, __end) (n :> (SProgram42, __p, __stk))
; SProgram42 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram31 (__input, __end) (n :> (SProgram42, __p, __stk))
; SProgram42 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram35 (__input, __end) (n :> (SProgram42, __p, __stk))
; SProgram43 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram20 (__input, __end) (() :> (SProgram43, __p, __stk))
; SProgram43 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram24 (__input, __end) (n :> (SProgram43, __p, __stk))
; SProgram43 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram32 (__input, __end) (n :> (SProgram43, __p, __stk))
; SProgram43 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram36 (__input, __end) (n :> (SProgram43, __p, __stk))
; SProgram44 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram17 (__input, __end) (() :> (SProgram44, __p, __stk))
; SProgram44 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram21 (__input, __end) (n :> (SProgram44, __p, __stk))
; SProgram44 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram29 (__input, __end) (n :> (SProgram44, __p, __stk))
; SProgram44 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram33 (__input, __end) (n :> (SProgram44, __p, __stk))
; SProgram45 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram18 (__input, __end) (() :> (SProgram45, __p, __stk))
; SProgram45 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram22 (__input, __end) (n :> (SProgram45, __p, __stk))
; SProgram45 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram30 (__input, __end) (n :> (SProgram45, __p, __stk))
; SProgram45 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram34 (__input, __end) (n :> (SProgram45, __p, __stk))
; SProgram46 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram19 (__input, __end) (() :> (SProgram46, __p, __stk))
; SProgram46 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram23 (__input, __end) (n :> (SProgram46, __p, __stk))
; SProgram46 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram31 (__input, __end) (n :> (SProgram46, __p, __stk))
; SProgram46 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram35 (__input, __end) (n :> (SProgram46, __p, __stk))
; SProgram47 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram20 (__input, __end) (() :> (SProgram47, __p, __stk))
; SProgram47 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram24 (__input, __end) (n :> (SProgram47, __p, __stk))
; SProgram47 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram32 (__input, __end) (n :> (SProgram47, __p, __stk))
; SProgram47 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram36 (__input, __end) (n :> (SProgram47, __p, __stk))
; SProgram48 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram57 (__input, __end) (() :> (SProgram48, __p, __stk))
; SProgram49 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram58 (__input, __end) (() :> (SProgram49, __p, __stk))
; SProgram50 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram59 (__input, __end) (() :> (SProgram50, __p, __stk))
; SProgram51 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram60 (__input, __end) (() :> (SProgram51, __p, __stk))
; SProgram61 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram95 (__input, __end) (() :> (SProgram61, __p, __stk))
; SProgram62 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram96 (__input, __end) (() :> (SProgram62, __p, __stk))
; SProgram63 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram97 (__input, __end) (() :> (SProgram63, __p, __stk))
; SProgram64 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram98 (__input, __end) (() :> (SProgram64, __p, __stk))
; SProgram65 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram95 (__input, __end) (() :> (SProgram65, __p, __stk))
; SProgram66 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram96 (__input, __end) (() :> (SProgram66, __p, __stk))
; SProgram67 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram97 (__input, __end) (() :> (SProgram67, __p, __stk))
; SProgram68 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram98 (__input, __end) (() :> (SProgram68, __p, __stk))
; SProgram73 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram20 (__input, __end) (() :> (SProgram73, __p, __stk))
; SProgram73 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram24 (__input, __end) (n :> (SProgram73, __p, __stk))
; SProgram73 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram32 (__input, __end) (n :> (SProgram73, __p, __stk))
; SProgram73 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram36 (__input, __end) (n :> (SProgram73, __p, __stk))
; SProgram74 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram20 (__input, __end) (() :> (SProgram74, __p, __stk))
; SProgram74 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram24 (__input, __end) (n :> (SProgram74, __p, __stk))
; SProgram74 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram32 (__input, __end) (n :> (SProgram74, __p, __stk))
; SProgram74 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram36 (__input, __end) (n :> (SProgram74, __p, __stk))
; SProgram75 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram20 (__input, __end) (() :> (SProgram75, __p, __stk))
; SProgram75 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram24 (__input, __end) (n :> (SProgram75, __p, __stk))
; SProgram75 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram32 (__input, __end) (n :> (SProgram75, __p, __stk))
; SProgram75 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram36 (__input, __end) (n :> (SProgram75, __p, __stk))
; SProgram76 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram20 (__input, __end) (() :> (SProgram76, __p, __stk))
; SProgram76 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram24 (__input, __end) (n :> (SProgram76, __p, __stk))
; SProgram76 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram32 (__input, __end) (n :> (SProgram76, __p, __stk))
; SProgram76 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram36 (__input, __end) (n :> (SProgram76, __p, __stk))
; SProgram85 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram129 (__input, __end) (() :> (SProgram85, __p, __stk))
; SProgram86 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram130 (__input, __end) (() :> (SProgram86, __p, __stk))
; SProgram95 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram73 (__input, __end) (() :> (SProgram95, __p, __stk))
; SProgram95 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram77 (__input, __end) (n :> (SProgram95, __p, __stk))
; SProgram95 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram87 (__input, __end) (n :> (SProgram95, __p, __stk))
; SProgram95 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram91 (__input, __end) (n :> (SProgram95, __p, __stk))
; SProgram96 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram74 (__input, __end) (() :> (SProgram96, __p, __stk))
; SProgram96 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram78 (__input, __end) (n :> (SProgram96, __p, __stk))
; SProgram96 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram88 (__input, __end) (n :> (SProgram96, __p, __stk))
; SProgram96 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram92 (__input, __end) (n :> (SProgram96, __p, __stk))
; SProgram97 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram75 (__input, __end) (() :> (SProgram97, __p, __stk))
; SProgram97 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram79 (__input, __end) (n :> (SProgram97, __p, __stk))
; SProgram97 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram89 (__input, __end) (n :> (SProgram97, __p, __stk))
; SProgram97 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram93 (__input, __end) (n :> (SProgram97, __p, __stk))
; SProgram98 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram76 (__input, __end) (() :> (SProgram98, __p, __stk))
; SProgram98 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram80 (__input, __end) (n :> (SProgram98, __p, __stk))
; SProgram98 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram90 (__input, __end) (n :> (SProgram98, __p, __stk))
; SProgram98 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram94 (__input, __end) (n :> (SProgram98, __p, __stk))
; SProgram99 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram107 (__input, __end) (() :> (SProgram99, __p, __stk))
; SProgram100 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram108 (__input, __end) (() :> (SProgram100, __p, __stk))
; SProgram101 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram109 (__input, __end) (() :> (SProgram101, __p, __stk))
; SProgram102 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram110 (__input, __end) (() :> (SProgram102, __p, __stk))
; SProgram111 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram116 (__input, __end) (() :> (SProgram111, __p, __stk))
; SProgram111 ((__p,  "=") : __input, __end) __stk ->
    __runProgram SProgram141 (__input, __end) (() :> (SProgram111, __p, __stk))
; SProgram112 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram117 (__input, __end) (() :> (SProgram112, __p, __stk))
; SProgram112 ((__p,  "=") : __input, __end) __stk ->
    __runProgram SProgram142 (__input, __end) (() :> (SProgram112, __p, __stk))
; SProgram113 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram115 (__input, __end) (() :> (SProgram113, __p, __stk))
; SProgram114 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram118 (__input, __end) (() :> (SProgram114, __p, __stk))
; SProgram115 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram73 (__input, __end) (() :> (SProgram115, __p, __stk))
; SProgram115 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram77 (__input, __end) (n :> (SProgram115, __p, __stk))
; SProgram115 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram87 (__input, __end) (n :> (SProgram115, __p, __stk))
; SProgram115 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram91 (__input, __end) (n :> (SProgram115, __p, __stk))
; SProgram116 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram74 (__input, __end) (() :> (SProgram116, __p, __stk))
; SProgram116 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram78 (__input, __end) (n :> (SProgram116, __p, __stk))
; SProgram116 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram88 (__input, __end) (n :> (SProgram116, __p, __stk))
; SProgram116 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram92 (__input, __end) (n :> (SProgram116, __p, __stk))
; SProgram117 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram75 (__input, __end) (() :> (SProgram117, __p, __stk))
; SProgram117 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram79 (__input, __end) (n :> (SProgram117, __p, __stk))
; SProgram117 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram89 (__input, __end) (n :> (SProgram117, __p, __stk))
; SProgram117 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram93 (__input, __end) (n :> (SProgram117, __p, __stk))
; SProgram118 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram76 (__input, __end) (() :> (SProgram118, __p, __stk))
; SProgram118 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram80 (__input, __end) (n :> (SProgram118, __p, __stk))
; SProgram118 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram90 (__input, __end) (n :> (SProgram118, __p, __stk))
; SProgram118 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram94 (__input, __end) (n :> (SProgram118, __p, __stk))
; SProgram119 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram127 (__input, __end) (n :> (SProgram119, __p, __stk))
; SProgram120 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram127 (__input, __end) (n :> (SProgram120, __p, __stk))
; SProgram123 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram127 (__input, __end) (n :> (SProgram123, __p, __stk))
; SProgram124 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram128 (__input, __end) (n :> (SProgram124, __p, __stk))
; SProgram127 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram129 (__input, __end) (() :> (SProgram127, __p, __stk))
; SProgram128 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram130 (__input, __end) (() :> (SProgram128, __p, __stk))
; SProgram129 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram19 (__input, __end) (() :> (SProgram129, __p, __stk))
; SProgram129 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram137 (__input, __end) (() :> (SProgram129, __p, __stk))
; SProgram129 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram23 (__input, __end) (n :> (SProgram129, __p, __stk))
; SProgram129 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram31 (__input, __end) (n :> (SProgram129, __p, __stk))
; SProgram129 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram35 (__input, __end) (n :> (SProgram129, __p, __stk))
; SProgram130 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram19 (__input, __end) (() :> (SProgram130, __p, __stk))
; SProgram130 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram138 (__input, __end) (() :> (SProgram130, __p, __stk))
; SProgram130 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram23 (__input, __end) (n :> (SProgram130, __p, __stk))
; SProgram130 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram31 (__input, __end) (n :> (SProgram130, __p, __stk))
; SProgram130 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram35 (__input, __end) (n :> (SProgram130, __p, __stk))
; SProgram139 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram143 (__input, __end) (() :> (SProgram139, __p, __stk))
; SProgram140 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram144 (__input, __end) (() :> (SProgram140, __p, __stk))
; SProgram141 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram73 (__input, __end) (() :> (SProgram141, __p, __stk))
; SProgram141 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram77 (__input, __end) (n :> (SProgram141, __p, __stk))
; SProgram141 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram87 (__input, __end) (n :> (SProgram141, __p, __stk))
; SProgram141 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram91 (__input, __end) (n :> (SProgram141, __p, __stk))
; SProgram142 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram76 (__input, __end) (() :> (SProgram142, __p, __stk))
; SProgram142 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram80 (__input, __end) (n :> (SProgram142, __p, __stk))
; SProgram142 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram90 (__input, __end) (n :> (SProgram142, __p, __stk))
; SProgram142 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram94 (__input, __end) (n :> (SProgram142, __p, __stk))
; SProgram145 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram146 (__input, __end) (() :> (SProgram145, __p, __stk))
; SProgram146 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram19 (__input, __end) (() :> (SProgram146, __p, __stk))
; SProgram146 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram148 (__input, __end) (() :> (SProgram146, __p, __stk))
; SProgram146 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram23 (__input, __end) (n :> (SProgram146, __p, __stk))
; SProgram146 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram31 (__input, __end) (n :> (SProgram146, __p, __stk))
; SProgram146 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram35 (__input, __end) (n :> (SProgram146, __p, __stk))
; SProgram149 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram150 (__input, __end) (() :> (SProgram149, __p, __stk))
; SProgram151 ((__p,  ",") : __input, __end) __stk ->
    __runProgram SProgram154 (__input, __end) (() :> (SProgram151, __p, __stk))
; SProgram152 ((__p,  ",") : __input, __end) __stk ->
    __runProgram SProgram155 (__input, __end) (() :> (SProgram152, __p, __stk))
; SProgram153 ((__p,  ",") : __input, __end) __stk ->
    __runProgram SProgram156 (__input, __end) (() :> (SProgram153, __p, __stk))
; SProgram154 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram119 (__input, __end) (() :> (SProgram154, __p, __stk))
; SProgram154 ((__p,  "-") : __input, __end) __stk ->
    __runProgram SProgram120 (__input, __end) (() :> (SProgram154, __p, __stk))
; SProgram155 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram74 (__input, __end) (() :> (SProgram155, __p, __stk))
; SProgram155 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram78 (__input, __end) (n :> (SProgram155, __p, __stk))
; SProgram155 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram85 (__input, __end) (n :> (SProgram155, __p, __stk))
; SProgram155 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram92 (__input, __end) (n :> (SProgram155, __p, __stk))
; SProgram155 ((__p,  "~") : __input, __end) __stk ->
    __runProgram SProgram123 (__input, __end) (() :> (SProgram155, __p, __stk))
; SProgram156 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram75 (__input, __end) (() :> (SProgram156, __p, __stk))
; SProgram156 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram79 (__input, __end) (n :> (SProgram156, __p, __stk))
; SProgram156 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram86 (__input, __end) (n :> (SProgram156, __p, __stk))
; SProgram156 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram93 (__input, __end) (n :> (SProgram156, __p, __stk))
; SProgram156 ((__p,  "~") : __input, __end) __stk ->
    __runProgram SProgram124 (__input, __end) (() :> (SProgram156, __p, __stk))
; SProgram162 ((__p,  "->") : __input, __end) __stk ->
    __runProgram SProgram164 (__input, __end) (() :> (SProgram162, __p, __stk))
; SProgram162 ((__p,  ".") : __input, __end) __stk ->
    __runProgram SProgram165 (__input, __end) (() :> (SProgram162, __p, __stk))
; SProgram162 ((__p,  "<-") : __input, __end) __stk ->
    __runProgram SProgram166 (__input, __end) (() :> (SProgram162, __p, __stk))
; SProgram162 ((__p,  "=>") : __input, __end) __stk ->
    __runProgram SProgram163 (__input, __end) (() :> (SProgram162, __p, __stk))
; SProgram163 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram119 (__input, __end) (() :> (SProgram163, __p, __stk))
; SProgram163 ((__p,  "-") : __input, __end) __stk ->
    __runProgram SProgram120 (__input, __end) (() :> (SProgram163, __p, __stk))
; SProgram164 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram75 (__input, __end) (() :> (SProgram164, __p, __stk))
; SProgram164 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram79 (__input, __end) (n :> (SProgram164, __p, __stk))
; SProgram164 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram86 (__input, __end) (n :> (SProgram164, __p, __stk))
; SProgram164 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram93 (__input, __end) (n :> (SProgram164, __p, __stk))
; SProgram164 ((__p,  "~") : __input, __end) __stk ->
    __runProgram SProgram124 (__input, __end) (() :> (SProgram164, __p, __stk))
; SProgram166 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram74 (__input, __end) (() :> (SProgram166, __p, __stk))
; SProgram166 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram78 (__input, __end) (n :> (SProgram166, __p, __stk))
; SProgram166 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram85 (__input, __end) (n :> (SProgram166, __p, __stk))
; SProgram166 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram92 (__input, __end) (n :> (SProgram166, __p, __stk))
; SProgram166 ((__p,  "~") : __input, __end) __stk ->
    __runProgram SProgram123 (__input, __end) (() :> (SProgram166, __p, __stk))
; SProgram167 ((__p,  ".") : __input, __end) __stk ->
    __runProgram SProgram170 (__input, __end) (() :> (SProgram167, __p, __stk))
; SProgram168 ((__p,  ".") : __input, __end) __stk ->
    __runProgram SProgram172 (__input, __end) (() :> (SProgram168, __p, __stk))
; SProgram168 ((__p,  "=>") : __input, __end) __stk ->
    __runProgram SProgram171 (__input, __end) (() :> (SProgram168, __p, __stk))
; SProgram169 ((__p,  ".") : __input, __end) __stk ->
    __runProgram SProgram173 (__input, __end) (() :> (SProgram169, __p, __stk))
; SProgram171 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram119 (__input, __end) (() :> (SProgram171, __p, __stk))
; SProgram171 ((__p,  "-") : __input, __end) __stk ->
    __runProgram SProgram120 (__input, __end) (() :> (SProgram171, __p, __stk))
; SProgram174 ((__p,  ".") : __input, __end) __stk ->
    __runProgram SProgram175 (__input, __end) (() :> (SProgram174, __p, __stk))
; SProgram176 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram145 (__input, __end) (n :> (SProgram176, __p, __stk))
; SProgram179 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram145 (__input, __end) (n :> (SProgram179, __p, __stk))
-- lookahead ), entity Exprs1
; SProgram0 ((__p,  ")") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoExprs1ForProgram ((__p,  ")") : __input, __end) (action55 __pos e) __stk
-- lookahead ), entity Expr
; SProgram1 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  ")") : __input, __end) (action59 __pos a) __stk
-- lookahead ,, entity Expr
; SProgram1 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  ",") : __input, __end) (action59 __pos a) __stk
-- lookahead ), entity Expr
; SProgram2 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  ")") : __input, __end) (action59 __pos a) __stk
-- lookahead ), entity Expr
; SProgram3 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  ")") : __input, __end) (action58 __pos a
                                                                        b) __stk
-- lookahead ), entity Expr
; SProgram4 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  ")") : __input, __end) (action58 __pos a
                                                                        b) __stk
-- lookahead ,, entity Expr
; SProgram4 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  ",") : __input, __end) (action58 __pos a
                                                                        b) __stk
-- lookahead ), entity Add
; SProgram5 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; SProgram5 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead ), entity Add
; SProgram6 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; SProgram6 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead ,, entity Add
; SProgram6 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead ), entity Add
; SProgram7 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; SProgram7 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead ,, entity Add
; SProgram7 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead =, entity Add
; SProgram7 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action63 __pos a) __stk
-- lookahead ), entity Add
; SProgram8 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; SProgram8 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead =, entity Add
; SProgram8 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action63 __pos a) __stk
-- lookahead ), entity Add
; SProgram9 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead +, entity Add
; SProgram9 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ), entity Add
; SProgram10 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead +, entity Add
; SProgram10 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ,, entity Add
; SProgram10 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ), entity Add
; SProgram11 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead +, entity Add
; SProgram11 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ,, entity Add
; SProgram11 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead =, entity Add
; SProgram11 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ), entity Add
; SProgram12 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead +, entity Add
; SProgram12 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead =, entity Add
; SProgram12 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ), entity Mult
; SProgram13 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; SProgram13 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; SProgram13 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead ), entity Mult
; SProgram14 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; SProgram14 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; SProgram14 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead ,, entity Mult
; SProgram14 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead ), entity Mult
; SProgram15 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; SProgram15 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; SProgram15 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead ,, entity Mult
; SProgram15 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead =, entity Mult
; SProgram15 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action67 __pos a) __stk
-- lookahead ), entity Mult
; SProgram16 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; SProgram16 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; SProgram16 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead =, entity Mult
; SProgram16 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action67 __pos a) __stk
-- lookahead ), entity Term
; SProgram21 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; SProgram21 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; SProgram21 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead ), entity Term
; SProgram22 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; SProgram22 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; SProgram22 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead ,, entity Term
; SProgram22 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead ), entity Term
; SProgram23 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; SProgram23 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; SProgram23 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead ,, entity Term
; SProgram23 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead =, entity Term
; SProgram23 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action71 __pos n) __stk
-- lookahead ), entity Term
; SProgram24 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; SProgram24 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; SProgram24 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead =, entity Term
; SProgram24 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action71 __pos n) __stk
-- lookahead ), entity Term
; SProgram25 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; SProgram25 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; SProgram25 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead ), entity Term
; SProgram26 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; SProgram26 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; SProgram26 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead ,, entity Term
; SProgram26 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead ), entity Term
; SProgram27 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; SProgram27 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; SProgram27 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead ,, entity Term
; SProgram27 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead =, entity Term
; SProgram27 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action72 __pos n) __stk
-- lookahead ), entity Term
; SProgram28 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; SProgram28 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; SProgram28 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead =, entity Term
; SProgram28 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action72 __pos n) __stk
-- lookahead ), entity Const
; SProgram29 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; SProgram29 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; SProgram29 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ), entity Const
; SProgram30 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; SProgram30 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; SProgram30 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ,, entity Const
; SProgram30 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead ), entity Const
; SProgram31 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; SProgram31 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; SProgram31 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ,, entity Const
; SProgram31 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead =, entity Const
; SProgram31 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead ), entity Const
; SProgram32 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; SProgram32 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; SProgram32 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead =, entity Const
; SProgram32 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead ), entity Const
; SProgram33 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; SProgram33 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; SProgram33 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead ), entity Const
; SProgram34 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; SProgram34 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; SProgram34 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead ,, entity Const
; SProgram34 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead ), entity Const
; SProgram35 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; SProgram35 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; SProgram35 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead ,, entity Const
; SProgram35 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead =, entity Const
; SProgram35 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action76 __pos n) __stk
-- lookahead ), entity Const
; SProgram36 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; SProgram36 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; SProgram36 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead =, entity Const
; SProgram36 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action76 __pos n) __stk
-- lookahead ), entity Exprs1
; SProgram52 ((__p,  ")") : __input, __end) ((es :> (_, _, _ :> (_, _, e :> __stk@(_, __pos, _))))) ->
    __gotoExprs1ForProgram ((__p,  ")") : __input, __end) (action54 __pos e
                                                                          es) __stk
-- lookahead ), entity Mult
; SProgram53 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead *, entity Mult
; SProgram53 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead +, entity Mult
; SProgram53 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ), entity Mult
; SProgram54 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead *, entity Mult
; SProgram54 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead +, entity Mult
; SProgram54 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ,, entity Mult
; SProgram54 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ), entity Mult
; SProgram55 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead *, entity Mult
; SProgram55 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead +, entity Mult
; SProgram55 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ,, entity Mult
; SProgram55 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead =, entity Mult
; SProgram55 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ), entity Mult
; SProgram56 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead *, entity Mult
; SProgram56 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead +, entity Mult
; SProgram56 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead =, entity Mult
; SProgram56 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ), entity Term
; SProgram57 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; SProgram57 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; SProgram57 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead ), entity Term
; SProgram58 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; SProgram58 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; SProgram58 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead ,, entity Term
; SProgram58 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead ), entity Term
; SProgram59 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; SProgram59 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; SProgram59 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead ,, entity Term
; SProgram59 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead =, entity Term
; SProgram59 ((__p,  "=") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action70 __pos e) __stk
-- lookahead ), entity Term
; SProgram60 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; SProgram60 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; SProgram60 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead =, entity Term
; SProgram60 ((__p,  "=") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Add
; SProgram61 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead ,, entity Add
; SProgram61 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead ., entity Add
; SProgram61 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; SProgram62 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead ,, entity Add
; SProgram62 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead ., entity Add
; SProgram62 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action63 __pos a) __stk
-- lookahead =, entity Add
; SProgram62 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; SProgram63 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead ,, entity Add
; SProgram63 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead ., entity Add
; SProgram63 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action63 __pos a) __stk
-- lookahead =, entity Add
; SProgram63 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action63 __pos a) __stk
-- lookahead =>, entity Add
; SProgram63 ((__p,  "=>") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "=>") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; SProgram64 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead ,, entity Add
; SProgram64 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead ., entity Add
; SProgram64 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action63 __pos a) __stk
-- lookahead =>, entity Add
; SProgram64 ((__p,  "=>") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "=>") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; SProgram65 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ,, entity Add
; SProgram65 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ., entity Add
; SProgram65 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead +, entity Add
; SProgram66 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ,, entity Add
; SProgram66 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ., entity Add
; SProgram66 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead =, entity Add
; SProgram66 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead +, entity Add
; SProgram67 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ,, entity Add
; SProgram67 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ., entity Add
; SProgram67 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead =, entity Add
; SProgram67 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead =>, entity Add
; SProgram67 ((__p,  "=>") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "=>") : __input, __end) (action62 __pos a
                                                                        b) __stk
-- lookahead +, entity Add
; SProgram68 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ,, entity Add
; SProgram68 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ., entity Add
; SProgram68 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead =>, entity Add
; SProgram68 ((__p,  "=>") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "=>") : __input, __end) (action62 __pos a
                                                                        b) __stk
-- lookahead *, entity Mult
; SProgram69 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; SProgram69 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead ,, entity Mult
; SProgram69 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead ., entity Mult
; SProgram69 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; SProgram70 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; SProgram70 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead ,, entity Mult
; SProgram70 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead ., entity Mult
; SProgram70 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action67 __pos a) __stk
-- lookahead =, entity Mult
; SProgram70 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; SProgram71 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; SProgram71 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead ,, entity Mult
; SProgram71 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead ., entity Mult
; SProgram71 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action67 __pos a) __stk
-- lookahead =, entity Mult
; SProgram71 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action67 __pos a) __stk
-- lookahead =>, entity Mult
; SProgram71 ((__p,  "=>") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "=>") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; SProgram72 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; SProgram72 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead ,, entity Mult
; SProgram72 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead ., entity Mult
; SProgram72 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action67 __pos a) __stk
-- lookahead =>, entity Mult
; SProgram72 ((__p,  "=>") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "=>") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Term
; SProgram77 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; SProgram77 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead ,, entity Term
; SProgram77 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead ., entity Term
; SProgram77 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; SProgram78 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; SProgram78 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead ,, entity Term
; SProgram78 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead ., entity Term
; SProgram78 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action71 __pos n) __stk
-- lookahead =, entity Term
; SProgram78 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; SProgram79 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; SProgram79 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead ,, entity Term
; SProgram79 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead ., entity Term
; SProgram79 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action71 __pos n) __stk
-- lookahead =, entity Term
; SProgram79 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action71 __pos n) __stk
-- lookahead =>, entity Term
; SProgram79 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=>") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; SProgram80 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; SProgram80 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead ,, entity Term
; SProgram80 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead ., entity Term
; SProgram80 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action71 __pos n) __stk
-- lookahead =>, entity Term
; SProgram80 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=>") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; SProgram81 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; SProgram81 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead ,, entity Term
; SProgram81 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead ., entity Term
; SProgram81 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; SProgram82 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; SProgram82 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead ,, entity Term
; SProgram82 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead ., entity Term
; SProgram82 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action72 __pos n) __stk
-- lookahead =, entity Term
; SProgram82 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; SProgram83 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; SProgram83 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead ,, entity Term
; SProgram83 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead ., entity Term
; SProgram83 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action72 __pos n) __stk
-- lookahead =, entity Term
; SProgram83 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action72 __pos n) __stk
-- lookahead =>, entity Term
; SProgram83 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=>") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; SProgram84 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; SProgram84 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead ,, entity Term
; SProgram84 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead ., entity Term
; SProgram84 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action72 __pos n) __stk
-- lookahead =>, entity Term
; SProgram84 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=>") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Const
; SProgram85 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; SProgram85 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ,, entity Const
; SProgram85 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead ., entity Const
; SProgram85 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action75 __pos n) __stk
-- lookahead =, entity Const
; SProgram85 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; SProgram86 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; SProgram86 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ,, entity Const
; SProgram86 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead ., entity Const
; SProgram86 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action75 __pos n) __stk
-- lookahead =, entity Const
; SProgram86 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead =>, entity Const
; SProgram86 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=>") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; SProgram87 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; SProgram87 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ,, entity Const
; SProgram87 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead ., entity Const
; SProgram87 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; SProgram88 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; SProgram88 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ,, entity Const
; SProgram88 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead ., entity Const
; SProgram88 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action75 __pos n) __stk
-- lookahead =, entity Const
; SProgram88 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; SProgram89 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; SProgram89 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ,, entity Const
; SProgram89 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead ., entity Const
; SProgram89 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action75 __pos n) __stk
-- lookahead =, entity Const
; SProgram89 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead =>, entity Const
; SProgram89 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=>") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; SProgram90 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; SProgram90 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ,, entity Const
; SProgram90 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead ., entity Const
; SProgram90 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action75 __pos n) __stk
-- lookahead =>, entity Const
; SProgram90 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=>") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; SProgram91 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; SProgram91 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead ,, entity Const
; SProgram91 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead ., entity Const
; SProgram91 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; SProgram92 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; SProgram92 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead ,, entity Const
; SProgram92 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead ., entity Const
; SProgram92 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action76 __pos n) __stk
-- lookahead =, entity Const
; SProgram92 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; SProgram93 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; SProgram93 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead ,, entity Const
; SProgram93 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead ., entity Const
; SProgram93 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action76 __pos n) __stk
-- lookahead =, entity Const
; SProgram93 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action76 __pos n) __stk
-- lookahead =>, entity Const
; SProgram93 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=>") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; SProgram94 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; SProgram94 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead ,, entity Const
; SProgram94 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead ., entity Const
; SProgram94 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action76 __pos n) __stk
-- lookahead =>, entity Const
; SProgram94 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=>") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Mult
; SProgram103 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead +, entity Mult
; SProgram103 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ,, entity Mult
; SProgram103 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ., entity Mult
; SProgram103 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead *, entity Mult
; SProgram104 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead +, entity Mult
; SProgram104 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ,, entity Mult
; SProgram104 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ., entity Mult
; SProgram104 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead =, entity Mult
; SProgram104 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead *, entity Mult
; SProgram105 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead +, entity Mult
; SProgram105 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ,, entity Mult
; SProgram105 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ., entity Mult
; SProgram105 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead =, entity Mult
; SProgram105 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead =>, entity Mult
; SProgram105 ((__p,  "=>") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "=>") : __input, __end) (action66 __pos a
                                                                         b) __stk
-- lookahead *, entity Mult
; SProgram106 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead +, entity Mult
; SProgram106 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ,, entity Mult
; SProgram106 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ., entity Mult
; SProgram106 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead =>, entity Mult
; SProgram106 ((__p,  "=>") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "=>") : __input, __end) (action66 __pos a
                                                                         b) __stk
-- lookahead *, entity Term
; SProgram107 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; SProgram107 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead ,, entity Term
; SProgram107 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead ., entity Term
; SProgram107 ((__p,  ".") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; SProgram108 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; SProgram108 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead ,, entity Term
; SProgram108 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead ., entity Term
; SProgram108 ((__p,  ".") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action70 __pos e) __stk
-- lookahead =, entity Term
; SProgram108 ((__p,  "=") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; SProgram109 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; SProgram109 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead ,, entity Term
; SProgram109 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead ., entity Term
; SProgram109 ((__p,  ".") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action70 __pos e) __stk
-- lookahead =, entity Term
; SProgram109 ((__p,  "=") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action70 __pos e) __stk
-- lookahead =>, entity Term
; SProgram109 ((__p,  "=>") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "=>") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; SProgram110 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; SProgram110 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead ,, entity Term
; SProgram110 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead ., entity Term
; SProgram110 ((__p,  ".") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action70 __pos e) __stk
-- lookahead =>, entity Term
; SProgram110 ((__p,  "=>") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "=>") : __input, __end) (action70 __pos e) __stk
-- lookahead ,, entity Expr
; SProgram111 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  ",") : __input, __end) (action59 __pos a) __stk
-- lookahead ., entity Expr
; SProgram111 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  ".") : __input, __end) (action59 __pos a) __stk
-- lookahead ,, entity Expr
; SProgram112 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  ",") : __input, __end) (action59 __pos a) __stk
-- lookahead ., entity Expr
; SProgram112 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  ".") : __input, __end) (action59 __pos a) __stk
-- lookahead =>, entity Expr
; SProgram112 ((__p,  "=>") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  "=>") : __input, __end) (action59 __pos a) __stk
-- lookahead ,, entity Expr
; SProgram113 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  ",") : __input, __end) (action58 __pos a
                                                                        b) __stk
-- lookahead ., entity Expr
; SProgram113 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  ".") : __input, __end) (action58 __pos a
                                                                        b) __stk
-- lookahead ,, entity Expr
; SProgram114 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  ",") : __input, __end) (action58 __pos a
                                                                        b) __stk
-- lookahead ., entity Expr
; SProgram114 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  ".") : __input, __end) (action58 __pos a
                                                                        b) __stk
-- lookahead =>, entity Expr
; SProgram114 ((__p,  "=>") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  "=>") : __input, __end) (action58 __pos a
                                                                         b) __stk
-- lookahead ,, entity Cond
; SProgram121 ((__p,  ",") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ",") : __input, __end) (action42 __pos c) __stk
-- lookahead ., entity Cond
; SProgram121 ((__p,  ".") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ".") : __input, __end) (action42 __pos c) __stk
-- lookahead ,, entity Cond
; SProgram122 ((__p,  ",") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ",") : __input, __end) (action42 __pos c) __stk
-- lookahead ., entity Cond
; SProgram122 ((__p,  ".") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ".") : __input, __end) (action42 __pos c) __stk
-- lookahead =>, entity Cond
; SProgram122 ((__p,  "=>") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  "=>") : __input, __end) (action42 __pos c) __stk
-- lookahead ,, entity Cond
; SProgram125 ((__p,  ",") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ",") : __input, __end) (action44 __pos e) __stk
-- lookahead ., entity Cond
; SProgram125 ((__p,  ".") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ".") : __input, __end) (action44 __pos e) __stk
-- lookahead ,, entity Cond
; SProgram126 ((__p,  ",") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ",") : __input, __end) (action44 __pos e) __stk
-- lookahead ., entity Cond
; SProgram126 ((__p,  ".") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ".") : __input, __end) (action44 __pos e) __stk
-- lookahead =>, entity Cond
; SProgram126 ((__p,  "=>") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  "=>") : __input, __end) (action44 __pos e) __stk
-- lookahead ,, entity Change
; SProgram131 ((__p,  ",") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoChangeForProgram ((__p,  ",") : __input, __end) (action34 __pos c) __stk
-- lookahead ., entity Change
; SProgram131 ((__p,  ".") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoChangeForProgram ((__p,  ".") : __input, __end) (action34 __pos c) __stk
-- lookahead ,, entity Change
; SProgram132 ((__p,  ",") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoChangeForProgram ((__p,  ",") : __input, __end) (action35 __pos c) __stk
-- lookahead ., entity Change
; SProgram132 ((__p,  ".") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoChangeForProgram ((__p,  ".") : __input, __end) (action35 __pos c) __stk
-- lookahead ,, entity Cond
; SProgram133 ((__p,  ",") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoCondForProgram ((__p,  ",") : __input, __end) (action43 __pos c) __stk
-- lookahead ., entity Cond
; SProgram133 ((__p,  ".") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoCondForProgram ((__p,  ".") : __input, __end) (action43 __pos c) __stk
-- lookahead ,, entity Cond
; SProgram134 ((__p,  ",") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoCondForProgram ((__p,  ",") : __input, __end) (action43 __pos c) __stk
-- lookahead ., entity Cond
; SProgram134 ((__p,  ".") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoCondForProgram ((__p,  ".") : __input, __end) (action43 __pos c) __stk
-- lookahead =>, entity Cond
; SProgram134 ((__p,  "=>") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoCondForProgram ((__p,  "=>") : __input, __end) (action43 __pos c) __stk
-- lookahead ,, entity Call
; SProgram135 ((__p,  ",") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  ",") : __input, __end) (action47 __pos pre
                                                                        t) __stk
-- lookahead ., entity Call
; SProgram135 ((__p,  ".") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  ".") : __input, __end) (action47 __pos pre
                                                                        t) __stk
-- lookahead ,, entity Call
; SProgram136 ((__p,  ",") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  ",") : __input, __end) (action47 __pos pre
                                                                        t) __stk
-- lookahead ., entity Call
; SProgram136 ((__p,  ".") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  ".") : __input, __end) (action47 __pos pre
                                                                        t) __stk
-- lookahead =>, entity Call
; SProgram136 ((__p,  "=>") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  "=>") : __input, __end) (action47 __pos pre
                                                                         t) __stk
-- lookahead ,, entity Tuple
; SProgram137 ((__p,  ",") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  ",") : __input, __end) (action50 __pos ) __stk
-- lookahead ., entity Tuple
; SProgram137 ((__p,  ".") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  ".") : __input, __end) (action50 __pos ) __stk
-- lookahead ,, entity Tuple
; SProgram138 ((__p,  ",") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  ",") : __input, __end) (action50 __pos ) __stk
-- lookahead ., entity Tuple
; SProgram138 ((__p,  ".") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  ".") : __input, __end) (action50 __pos ) __stk
-- lookahead =>, entity Tuple
; SProgram138 ((__p,  "=>") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  "=>") : __input, __end) (action50 __pos ) __stk
-- lookahead ,, entity Tuple
; SProgram143 ((__p,  ",") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  ",") : __input, __end) (action51 __pos es) __stk
-- lookahead ., entity Tuple
; SProgram143 ((__p,  ".") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  ".") : __input, __end) (action51 __pos es) __stk
-- lookahead ,, entity Tuple
; SProgram144 ((__p,  ",") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  ",") : __input, __end) (action51 __pos es) __stk
-- lookahead ., entity Tuple
; SProgram144 ((__p,  ".") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  ".") : __input, __end) (action51 __pos es) __stk
-- lookahead =>, entity Tuple
; SProgram144 ((__p,  "=>") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  "=>") : __input, __end) (action51 __pos es) __stk
-- lookahead ->, entity Call
; SProgram147 ((__p,  "->") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  "->") : __input, __end) (action47 __pos pre
                                                                         t) __stk
-- lookahead ., entity Call
; SProgram147 ((__p,  ".") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  ".") : __input, __end) (action47 __pos pre
                                                                        t) __stk
-- lookahead <-, entity Call
; SProgram147 ((__p,  "<-") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  "<-") : __input, __end) (action47 __pos pre
                                                                         t) __stk
-- lookahead =>, entity Call
; SProgram147 ((__p,  "=>") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  "=>") : __input, __end) (action47 __pos pre
                                                                         t) __stk
-- lookahead ->, entity Tuple
; SProgram148 ((__p,  "->") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  "->") : __input, __end) (action50 __pos ) __stk
-- lookahead ., entity Tuple
; SProgram148 ((__p,  ".") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  ".") : __input, __end) (action50 __pos ) __stk
-- lookahead <-, entity Tuple
; SProgram148 ((__p,  "<-") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  "<-") : __input, __end) (action50 __pos ) __stk
-- lookahead =>, entity Tuple
; SProgram148 ((__p,  "=>") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  "=>") : __input, __end) (action50 __pos ) __stk
-- lookahead ->, entity Tuple
; SProgram150 ((__p,  "->") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  "->") : __input, __end) (action51 __pos es) __stk
-- lookahead ., entity Tuple
; SProgram150 ((__p,  ".") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  ".") : __input, __end) (action51 __pos es) __stk
-- lookahead <-, entity Tuple
; SProgram150 ((__p,  "<-") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  "<-") : __input, __end) (action51 __pos es) __stk
-- lookahead =>, entity Tuple
; SProgram150 ((__p,  "=>") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  "=>") : __input, __end) (action51 __pos es) __stk
-- lookahead ., entity Changes
; SProgram151 ((__p,  ".") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoChangesForProgram ((__p,  ".") : __input, __end) (action31 __pos c) __stk
-- lookahead ., entity Conds
; SProgram152 ((__p,  ".") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondsForProgram ((__p,  ".") : __input, __end) (action39 __pos c) __stk
-- lookahead ., entity Conds
; SProgram153 ((__p,  ".") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondsForProgram ((__p,  ".") : __input, __end) (action39 __pos c) __stk
-- lookahead =>, entity Conds
; SProgram153 ((__p,  "=>") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondsForProgram ((__p,  "=>") : __input, __end) (action39 __pos c) __stk
-- lookahead ., entity Changes
; SProgram157 ((__p,  ".") : __input, __end) ((cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _))))) ->
    __gotoChangesForProgram ((__p,  ".") : __input, __end) (action30 __pos c
                                                                           cs) __stk
-- lookahead ., entity Conds
; SProgram158 ((__p,  ".") : __input, __end) ((cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _))))) ->
    __gotoCondsForProgram ((__p,  ".") : __input, __end) (action38 __pos c
                                                                         cs) __stk
-- lookahead ., entity Conds
; SProgram159 ((__p,  ".") : __input, __end) ((cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _))))) ->
    __gotoCondsForProgram ((__p,  ".") : __input, __end) (action38 __pos c
                                                                         cs) __stk
-- lookahead =>, entity Conds
; SProgram159 ((__p,  "=>") : __input, __end) ((cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _))))) ->
    __gotoCondsForProgram ((__p,  "=>") : __input, __end) (action38 __pos c
                                                                          cs) __stk
-- lookahead <name>, entity Stmt
; SProgram160 ((__p, LowercaseName tok) : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoStmtForProgram ((__p, LowercaseName tok) : __input, __end) (action17 __pos c) __stk
-- lookahead <eof>, entity Stmt
; SProgram160 ([], __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoStmtForProgram ([], __end) (action17 __pos c) __stk
-- lookahead <name>, entity Stmt
; SProgram161 ((__p, LowercaseName tok) : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoStmtForProgram ((__p, LowercaseName tok) : __input, __end) (action18 __pos e) __stk
-- lookahead <eof>, entity Stmt
; SProgram161 ([], __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoStmtForProgram ([], __end) (action18 __pos e) __stk
-- lookahead <name>, entity Clause
; SProgram165 ((__p, LowercaseName tok) : __input, __end) ((_ :> (_, _, c :> __stk@(_, __pos, _)))) ->
    __gotoClauseForProgram ((__p, LowercaseName tok) : __input, __end) (action26 __pos c) __stk
-- lookahead <eof>, entity Clause
; SProgram165 ([], __end) ((_ :> (_, _, c :> __stk@(_, __pos, _)))) ->
    __gotoClauseForProgram ([], __end) (action26 __pos c) __stk
-- lookahead <name>, entity Effect
; SProgram170 ((__p, LowercaseName tok) : __input, __end) ((_ :> (_, _, ds :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))) ->
    __gotoEffectForProgram ((__p, LowercaseName tok) : __input, __end) (action21 __pos c
                                                                                       ds) __stk
-- lookahead <eof>, entity Effect
; SProgram170 ([], __end) ((_ :> (_, _, ds :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))) ->
    __gotoEffectForProgram ([], __end) (action21 __pos c ds) __stk
-- lookahead <name>, entity Effect
; SProgram172 ((__p, LowercaseName tok) : __input, __end) ((_ :> (_, _, cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))) ->
    __gotoEffectForProgram ((__p, LowercaseName tok) : __input, __end) (action23 __pos c
                                                                                       cs) __stk
-- lookahead <eof>, entity Effect
; SProgram172 ([], __end) ((_ :> (_, _, cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))) ->
    __gotoEffectForProgram ([], __end) (action23 __pos c cs) __stk
-- lookahead <name>, entity Clause
; SProgram173 ((__p, LowercaseName tok) : __input, __end) ((_ :> (_, _, cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))) ->
    __gotoClauseForProgram ((__p, LowercaseName tok) : __input, __end) (action27 __pos c
                                                                                       cs) __stk
-- lookahead <eof>, entity Clause
; SProgram173 ([], __end) ((_ :> (_, _, cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))) ->
    __gotoClauseForProgram ([], __end) (action27 __pos c cs) __stk
-- lookahead <name>, entity Effect
; SProgram175 ((__p, LowercaseName tok) : __input, __end) ((_ :> (_, _, ds :> (_, _, _ :> (_, _, cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))))) ->
    __gotoEffectForProgram ((__p, LowercaseName tok) : __input, __end) (action22 __pos c
                                                                                       cs ds) __stk
-- lookahead <eof>, entity Effect
; SProgram175 ([], __end) ((_ :> (_, _, ds :> (_, _, _ :> (_, _, cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))))) ->
    __gotoEffectForProgram ([], __end) (action22 __pos c cs ds) __stk
-- lookahead <eof>, entity Program
; SProgram177 ([], __end) ((res :> __stk@(_, __pos, _))) ->
    pure res
-- lookahead <eof>, entity Program
; SProgram178 ([], __end) ((stmts :> __stk@(_, __pos, _))) ->
    __gotoProgramForProgram ([], __end) (action10 __pos stmts) __stk
-- lookahead <eof>, entity Stmts
; SProgram179 ([], __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoStmtsForProgram ([], __end) (action14 __pos c) __stk
-- lookahead <eof>, entity Stmts
; SProgram180 ([], __end) ((cs :> (_, _, c :> __stk@(_, __pos, _)))) ->
    __gotoStmtsForProgram ([], __end) (action13 __pos c cs) __stk
; SProgram0 __input _ -> Left  (currentPos __input, [")", ","])
; SProgram1 __input _ ->
    Left  (currentPos __input, [")", "+", ",", "="])
; SProgram2 __input _ ->
    Left  (currentPos __input, [")", "+", "="])
; SProgram3 __input _ -> Left  (currentPos __input, [")", "+"])
; SProgram4 __input _ ->
    Left  (currentPos __input, [")", "+", ","])
; SProgram5 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram6 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram7 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram8 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram9 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram10 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram11 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram12 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram13 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram14 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram15 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram16 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram17 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram18 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram19 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram20 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram21 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram22 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram23 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram24 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram25 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram26 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram27 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram28 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram29 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram30 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram31 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram32 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram33 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram34 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram35 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram36 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram37 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram38 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram39 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram40 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram41 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram42 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram43 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram44 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram45 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram46 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram47 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram48 __input _ -> Left  (currentPos __input, [")"])
; SProgram49 __input _ -> Left  (currentPos __input, [")"])
; SProgram50 __input _ -> Left  (currentPos __input, [")"])
; SProgram51 __input _ -> Left  (currentPos __input, [")"])
; SProgram52 __input _ -> Left  (currentPos __input, [")"])
; SProgram53 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram54 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram55 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram56 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram57 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram58 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram59 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram60 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram61 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram62 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram63 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram64 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram65 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram66 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram67 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram68 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram69 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram70 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram71 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram72 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram73 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram74 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram75 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram76 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram77 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram78 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram79 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram80 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram81 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram82 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram83 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram84 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram85 __input _ ->
    Left  (currentPos __input, ["(", "*", "+", ",", ".", "="])
; SProgram86 __input _ ->
    Left  (currentPos __input, ["(", "*", "+", ",", ".", "=", "=>"])
; SProgram87 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram88 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram89 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram90 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram91 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram92 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram93 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram94 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram95 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram96 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram97 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram98 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram99 __input _ -> Left  (currentPos __input, [")"])
; SProgram100 __input _ -> Left  (currentPos __input, [")"])
; SProgram101 __input _ -> Left  (currentPos __input, [")"])
; SProgram102 __input _ -> Left  (currentPos __input, [")"])
; SProgram103 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram104 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram105 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram106 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram107 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram108 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram109 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram110 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram111 __input _ ->
    Left  (currentPos __input, ["+", ",", ".", "="])
; SProgram112 __input _ ->
    Left  (currentPos __input, ["+", ",", ".", "=", "=>"])
; SProgram113 __input _ ->
    Left  (currentPos __input, ["+", ",", "."])
; SProgram114 __input _ ->
    Left  (currentPos __input, ["+", ",", ".", "=>"])
; SProgram115 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram116 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram117 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram118 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram119 __input _ -> Left  (currentPos __input, ["<name>"])
; SProgram120 __input _ -> Left  (currentPos __input, ["<name>"])
; SProgram121 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram122 __input _ ->
    Left  (currentPos __input, [",", ".", "=>"])
; SProgram123 __input _ -> Left  (currentPos __input, ["<name>"])
; SProgram124 __input _ -> Left  (currentPos __input, ["<name>"])
; SProgram125 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram126 __input _ ->
    Left  (currentPos __input, [",", ".", "=>"])
; SProgram127 __input _ -> Left  (currentPos __input, ["("])
; SProgram128 __input _ -> Left  (currentPos __input, ["("])
; SProgram129 __input _ ->
    Left  (currentPos __input, ["(", ")", "<Name>", "<name>", "<num>"])
; SProgram130 __input _ ->
    Left  (currentPos __input, ["(", ")", "<Name>", "<name>", "<num>"])
; SProgram131 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram132 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram133 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram134 __input _ ->
    Left  (currentPos __input, [",", ".", "=>"])
; SProgram135 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram136 __input _ ->
    Left  (currentPos __input, [",", ".", "=>"])
; SProgram137 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram138 __input _ ->
    Left  (currentPos __input, [",", ".", "=>"])
; SProgram139 __input _ -> Left  (currentPos __input, [")"])
; SProgram140 __input _ -> Left  (currentPos __input, [")"])
; SProgram141 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram142 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram143 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram144 __input _ ->
    Left  (currentPos __input, [",", ".", "=>"])
; SProgram145 __input _ -> Left  (currentPos __input, ["("])
; SProgram146 __input _ ->
    Left  (currentPos __input, ["(", ")", "<Name>", "<name>", "<num>"])
; SProgram147 __input _ ->
    Left  (currentPos __input, ["->", ".", "<-", "=>"])
; SProgram148 __input _ ->
    Left  (currentPos __input, ["->", ".", "<-", "=>"])
; SProgram149 __input _ -> Left  (currentPos __input, [")"])
; SProgram150 __input _ ->
    Left  (currentPos __input, ["->", ".", "<-", "=>"])
; SProgram151 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram152 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram153 __input _ ->
    Left  (currentPos __input, [",", ".", "=>"])
; SProgram154 __input _ -> Left  (currentPos __input, ["+", "-"])
; SProgram155 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>", "~"])
; SProgram156 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>", "~"])
; SProgram157 __input _ -> Left  (currentPos __input, ["."])
; SProgram158 __input _ -> Left  (currentPos __input, ["."])
; SProgram159 __input _ -> Left  (currentPos __input, [".", "=>"])
; SProgram160 __input _ ->
    Left  (currentPos __input, ["<name>", "<eof>"])
; SProgram161 __input _ ->
    Left  (currentPos __input, ["<name>", "<eof>"])
; SProgram162 __input _ ->
    Left  (currentPos __input, ["->", ".", "<-", "=>"])
; SProgram163 __input _ -> Left  (currentPos __input, ["+", "-"])
; SProgram164 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>", "~"])
; SProgram165 __input _ ->
    Left  (currentPos __input, ["<name>", "<eof>"])
; SProgram166 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>", "~"])
; SProgram167 __input _ -> Left  (currentPos __input, ["."])
; SProgram168 __input _ -> Left  (currentPos __input, [".", "=>"])
; SProgram169 __input _ -> Left  (currentPos __input, ["."])
; SProgram170 __input _ ->
    Left  (currentPos __input, ["<name>", "<eof>"])
; SProgram171 __input _ -> Left  (currentPos __input, ["+", "-"])
; SProgram172 __input _ ->
    Left  (currentPos __input, ["<name>", "<eof>"])
; SProgram173 __input _ ->
    Left  (currentPos __input, ["<name>", "<eof>"])
; SProgram174 __input _ -> Left  (currentPos __input, ["."])
; SProgram175 __input _ ->
    Left  (currentPos __input, ["<name>", "<eof>"])
; SProgram176 __input _ -> Left  (currentPos __input, ["<name>"])
; SProgram177 __input _ -> Left  (currentPos __input, ["<eof>"])
; SProgram178 __input _ -> Left  (currentPos __input, ["<eof>"])
; SProgram179 __input _ ->
    Left  (currentPos __input, ["<name>", "<eof>"])
; SProgram180 __input _ -> Left  (currentPos __input, ["<eof>"])
} where {
; action10 pos stmts =
          Program {stmts}
; action13 pos c cs =
    c : cs
; action14 pos c =
    [c]
; action17 pos c =
    StmtClause pos c
; action18 pos e =
    StmtEffect pos e
; action21 pos c ds =
    Effect pos c [] ds
; action22 pos c cs ds =
    Effect pos c cs ds
; action23 pos c cs =
    Effect pos c cs []
; action26 pos c =
    Clause pos c []
; action27 pos c cs =
    Clause pos c cs
; action30 pos c cs =
    c : cs
; action31 pos c =
    [c]
; action34 pos c =
    Assert pos c
; action35 pos c =
    Refute pos c
; action38 pos c cs =
    c : cs
; action39 pos c =
    [c]
; action42 pos c =
        CondAssert pos c
; action43 pos c =
    CondRefute pos c
; action44 pos e =
        CondGuard  pos e
; action47 pos pre t =
    Call pos pre t
; action50 pos =
    []
; action51 pos es =
    es
; action54 pos e es =
    e : es
; action55 pos e =
    [e]
; action58 pos a b =
    ExprBinary pos a Equals b
; action59 pos a =
    a
; action62 pos a b =
    ExprBinary pos a Add b
; action63 pos a =
    a
; action66 pos a b =
    ExprBinary pos a Mult b
; action67 pos a =
    a
; action70 pos e =
    e
; action71 pos n =
    ExprVar   pos n
; action72 pos n =
    ExprConst pos n
; action75 pos n =
    ConstNamed pos n
; action76 pos n =
    ConstInt   pos n
; action79 pos c =
    Expect pos c
; action80 pos c =
    Notify pos c
; action81 pos e =
    Guard  pos e
; action84 pos tests =
    TestSuite {tests}
; action87 pos t =
    [t]
; action88 pos t ts =
     t : ts
}

parseProgram :: FilePath -> IO (Either LexerError (Either (Pos, [String]) Program))
parseProgram filepath = do
  text <- Text.readFile filepath
  case lexText filepath text ["(", ")", "*", "+", ",", "-", "->",
                              ".", "<-", "=", "=>", "expect", "guard", "notify", "test", "~"] of
    Left  err   -> pure (Left err)
    Right input -> pure (Right (__runProgram SProgram176 input Nil))
data StTestSuite :: [Kind.Type] -> Kind.Type where
  STestSuite0 :: StTestSuite (Expr : a)
  STestSuite1 :: StTestSuite (Expr : a)
  STestSuite2 :: StTestSuite (Expr : a)
  STestSuite3 :: StTestSuite (Expr : () : Expr : a)
  STestSuite4 :: StTestSuite (Expr : () : Expr : a)
  STestSuite5 :: StTestSuite (Expr : a)
  STestSuite6 :: StTestSuite (Expr : a)
  STestSuite7 :: StTestSuite (Expr : a)
  STestSuite8 :: StTestSuite (Expr : a)
  STestSuite9 :: StTestSuite (Expr : () : Expr : a)
  STestSuite10 :: StTestSuite (Expr : () : Expr : a)
  STestSuite11 :: StTestSuite (Expr : () : Expr : a)
  STestSuite12 :: StTestSuite (Expr : () : Expr : a)
  STestSuite13 :: StTestSuite (Expr : a)
  STestSuite14 :: StTestSuite (Expr : a)
  STestSuite15 :: StTestSuite (Expr : a)
  STestSuite16 :: StTestSuite (Expr : a)
  STestSuite17 :: StTestSuite (() : a)
  STestSuite18 :: StTestSuite (() : a)
  STestSuite19 :: StTestSuite (() : a)
  STestSuite20 :: StTestSuite (() : a)
  STestSuite21 :: StTestSuite (Text : a)
  STestSuite22 :: StTestSuite (Text : a)
  STestSuite23 :: StTestSuite (Text : a)
  STestSuite24 :: StTestSuite (Text : a)
  STestSuite25 :: StTestSuite (Const : a)
  STestSuite26 :: StTestSuite (Const : a)
  STestSuite27 :: StTestSuite (Const : a)
  STestSuite28 :: StTestSuite (Const : a)
  STestSuite29 :: StTestSuite (Text : a)
  STestSuite30 :: StTestSuite (Text : a)
  STestSuite31 :: StTestSuite (Text : a)
  STestSuite32 :: StTestSuite (Text : a)
  STestSuite33 :: StTestSuite (Integer : a)
  STestSuite34 :: StTestSuite (Integer : a)
  STestSuite35 :: StTestSuite (Integer : a)
  STestSuite36 :: StTestSuite (Integer : a)
  STestSuite37 :: StTestSuite (() : Expr : a)
  STestSuite38 :: StTestSuite (() : Expr : a)
  STestSuite39 :: StTestSuite (() : Expr : a)
  STestSuite40 :: StTestSuite (() : Expr : a)
  STestSuite41 :: StTestSuite (() : Expr : a)
  STestSuite42 :: StTestSuite (() : Expr : a)
  STestSuite43 :: StTestSuite (() : Expr : a)
  STestSuite44 :: StTestSuite (() : Expr : a)
  STestSuite45 :: StTestSuite (() : Expr : a)
  STestSuite46 :: StTestSuite (() : Expr : a)
  STestSuite47 :: StTestSuite (() : Expr : a)
  STestSuite48 :: StTestSuite (Expr : () : a)
  STestSuite49 :: StTestSuite (Expr : () : a)
  STestSuite50 :: StTestSuite (Expr : () : a)
  STestSuite51 :: StTestSuite (Expr : () : a)
  STestSuite52 :: StTestSuite ([Expr] : () : Expr : a)
  STestSuite53 :: StTestSuite (Expr : () : Expr : a)
  STestSuite54 :: StTestSuite (Expr : () : Expr : a)
  STestSuite55 :: StTestSuite (Expr : () : Expr : a)
  STestSuite56 :: StTestSuite (Expr : () : Expr : a)
  STestSuite57 :: StTestSuite (() : Expr : () : a)
  STestSuite58 :: StTestSuite (() : Expr : () : a)
  STestSuite59 :: StTestSuite (() : Expr : () : a)
  STestSuite60 :: StTestSuite (() : Expr : () : a)
  STestSuite61 :: StTestSuite (Expr : a)
  STestSuite62 :: StTestSuite (Expr : a)
  STestSuite63 :: StTestSuite (Expr : () : Expr : a)
  STestSuite64 :: StTestSuite (Expr : () : Expr : a)
  STestSuite65 :: StTestSuite (Expr : a)
  STestSuite66 :: StTestSuite (Expr : a)
  STestSuite67 :: StTestSuite (() : a)
  STestSuite68 :: StTestSuite (() : a)
  STestSuite69 :: StTestSuite (Text : a)
  STestSuite70 :: StTestSuite (Text : a)
  STestSuite71 :: StTestSuite (Const : a)
  STestSuite72 :: StTestSuite (Const : a)
  STestSuite73 :: StTestSuite (Text : a)
  STestSuite74 :: StTestSuite (Text : a)
  STestSuite75 :: StTestSuite (Integer : a)
  STestSuite76 :: StTestSuite (Integer : a)
  STestSuite77 :: StTestSuite (() : Expr : a)
  STestSuite78 :: StTestSuite (() : Expr : a)
  STestSuite79 :: StTestSuite (Expr : () : a)
  STestSuite80 :: StTestSuite (Expr : () : a)
  STestSuite81 :: StTestSuite (Expr : () : Expr : a)
  STestSuite82 :: StTestSuite (Expr : () : Expr : a)
  STestSuite83 :: StTestSuite (() : Expr : () : a)
  STestSuite84 :: StTestSuite (() : Expr : () : a)
  STestSuite85 :: StTestSuite (Expr : a)
  STestSuite86 :: StTestSuite (Expr : () : Expr : a)
  STestSuite87 :: StTestSuite (() : Expr : a)
  STestSuite88 :: StTestSuite (() : Expr : a)
  STestSuite89 :: StTestSuite (Text : a)
  STestSuite90 :: StTestSuite (() : a)
  STestSuite91 :: StTestSuite (() : a)
  STestSuite92 :: StTestSuite (() : a)
  STestSuite93 :: StTestSuite (() : a)
  STestSuite94 :: StTestSuite ([Expr] : Text : a)
  STestSuite95 :: StTestSuite (() : () : a)
  STestSuite96 :: StTestSuite ([Expr] : () : a)
  STestSuite97 :: StTestSuite (() : Expr : a)
  STestSuite98 :: StTestSuite (Call : () : a)
  STestSuite99 :: StTestSuite (Call : () : a)
  STestSuite100 :: StTestSuite (Expr : () : a)
  STestSuite101 :: StTestSuite (() : [Expr] : () : a)
  STestSuite102 :: StTestSuite (a)
  STestSuite103 :: StTestSuite (TestSuite : a)
  STestSuite104 :: StTestSuite (() : a)
  STestSuite105 :: StTestSuite (Test : a)
  STestSuite106 :: StTestSuite ([Test] : () : a)
  STestSuite107 :: StTestSuite ([Test] : Test : a)

__gotoAddForTestSuite :: ([Lexeme], Pos) -> Expr -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoAddForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite17 -> __runTestSuite STestSuite2 toks (term :> stk)
  STestSuite18 -> __runTestSuite STestSuite2 toks (term :> stk)
  STestSuite19 -> __runTestSuite STestSuite2 toks (term :> stk)
  STestSuite20 -> __runTestSuite STestSuite2 toks (term :> stk)
  STestSuite37 -> __runTestSuite STestSuite1 toks (term :> stk)
  STestSuite38 -> __runTestSuite STestSuite3 toks (term :> stk)
  STestSuite39 -> __runTestSuite STestSuite4 toks (term :> stk)
  STestSuite67 -> __runTestSuite STestSuite2 toks (term :> stk)
  STestSuite68 -> __runTestSuite STestSuite2 toks (term :> stk)
  STestSuite90 -> __runTestSuite STestSuite1 toks (term :> stk)
  STestSuite93 -> __runTestSuite STestSuite85 toks (term :> stk)
  STestSuite97 -> __runTestSuite STestSuite86 toks (term :> stk)
  _ -> error ""

__gotoCallForTestSuite :: ([Lexeme], Pos) -> Call -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoCallForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite91 -> __runTestSuite STestSuite98 toks (term :> stk)
  STestSuite92 -> __runTestSuite STestSuite99 toks (term :> stk)
  _ -> error ""

__gotoChangeForTestSuite :: ([Lexeme], Pos) -> Change -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoChangeForTestSuite toks term stk@(state, _, _) = case state of
  _ -> error ""

__gotoChangesForTestSuite :: ([Lexeme], Pos) -> [Change] -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoChangesForTestSuite toks term stk@(state, _, _) = case state of
  _ -> error ""

__gotoClauseForTestSuite :: ([Lexeme], Pos) -> Clause -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoClauseForTestSuite toks term stk@(state, _, _) = case state of
  _ -> error ""

__gotoCondForTestSuite :: ([Lexeme], Pos) -> Cond -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoCondForTestSuite toks term stk@(state, _, _) = case state of
  _ -> error ""

__gotoCondsForTestSuite :: ([Lexeme], Pos) -> [Cond] -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoCondsForTestSuite toks term stk@(state, _, _) = case state of
  _ -> error ""

__gotoConstForTestSuite :: ([Lexeme], Pos) -> Const -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoConstForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite17 -> __runTestSuite STestSuite28 toks (term :> stk)
  STestSuite18 -> __runTestSuite STestSuite28 toks (term :> stk)
  STestSuite19 -> __runTestSuite STestSuite28 toks (term :> stk)
  STestSuite20 -> __runTestSuite STestSuite28 toks (term :> stk)
  STestSuite37 -> __runTestSuite STestSuite27 toks (term :> stk)
  STestSuite38 -> __runTestSuite STestSuite25 toks (term :> stk)
  STestSuite39 -> __runTestSuite STestSuite26 toks (term :> stk)
  STestSuite40 -> __runTestSuite STestSuite25 toks (term :> stk)
  STestSuite41 -> __runTestSuite STestSuite26 toks (term :> stk)
  STestSuite42 -> __runTestSuite STestSuite27 toks (term :> stk)
  STestSuite43 -> __runTestSuite STestSuite28 toks (term :> stk)
  STestSuite44 -> __runTestSuite STestSuite25 toks (term :> stk)
  STestSuite45 -> __runTestSuite STestSuite26 toks (term :> stk)
  STestSuite46 -> __runTestSuite STestSuite27 toks (term :> stk)
  STestSuite47 -> __runTestSuite STestSuite28 toks (term :> stk)
  STestSuite67 -> __runTestSuite STestSuite28 toks (term :> stk)
  STestSuite68 -> __runTestSuite STestSuite28 toks (term :> stk)
  STestSuite77 -> __runTestSuite STestSuite71 toks (term :> stk)
  STestSuite78 -> __runTestSuite STestSuite72 toks (term :> stk)
  STestSuite87 -> __runTestSuite STestSuite71 toks (term :> stk)
  STestSuite88 -> __runTestSuite STestSuite72 toks (term :> stk)
  STestSuite90 -> __runTestSuite STestSuite27 toks (term :> stk)
  STestSuite93 -> __runTestSuite STestSuite71 toks (term :> stk)
  STestSuite97 -> __runTestSuite STestSuite72 toks (term :> stk)
  _ -> error ""

__gotoEffectForTestSuite :: ([Lexeme], Pos) -> Effect -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoEffectForTestSuite toks term stk@(state, _, _) = case state of
  _ -> error ""

__gotoExprForTestSuite :: ([Lexeme], Pos) -> Expr -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoExprForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite17 -> __runTestSuite STestSuite48 toks (term :> stk)
  STestSuite18 -> __runTestSuite STestSuite49 toks (term :> stk)
  STestSuite19 -> __runTestSuite STestSuite50 toks (term :> stk)
  STestSuite20 -> __runTestSuite STestSuite51 toks (term :> stk)
  STestSuite37 -> __runTestSuite STestSuite0 toks (term :> stk)
  STestSuite67 -> __runTestSuite STestSuite79 toks (term :> stk)
  STestSuite68 -> __runTestSuite STestSuite80 toks (term :> stk)
  STestSuite90 -> __runTestSuite STestSuite0 toks (term :> stk)
  STestSuite93 -> __runTestSuite STestSuite100 toks (term :> stk)
  _ -> error ""

__gotoExprs1ForTestSuite :: ([Lexeme], Pos) -> [Expr] -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoExprs1ForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite37 -> __runTestSuite STestSuite52 toks (term :> stk)
  STestSuite90 -> __runTestSuite STestSuite96 toks (term :> stk)
  _ -> error ""

__gotoMultForTestSuite :: ([Lexeme], Pos) -> Expr -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoMultForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite17 -> __runTestSuite STestSuite8 toks (term :> stk)
  STestSuite18 -> __runTestSuite STestSuite8 toks (term :> stk)
  STestSuite19 -> __runTestSuite STestSuite8 toks (term :> stk)
  STestSuite20 -> __runTestSuite STestSuite8 toks (term :> stk)
  STestSuite37 -> __runTestSuite STestSuite7 toks (term :> stk)
  STestSuite38 -> __runTestSuite STestSuite5 toks (term :> stk)
  STestSuite39 -> __runTestSuite STestSuite6 toks (term :> stk)
  STestSuite40 -> __runTestSuite STestSuite9 toks (term :> stk)
  STestSuite41 -> __runTestSuite STestSuite10 toks (term :> stk)
  STestSuite42 -> __runTestSuite STestSuite11 toks (term :> stk)
  STestSuite43 -> __runTestSuite STestSuite12 toks (term :> stk)
  STestSuite67 -> __runTestSuite STestSuite8 toks (term :> stk)
  STestSuite68 -> __runTestSuite STestSuite8 toks (term :> stk)
  STestSuite87 -> __runTestSuite STestSuite63 toks (term :> stk)
  STestSuite88 -> __runTestSuite STestSuite64 toks (term :> stk)
  STestSuite90 -> __runTestSuite STestSuite7 toks (term :> stk)
  STestSuite93 -> __runTestSuite STestSuite61 toks (term :> stk)
  STestSuite97 -> __runTestSuite STestSuite62 toks (term :> stk)
  _ -> error ""

__gotoProgramForTestSuite :: ([Lexeme], Pos) -> Program -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoProgramForTestSuite toks term stk@(state, _, _) = case state of
  _ -> error ""

__gotoStmtForTestSuite :: ([Lexeme], Pos) -> Stmt -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoStmtForTestSuite toks term stk@(state, _, _) = case state of
  _ -> error ""

__gotoStmtsForTestSuite :: ([Lexeme], Pos) -> [Stmt] -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoStmtsForTestSuite toks term stk@(state, _, _) = case state of
  _ -> error ""

__gotoTermForTestSuite :: ([Lexeme], Pos) -> Expr -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoTermForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite17 -> __runTestSuite STestSuite16 toks (term :> stk)
  STestSuite18 -> __runTestSuite STestSuite16 toks (term :> stk)
  STestSuite19 -> __runTestSuite STestSuite16 toks (term :> stk)
  STestSuite20 -> __runTestSuite STestSuite16 toks (term :> stk)
  STestSuite37 -> __runTestSuite STestSuite15 toks (term :> stk)
  STestSuite38 -> __runTestSuite STestSuite13 toks (term :> stk)
  STestSuite39 -> __runTestSuite STestSuite14 toks (term :> stk)
  STestSuite40 -> __runTestSuite STestSuite13 toks (term :> stk)
  STestSuite41 -> __runTestSuite STestSuite14 toks (term :> stk)
  STestSuite42 -> __runTestSuite STestSuite15 toks (term :> stk)
  STestSuite43 -> __runTestSuite STestSuite16 toks (term :> stk)
  STestSuite44 -> __runTestSuite STestSuite53 toks (term :> stk)
  STestSuite45 -> __runTestSuite STestSuite54 toks (term :> stk)
  STestSuite46 -> __runTestSuite STestSuite55 toks (term :> stk)
  STestSuite47 -> __runTestSuite STestSuite56 toks (term :> stk)
  STestSuite67 -> __runTestSuite STestSuite16 toks (term :> stk)
  STestSuite68 -> __runTestSuite STestSuite16 toks (term :> stk)
  STestSuite77 -> __runTestSuite STestSuite81 toks (term :> stk)
  STestSuite78 -> __runTestSuite STestSuite82 toks (term :> stk)
  STestSuite87 -> __runTestSuite STestSuite65 toks (term :> stk)
  STestSuite88 -> __runTestSuite STestSuite66 toks (term :> stk)
  STestSuite90 -> __runTestSuite STestSuite15 toks (term :> stk)
  STestSuite93 -> __runTestSuite STestSuite65 toks (term :> stk)
  STestSuite97 -> __runTestSuite STestSuite66 toks (term :> stk)
  _ -> error ""

__gotoTestForTestSuite :: ([Lexeme], Pos) -> Test -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoTestForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite104 -> __runTestSuite STestSuite105 toks (term :> stk)
  STestSuite105 -> __runTestSuite STestSuite105 toks (term :> stk)
  _ -> error ""

__gotoTestSuiteForTestSuite :: ([Lexeme], Pos) -> TestSuite -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoTestSuiteForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite102 -> __runTestSuite STestSuite103 toks (term :> stk)
  _ -> error ""

__gotoTestsForTestSuite :: ([Lexeme], Pos) -> [Test] -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoTestsForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite104 -> __runTestSuite STestSuite106 toks (term :> stk)
  STestSuite105 -> __runTestSuite STestSuite107 toks (term :> stk)
  _ -> error ""

__gotoTupleForTestSuite :: ([Lexeme], Pos) -> [Expr] -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoTupleForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite89 -> __runTestSuite STestSuite94 toks (term :> stk)
  _ -> error ""

__runTestSuite :: StTestSuite a -> ([Lexeme], Pos) -> Stack' StTestSuite a -> Either (Pos, [String]) TestSuite
__runTestSuite = \cases {
; STestSuite0 ((__p,  ",") : __input, __end) __stk ->
    __runTestSuite STestSuite37 (__input, __end) (() :> (STestSuite0, __p, __stk))
; STestSuite1 ((__p,  "+") : __input, __end) __stk ->
    __runTestSuite STestSuite42 (__input, __end) (() :> (STestSuite1, __p, __stk))
; STestSuite1 ((__p,  "=") : __input, __end) __stk ->
    __runTestSuite STestSuite39 (__input, __end) (() :> (STestSuite1, __p, __stk))
; STestSuite2 ((__p,  "+") : __input, __end) __stk ->
    __runTestSuite STestSuite43 (__input, __end) (() :> (STestSuite2, __p, __stk))
; STestSuite2 ((__p,  "=") : __input, __end) __stk ->
    __runTestSuite STestSuite38 (__input, __end) (() :> (STestSuite2, __p, __stk))
; STestSuite3 ((__p,  "+") : __input, __end) __stk ->
    __runTestSuite STestSuite40 (__input, __end) (() :> (STestSuite3, __p, __stk))
; STestSuite4 ((__p,  "+") : __input, __end) __stk ->
    __runTestSuite STestSuite41 (__input, __end) (() :> (STestSuite4, __p, __stk))
; STestSuite5 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite44 (__input, __end) (() :> (STestSuite5, __p, __stk))
; STestSuite6 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite45 (__input, __end) (() :> (STestSuite6, __p, __stk))
; STestSuite7 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite46 (__input, __end) (() :> (STestSuite7, __p, __stk))
; STestSuite8 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite47 (__input, __end) (() :> (STestSuite8, __p, __stk))
; STestSuite9 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite44 (__input, __end) (() :> (STestSuite9, __p, __stk))
; STestSuite10 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite45 (__input, __end) (() :> (STestSuite10, __p, __stk))
; STestSuite11 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite46 (__input, __end) (() :> (STestSuite11, __p, __stk))
; STestSuite12 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite47 (__input, __end) (() :> (STestSuite12, __p, __stk))
; STestSuite17 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite20 (__input, __end) (() :> (STestSuite17, __p, __stk))
; STestSuite17 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite24 (__input, __end) (n :> (STestSuite17, __p, __stk))
; STestSuite17 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite32 (__input, __end) (n :> (STestSuite17, __p, __stk))
; STestSuite17 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite36 (__input, __end) (n :> (STestSuite17, __p, __stk))
; STestSuite18 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite20 (__input, __end) (() :> (STestSuite18, __p, __stk))
; STestSuite18 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite24 (__input, __end) (n :> (STestSuite18, __p, __stk))
; STestSuite18 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite32 (__input, __end) (n :> (STestSuite18, __p, __stk))
; STestSuite18 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite36 (__input, __end) (n :> (STestSuite18, __p, __stk))
; STestSuite19 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite20 (__input, __end) (() :> (STestSuite19, __p, __stk))
; STestSuite19 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite24 (__input, __end) (n :> (STestSuite19, __p, __stk))
; STestSuite19 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite32 (__input, __end) (n :> (STestSuite19, __p, __stk))
; STestSuite19 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite36 (__input, __end) (n :> (STestSuite19, __p, __stk))
; STestSuite20 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite20 (__input, __end) (() :> (STestSuite20, __p, __stk))
; STestSuite20 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite24 (__input, __end) (n :> (STestSuite20, __p, __stk))
; STestSuite20 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite32 (__input, __end) (n :> (STestSuite20, __p, __stk))
; STestSuite20 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite36 (__input, __end) (n :> (STestSuite20, __p, __stk))
; STestSuite37 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite19 (__input, __end) (() :> (STestSuite37, __p, __stk))
; STestSuite37 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite23 (__input, __end) (n :> (STestSuite37, __p, __stk))
; STestSuite37 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite31 (__input, __end) (n :> (STestSuite37, __p, __stk))
; STestSuite37 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite35 (__input, __end) (n :> (STestSuite37, __p, __stk))
; STestSuite38 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite17 (__input, __end) (() :> (STestSuite38, __p, __stk))
; STestSuite38 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite21 (__input, __end) (n :> (STestSuite38, __p, __stk))
; STestSuite38 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite29 (__input, __end) (n :> (STestSuite38, __p, __stk))
; STestSuite38 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite33 (__input, __end) (n :> (STestSuite38, __p, __stk))
; STestSuite39 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite18 (__input, __end) (() :> (STestSuite39, __p, __stk))
; STestSuite39 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite22 (__input, __end) (n :> (STestSuite39, __p, __stk))
; STestSuite39 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite30 (__input, __end) (n :> (STestSuite39, __p, __stk))
; STestSuite39 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite34 (__input, __end) (n :> (STestSuite39, __p, __stk))
; STestSuite40 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite17 (__input, __end) (() :> (STestSuite40, __p, __stk))
; STestSuite40 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite21 (__input, __end) (n :> (STestSuite40, __p, __stk))
; STestSuite40 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite29 (__input, __end) (n :> (STestSuite40, __p, __stk))
; STestSuite40 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite33 (__input, __end) (n :> (STestSuite40, __p, __stk))
; STestSuite41 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite18 (__input, __end) (() :> (STestSuite41, __p, __stk))
; STestSuite41 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite22 (__input, __end) (n :> (STestSuite41, __p, __stk))
; STestSuite41 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite30 (__input, __end) (n :> (STestSuite41, __p, __stk))
; STestSuite41 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite34 (__input, __end) (n :> (STestSuite41, __p, __stk))
; STestSuite42 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite19 (__input, __end) (() :> (STestSuite42, __p, __stk))
; STestSuite42 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite23 (__input, __end) (n :> (STestSuite42, __p, __stk))
; STestSuite42 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite31 (__input, __end) (n :> (STestSuite42, __p, __stk))
; STestSuite42 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite35 (__input, __end) (n :> (STestSuite42, __p, __stk))
; STestSuite43 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite20 (__input, __end) (() :> (STestSuite43, __p, __stk))
; STestSuite43 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite24 (__input, __end) (n :> (STestSuite43, __p, __stk))
; STestSuite43 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite32 (__input, __end) (n :> (STestSuite43, __p, __stk))
; STestSuite43 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite36 (__input, __end) (n :> (STestSuite43, __p, __stk))
; STestSuite44 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite17 (__input, __end) (() :> (STestSuite44, __p, __stk))
; STestSuite44 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite21 (__input, __end) (n :> (STestSuite44, __p, __stk))
; STestSuite44 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite29 (__input, __end) (n :> (STestSuite44, __p, __stk))
; STestSuite44 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite33 (__input, __end) (n :> (STestSuite44, __p, __stk))
; STestSuite45 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite18 (__input, __end) (() :> (STestSuite45, __p, __stk))
; STestSuite45 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite22 (__input, __end) (n :> (STestSuite45, __p, __stk))
; STestSuite45 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite30 (__input, __end) (n :> (STestSuite45, __p, __stk))
; STestSuite45 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite34 (__input, __end) (n :> (STestSuite45, __p, __stk))
; STestSuite46 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite19 (__input, __end) (() :> (STestSuite46, __p, __stk))
; STestSuite46 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite23 (__input, __end) (n :> (STestSuite46, __p, __stk))
; STestSuite46 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite31 (__input, __end) (n :> (STestSuite46, __p, __stk))
; STestSuite46 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite35 (__input, __end) (n :> (STestSuite46, __p, __stk))
; STestSuite47 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite20 (__input, __end) (() :> (STestSuite47, __p, __stk))
; STestSuite47 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite24 (__input, __end) (n :> (STestSuite47, __p, __stk))
; STestSuite47 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite32 (__input, __end) (n :> (STestSuite47, __p, __stk))
; STestSuite47 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite36 (__input, __end) (n :> (STestSuite47, __p, __stk))
; STestSuite48 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite57 (__input, __end) (() :> (STestSuite48, __p, __stk))
; STestSuite49 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite58 (__input, __end) (() :> (STestSuite49, __p, __stk))
; STestSuite50 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite59 (__input, __end) (() :> (STestSuite50, __p, __stk))
; STestSuite51 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite60 (__input, __end) (() :> (STestSuite51, __p, __stk))
; STestSuite61 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite77 (__input, __end) (() :> (STestSuite61, __p, __stk))
; STestSuite62 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite78 (__input, __end) (() :> (STestSuite62, __p, __stk))
; STestSuite63 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite77 (__input, __end) (() :> (STestSuite63, __p, __stk))
; STestSuite64 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite78 (__input, __end) (() :> (STestSuite64, __p, __stk))
; STestSuite67 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite20 (__input, __end) (() :> (STestSuite67, __p, __stk))
; STestSuite67 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite24 (__input, __end) (n :> (STestSuite67, __p, __stk))
; STestSuite67 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite32 (__input, __end) (n :> (STestSuite67, __p, __stk))
; STestSuite67 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite36 (__input, __end) (n :> (STestSuite67, __p, __stk))
; STestSuite68 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite20 (__input, __end) (() :> (STestSuite68, __p, __stk))
; STestSuite68 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite24 (__input, __end) (n :> (STestSuite68, __p, __stk))
; STestSuite68 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite32 (__input, __end) (n :> (STestSuite68, __p, __stk))
; STestSuite68 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite36 (__input, __end) (n :> (STestSuite68, __p, __stk))
; STestSuite77 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite67 (__input, __end) (() :> (STestSuite77, __p, __stk))
; STestSuite77 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite69 (__input, __end) (n :> (STestSuite77, __p, __stk))
; STestSuite77 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite73 (__input, __end) (n :> (STestSuite77, __p, __stk))
; STestSuite77 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite75 (__input, __end) (n :> (STestSuite77, __p, __stk))
; STestSuite78 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite68 (__input, __end) (() :> (STestSuite78, __p, __stk))
; STestSuite78 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite70 (__input, __end) (n :> (STestSuite78, __p, __stk))
; STestSuite78 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite74 (__input, __end) (n :> (STestSuite78, __p, __stk))
; STestSuite78 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite76 (__input, __end) (n :> (STestSuite78, __p, __stk))
; STestSuite79 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite83 (__input, __end) (() :> (STestSuite79, __p, __stk))
; STestSuite80 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite84 (__input, __end) (() :> (STestSuite80, __p, __stk))
; STestSuite85 ((__p,  "+") : __input, __end) __stk ->
    __runTestSuite STestSuite87 (__input, __end) (() :> (STestSuite85, __p, __stk))
; STestSuite85 ((__p,  "=") : __input, __end) __stk ->
    __runTestSuite STestSuite97 (__input, __end) (() :> (STestSuite85, __p, __stk))
; STestSuite86 ((__p,  "+") : __input, __end) __stk ->
    __runTestSuite STestSuite88 (__input, __end) (() :> (STestSuite86, __p, __stk))
; STestSuite87 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite67 (__input, __end) (() :> (STestSuite87, __p, __stk))
; STestSuite87 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite69 (__input, __end) (n :> (STestSuite87, __p, __stk))
; STestSuite87 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite73 (__input, __end) (n :> (STestSuite87, __p, __stk))
; STestSuite87 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite75 (__input, __end) (n :> (STestSuite87, __p, __stk))
; STestSuite88 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite68 (__input, __end) (() :> (STestSuite88, __p, __stk))
; STestSuite88 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite70 (__input, __end) (n :> (STestSuite88, __p, __stk))
; STestSuite88 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite74 (__input, __end) (n :> (STestSuite88, __p, __stk))
; STestSuite88 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite76 (__input, __end) (n :> (STestSuite88, __p, __stk))
; STestSuite89 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite90 (__input, __end) (() :> (STestSuite89, __p, __stk))
; STestSuite90 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite19 (__input, __end) (() :> (STestSuite90, __p, __stk))
; STestSuite90 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite95 (__input, __end) (() :> (STestSuite90, __p, __stk))
; STestSuite90 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite23 (__input, __end) (n :> (STestSuite90, __p, __stk))
; STestSuite90 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite31 (__input, __end) (n :> (STestSuite90, __p, __stk))
; STestSuite90 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite35 (__input, __end) (n :> (STestSuite90, __p, __stk))
; STestSuite91 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite89 (__input, __end) (n :> (STestSuite91, __p, __stk))
; STestSuite92 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite89 (__input, __end) (n :> (STestSuite92, __p, __stk))
; STestSuite93 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite67 (__input, __end) (() :> (STestSuite93, __p, __stk))
; STestSuite93 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite69 (__input, __end) (n :> (STestSuite93, __p, __stk))
; STestSuite93 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite73 (__input, __end) (n :> (STestSuite93, __p, __stk))
; STestSuite93 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite75 (__input, __end) (n :> (STestSuite93, __p, __stk))
; STestSuite96 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite101 (__input, __end) (() :> (STestSuite96, __p, __stk))
; STestSuite97 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite68 (__input, __end) (() :> (STestSuite97, __p, __stk))
; STestSuite97 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite70 (__input, __end) (n :> (STestSuite97, __p, __stk))
; STestSuite97 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite74 (__input, __end) (n :> (STestSuite97, __p, __stk))
; STestSuite97 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite76 (__input, __end) (n :> (STestSuite97, __p, __stk))
; STestSuite102 ((__p,  "test") : __input, __end) __stk ->
    __runTestSuite STestSuite104 (__input, __end) (() :> (STestSuite102, __p, __stk))
; STestSuite104 ((__p,  "expect") : __input, __end) __stk ->
    __runTestSuite STestSuite91 (__input, __end) (() :> (STestSuite104, __p, __stk))
; STestSuite104 ((__p,  "guard") : __input, __end) __stk ->
    __runTestSuite STestSuite93 (__input, __end) (() :> (STestSuite104, __p, __stk))
; STestSuite104 ((__p,  "notify") : __input, __end) __stk ->
    __runTestSuite STestSuite92 (__input, __end) (() :> (STestSuite104, __p, __stk))
; STestSuite105 ((__p,  "expect") : __input, __end) __stk ->
    __runTestSuite STestSuite91 (__input, __end) (() :> (STestSuite105, __p, __stk))
; STestSuite105 ((__p,  "guard") : __input, __end) __stk ->
    __runTestSuite STestSuite93 (__input, __end) (() :> (STestSuite105, __p, __stk))
; STestSuite105 ((__p,  "notify") : __input, __end) __stk ->
    __runTestSuite STestSuite92 (__input, __end) (() :> (STestSuite105, __p, __stk))
-- lookahead ), entity Exprs1
; STestSuite0 ((__p,  ")") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoExprs1ForTestSuite ((__p,  ")") : __input, __end) (action55 __pos e) __stk
-- lookahead ), entity Expr
; STestSuite1 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForTestSuite ((__p,  ")") : __input, __end) (action59 __pos a) __stk
-- lookahead ,, entity Expr
; STestSuite1 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForTestSuite ((__p,  ",") : __input, __end) (action59 __pos a) __stk
-- lookahead ), entity Expr
; STestSuite2 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForTestSuite ((__p,  ")") : __input, __end) (action59 __pos a) __stk
-- lookahead ), entity Expr
; STestSuite3 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForTestSuite ((__p,  ")") : __input, __end) (action58 __pos a
                                                                          b) __stk
-- lookahead ), entity Expr
; STestSuite4 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForTestSuite ((__p,  ")") : __input, __end) (action58 __pos a
                                                                          b) __stk
-- lookahead ,, entity Expr
; STestSuite4 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForTestSuite ((__p,  ",") : __input, __end) (action58 __pos a
                                                                          b) __stk
-- lookahead ), entity Add
; STestSuite5 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; STestSuite5 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead ), entity Add
; STestSuite6 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; STestSuite6 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead ,, entity Add
; STestSuite6 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead ), entity Add
; STestSuite7 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; STestSuite7 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead ,, entity Add
; STestSuite7 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead =, entity Add
; STestSuite7 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "=") : __input, __end) (action63 __pos a) __stk
-- lookahead ), entity Add
; STestSuite8 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; STestSuite8 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead =, entity Add
; STestSuite8 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "=") : __input, __end) (action63 __pos a) __stk
-- lookahead ), entity Add
; STestSuite9 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead +, entity Add
; STestSuite9 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead ), entity Add
; STestSuite10 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead +, entity Add
; STestSuite10 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead ,, entity Add
; STestSuite10 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  ",") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead ), entity Add
; STestSuite11 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead +, entity Add
; STestSuite11 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead ,, entity Add
; STestSuite11 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  ",") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead =, entity Add
; STestSuite11 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "=") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead ), entity Add
; STestSuite12 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead +, entity Add
; STestSuite12 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead =, entity Add
; STestSuite12 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "=") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead ), entity Mult
; STestSuite13 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; STestSuite13 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; STestSuite13 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead ), entity Mult
; STestSuite14 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; STestSuite14 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; STestSuite14 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead ,, entity Mult
; STestSuite14 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead ), entity Mult
; STestSuite15 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; STestSuite15 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; STestSuite15 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead ,, entity Mult
; STestSuite15 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead =, entity Mult
; STestSuite15 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "=") : __input, __end) (action67 __pos a) __stk
-- lookahead ), entity Mult
; STestSuite16 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; STestSuite16 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; STestSuite16 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead =, entity Mult
; STestSuite16 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "=") : __input, __end) (action67 __pos a) __stk
-- lookahead ), entity Term
; STestSuite21 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; STestSuite21 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; STestSuite21 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead ), entity Term
; STestSuite22 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; STestSuite22 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; STestSuite22 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead ,, entity Term
; STestSuite22 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead ), entity Term
; STestSuite23 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; STestSuite23 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; STestSuite23 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead ,, entity Term
; STestSuite23 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead =, entity Term
; STestSuite23 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action71 __pos n) __stk
-- lookahead ), entity Term
; STestSuite24 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; STestSuite24 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; STestSuite24 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead =, entity Term
; STestSuite24 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action71 __pos n) __stk
-- lookahead ), entity Term
; STestSuite25 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; STestSuite25 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; STestSuite25 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead ), entity Term
; STestSuite26 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; STestSuite26 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; STestSuite26 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead ,, entity Term
; STestSuite26 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead ), entity Term
; STestSuite27 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; STestSuite27 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; STestSuite27 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead ,, entity Term
; STestSuite27 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead =, entity Term
; STestSuite27 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action72 __pos n) __stk
-- lookahead ), entity Term
; STestSuite28 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; STestSuite28 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; STestSuite28 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead =, entity Term
; STestSuite28 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action72 __pos n) __stk
-- lookahead ), entity Const
; STestSuite29 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; STestSuite29 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; STestSuite29 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ), entity Const
; STestSuite30 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; STestSuite30 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; STestSuite30 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ,, entity Const
; STestSuite30 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead ), entity Const
; STestSuite31 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; STestSuite31 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; STestSuite31 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ,, entity Const
; STestSuite31 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead =, entity Const
; STestSuite31 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead ), entity Const
; STestSuite32 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; STestSuite32 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; STestSuite32 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead =, entity Const
; STestSuite32 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead ), entity Const
; STestSuite33 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; STestSuite33 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; STestSuite33 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead ), entity Const
; STestSuite34 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; STestSuite34 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; STestSuite34 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead ,, entity Const
; STestSuite34 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead ), entity Const
; STestSuite35 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; STestSuite35 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; STestSuite35 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead ,, entity Const
; STestSuite35 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead =, entity Const
; STestSuite35 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "=") : __input, __end) (action76 __pos n) __stk
-- lookahead ), entity Const
; STestSuite36 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; STestSuite36 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; STestSuite36 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead =, entity Const
; STestSuite36 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "=") : __input, __end) (action76 __pos n) __stk
-- lookahead ), entity Exprs1
; STestSuite52 ((__p,  ")") : __input, __end) ((es :> (_, _, _ :> (_, _, e :> __stk@(_, __pos, _))))) ->
    __gotoExprs1ForTestSuite ((__p,  ")") : __input, __end) (action54 __pos e
                                                                            es) __stk
-- lookahead ), entity Mult
; STestSuite53 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead *, entity Mult
; STestSuite53 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead +, entity Mult
; STestSuite53 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead ), entity Mult
; STestSuite54 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead *, entity Mult
; STestSuite54 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead +, entity Mult
; STestSuite54 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead ,, entity Mult
; STestSuite54 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  ",") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead ), entity Mult
; STestSuite55 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead *, entity Mult
; STestSuite55 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead +, entity Mult
; STestSuite55 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead ,, entity Mult
; STestSuite55 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  ",") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead =, entity Mult
; STestSuite55 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "=") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead ), entity Mult
; STestSuite56 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead *, entity Mult
; STestSuite56 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead +, entity Mult
; STestSuite56 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead =, entity Mult
; STestSuite56 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "=") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead ), entity Term
; STestSuite57 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; STestSuite57 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; STestSuite57 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead ), entity Term
; STestSuite58 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; STestSuite58 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; STestSuite58 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead ,, entity Term
; STestSuite58 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead ), entity Term
; STestSuite59 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; STestSuite59 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; STestSuite59 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead ,, entity Term
; STestSuite59 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead =, entity Term
; STestSuite59 ((__p,  "=") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action70 __pos e) __stk
-- lookahead ), entity Term
; STestSuite60 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; STestSuite60 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; STestSuite60 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead =, entity Term
; STestSuite60 ((__p,  "=") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Add
; STestSuite61 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead =, entity Add
; STestSuite61 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "=") : __input, __end) (action63 __pos a) __stk
-- lookahead expect, entity Add
; STestSuite61 ((__p,  "expect") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "expect") : __input, __end) (action63 __pos a) __stk
-- lookahead guard, entity Add
; STestSuite61 ((__p,  "guard") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "guard") : __input, __end) (action63 __pos a) __stk
-- lookahead notify, entity Add
; STestSuite61 ((__p,  "notify") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "notify") : __input, __end) (action63 __pos a) __stk
-- lookahead <eof>, entity Add
; STestSuite61 ([], __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ([], __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; STestSuite62 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead expect, entity Add
; STestSuite62 ((__p,  "expect") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "expect") : __input, __end) (action63 __pos a) __stk
-- lookahead guard, entity Add
; STestSuite62 ((__p,  "guard") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "guard") : __input, __end) (action63 __pos a) __stk
-- lookahead notify, entity Add
; STestSuite62 ((__p,  "notify") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "notify") : __input, __end) (action63 __pos a) __stk
-- lookahead <eof>, entity Add
; STestSuite62 ([], __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ([], __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; STestSuite63 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead =, entity Add
; STestSuite63 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "=") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead expect, entity Add
; STestSuite63 ((__p,  "expect") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "expect") : __input, __end) (action62 __pos a
                                                                              b) __stk
-- lookahead guard, entity Add
; STestSuite63 ((__p,  "guard") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "guard") : __input, __end) (action62 __pos a
                                                                             b) __stk
-- lookahead notify, entity Add
; STestSuite63 ((__p,  "notify") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "notify") : __input, __end) (action62 __pos a
                                                                              b) __stk
-- lookahead <eof>, entity Add
; STestSuite63 ([], __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ([], __end) (action62 __pos a b) __stk
-- lookahead +, entity Add
; STestSuite64 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead expect, entity Add
; STestSuite64 ((__p,  "expect") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "expect") : __input, __end) (action62 __pos a
                                                                              b) __stk
-- lookahead guard, entity Add
; STestSuite64 ((__p,  "guard") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "guard") : __input, __end) (action62 __pos a
                                                                             b) __stk
-- lookahead notify, entity Add
; STestSuite64 ((__p,  "notify") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "notify") : __input, __end) (action62 __pos a
                                                                              b) __stk
-- lookahead <eof>, entity Add
; STestSuite64 ([], __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ([], __end) (action62 __pos a b) __stk
-- lookahead *, entity Mult
; STestSuite65 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; STestSuite65 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead =, entity Mult
; STestSuite65 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "=") : __input, __end) (action67 __pos a) __stk
-- lookahead expect, entity Mult
; STestSuite65 ((__p,  "expect") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "expect") : __input, __end) (action67 __pos a) __stk
-- lookahead guard, entity Mult
; STestSuite65 ((__p,  "guard") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "guard") : __input, __end) (action67 __pos a) __stk
-- lookahead notify, entity Mult
; STestSuite65 ((__p,  "notify") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "notify") : __input, __end) (action67 __pos a) __stk
-- lookahead <eof>, entity Mult
; STestSuite65 ([], __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ([], __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; STestSuite66 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; STestSuite66 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead expect, entity Mult
; STestSuite66 ((__p,  "expect") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "expect") : __input, __end) (action67 __pos a) __stk
-- lookahead guard, entity Mult
; STestSuite66 ((__p,  "guard") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "guard") : __input, __end) (action67 __pos a) __stk
-- lookahead notify, entity Mult
; STestSuite66 ((__p,  "notify") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "notify") : __input, __end) (action67 __pos a) __stk
-- lookahead <eof>, entity Mult
; STestSuite66 ([], __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ([], __end) (action67 __pos a) __stk
-- lookahead *, entity Term
; STestSuite69 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; STestSuite69 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead =, entity Term
; STestSuite69 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action71 __pos n) __stk
-- lookahead expect, entity Term
; STestSuite69 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "expect") : __input, __end) (action71 __pos n) __stk
-- lookahead guard, entity Term
; STestSuite69 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "guard") : __input, __end) (action71 __pos n) __stk
-- lookahead notify, entity Term
; STestSuite69 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "notify") : __input, __end) (action71 __pos n) __stk
-- lookahead <eof>, entity Term
; STestSuite69 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ([], __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; STestSuite70 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; STestSuite70 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead expect, entity Term
; STestSuite70 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "expect") : __input, __end) (action71 __pos n) __stk
-- lookahead guard, entity Term
; STestSuite70 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "guard") : __input, __end) (action71 __pos n) __stk
-- lookahead notify, entity Term
; STestSuite70 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "notify") : __input, __end) (action71 __pos n) __stk
-- lookahead <eof>, entity Term
; STestSuite70 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ([], __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; STestSuite71 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; STestSuite71 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead =, entity Term
; STestSuite71 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action72 __pos n) __stk
-- lookahead expect, entity Term
; STestSuite71 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "expect") : __input, __end) (action72 __pos n) __stk
-- lookahead guard, entity Term
; STestSuite71 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "guard") : __input, __end) (action72 __pos n) __stk
-- lookahead notify, entity Term
; STestSuite71 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "notify") : __input, __end) (action72 __pos n) __stk
-- lookahead <eof>, entity Term
; STestSuite71 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ([], __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; STestSuite72 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; STestSuite72 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead expect, entity Term
; STestSuite72 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "expect") : __input, __end) (action72 __pos n) __stk
-- lookahead guard, entity Term
; STestSuite72 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "guard") : __input, __end) (action72 __pos n) __stk
-- lookahead notify, entity Term
; STestSuite72 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "notify") : __input, __end) (action72 __pos n) __stk
-- lookahead <eof>, entity Term
; STestSuite72 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ([], __end) (action72 __pos n) __stk
-- lookahead *, entity Const
; STestSuite73 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; STestSuite73 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead =, entity Const
; STestSuite73 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead expect, entity Const
; STestSuite73 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "expect") : __input, __end) (action75 __pos n) __stk
-- lookahead guard, entity Const
; STestSuite73 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "guard") : __input, __end) (action75 __pos n) __stk
-- lookahead notify, entity Const
; STestSuite73 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "notify") : __input, __end) (action75 __pos n) __stk
-- lookahead <eof>, entity Const
; STestSuite73 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ([], __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; STestSuite74 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; STestSuite74 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead expect, entity Const
; STestSuite74 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "expect") : __input, __end) (action75 __pos n) __stk
-- lookahead guard, entity Const
; STestSuite74 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "guard") : __input, __end) (action75 __pos n) __stk
-- lookahead notify, entity Const
; STestSuite74 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "notify") : __input, __end) (action75 __pos n) __stk
-- lookahead <eof>, entity Const
; STestSuite74 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ([], __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; STestSuite75 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; STestSuite75 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead =, entity Const
; STestSuite75 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "=") : __input, __end) (action76 __pos n) __stk
-- lookahead expect, entity Const
; STestSuite75 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "expect") : __input, __end) (action76 __pos n) __stk
-- lookahead guard, entity Const
; STestSuite75 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "guard") : __input, __end) (action76 __pos n) __stk
-- lookahead notify, entity Const
; STestSuite75 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "notify") : __input, __end) (action76 __pos n) __stk
-- lookahead <eof>, entity Const
; STestSuite75 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ([], __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; STestSuite76 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; STestSuite76 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead expect, entity Const
; STestSuite76 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "expect") : __input, __end) (action76 __pos n) __stk
-- lookahead guard, entity Const
; STestSuite76 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "guard") : __input, __end) (action76 __pos n) __stk
-- lookahead notify, entity Const
; STestSuite76 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "notify") : __input, __end) (action76 __pos n) __stk
-- lookahead <eof>, entity Const
; STestSuite76 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ([], __end) (action76 __pos n) __stk
-- lookahead *, entity Mult
; STestSuite81 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead +, entity Mult
; STestSuite81 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead =, entity Mult
; STestSuite81 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "=") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead expect, entity Mult
; STestSuite81 ((__p,  "expect") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "expect") : __input, __end) (action66 __pos a
                                                                               b) __stk
-- lookahead guard, entity Mult
; STestSuite81 ((__p,  "guard") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "guard") : __input, __end) (action66 __pos a
                                                                              b) __stk
-- lookahead notify, entity Mult
; STestSuite81 ((__p,  "notify") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "notify") : __input, __end) (action66 __pos a
                                                                               b) __stk
-- lookahead <eof>, entity Mult
; STestSuite81 ([], __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ([], __end) (action66 __pos a b) __stk
-- lookahead *, entity Mult
; STestSuite82 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead +, entity Mult
; STestSuite82 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead expect, entity Mult
; STestSuite82 ((__p,  "expect") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "expect") : __input, __end) (action66 __pos a
                                                                               b) __stk
-- lookahead guard, entity Mult
; STestSuite82 ((__p,  "guard") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "guard") : __input, __end) (action66 __pos a
                                                                              b) __stk
-- lookahead notify, entity Mult
; STestSuite82 ((__p,  "notify") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "notify") : __input, __end) (action66 __pos a
                                                                               b) __stk
-- lookahead <eof>, entity Mult
; STestSuite82 ([], __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ([], __end) (action66 __pos a b) __stk
-- lookahead *, entity Term
; STestSuite83 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; STestSuite83 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead =, entity Term
; STestSuite83 ((__p,  "=") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action70 __pos e) __stk
-- lookahead expect, entity Term
; STestSuite83 ((__p,  "expect") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "expect") : __input, __end) (action70 __pos e) __stk
-- lookahead guard, entity Term
; STestSuite83 ((__p,  "guard") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "guard") : __input, __end) (action70 __pos e) __stk
-- lookahead notify, entity Term
; STestSuite83 ((__p,  "notify") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "notify") : __input, __end) (action70 __pos e) __stk
-- lookahead <eof>, entity Term
; STestSuite83 ([], __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ([], __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; STestSuite84 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; STestSuite84 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead expect, entity Term
; STestSuite84 ((__p,  "expect") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "expect") : __input, __end) (action70 __pos e) __stk
-- lookahead guard, entity Term
; STestSuite84 ((__p,  "guard") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "guard") : __input, __end) (action70 __pos e) __stk
-- lookahead notify, entity Term
; STestSuite84 ((__p,  "notify") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "notify") : __input, __end) (action70 __pos e) __stk
-- lookahead <eof>, entity Term
; STestSuite84 ([], __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ([], __end) (action70 __pos e) __stk
-- lookahead expect, entity Expr
; STestSuite85 ((__p,  "expect") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForTestSuite ((__p,  "expect") : __input, __end) (action59 __pos a) __stk
-- lookahead guard, entity Expr
; STestSuite85 ((__p,  "guard") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForTestSuite ((__p,  "guard") : __input, __end) (action59 __pos a) __stk
-- lookahead notify, entity Expr
; STestSuite85 ((__p,  "notify") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForTestSuite ((__p,  "notify") : __input, __end) (action59 __pos a) __stk
-- lookahead <eof>, entity Expr
; STestSuite85 ([], __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForTestSuite ([], __end) (action59 __pos a) __stk
-- lookahead expect, entity Expr
; STestSuite86 ((__p,  "expect") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForTestSuite ((__p,  "expect") : __input, __end) (action58 __pos a
                                                                               b) __stk
-- lookahead guard, entity Expr
; STestSuite86 ((__p,  "guard") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForTestSuite ((__p,  "guard") : __input, __end) (action58 __pos a
                                                                              b) __stk
-- lookahead notify, entity Expr
; STestSuite86 ((__p,  "notify") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForTestSuite ((__p,  "notify") : __input, __end) (action58 __pos a
                                                                               b) __stk
-- lookahead <eof>, entity Expr
; STestSuite86 ([], __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForTestSuite ([], __end) (action58 __pos a b) __stk
-- lookahead expect, entity Call
; STestSuite94 ((__p,  "expect") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForTestSuite ((__p,  "expect") : __input, __end) (action47 __pos pre
                                                                               t) __stk
-- lookahead guard, entity Call
; STestSuite94 ((__p,  "guard") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForTestSuite ((__p,  "guard") : __input, __end) (action47 __pos pre
                                                                              t) __stk
-- lookahead notify, entity Call
; STestSuite94 ((__p,  "notify") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForTestSuite ((__p,  "notify") : __input, __end) (action47 __pos pre
                                                                               t) __stk
-- lookahead <eof>, entity Call
; STestSuite94 ([], __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForTestSuite ([], __end) (action47 __pos pre t) __stk
-- lookahead expect, entity Tuple
; STestSuite95 ((__p,  "expect") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForTestSuite ((__p,  "expect") : __input, __end) (action50 __pos ) __stk
-- lookahead guard, entity Tuple
; STestSuite95 ((__p,  "guard") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForTestSuite ((__p,  "guard") : __input, __end) (action50 __pos ) __stk
-- lookahead notify, entity Tuple
; STestSuite95 ((__p,  "notify") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForTestSuite ((__p,  "notify") : __input, __end) (action50 __pos ) __stk
-- lookahead <eof>, entity Tuple
; STestSuite95 ([], __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForTestSuite ([], __end) (action50 __pos ) __stk
-- lookahead expect, entity Test
; STestSuite98 ((__p,  "expect") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "expect") : __input, __end) (action79 __pos c) __stk
-- lookahead guard, entity Test
; STestSuite98 ((__p,  "guard") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "guard") : __input, __end) (action79 __pos c) __stk
-- lookahead notify, entity Test
; STestSuite98 ((__p,  "notify") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "notify") : __input, __end) (action79 __pos c) __stk
-- lookahead <eof>, entity Test
; STestSuite98 ([], __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ([], __end) (action79 __pos c) __stk
-- lookahead expect, entity Test
; STestSuite99 ((__p,  "expect") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "expect") : __input, __end) (action80 __pos c) __stk
-- lookahead guard, entity Test
; STestSuite99 ((__p,  "guard") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "guard") : __input, __end) (action80 __pos c) __stk
-- lookahead notify, entity Test
; STestSuite99 ((__p,  "notify") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "notify") : __input, __end) (action80 __pos c) __stk
-- lookahead <eof>, entity Test
; STestSuite99 ([], __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ([], __end) (action80 __pos c) __stk
-- lookahead expect, entity Test
; STestSuite100 ((__p,  "expect") : __input, __end) ((e :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "expect") : __input, __end) (action81 __pos e) __stk
-- lookahead guard, entity Test
; STestSuite100 ((__p,  "guard") : __input, __end) ((e :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "guard") : __input, __end) (action81 __pos e) __stk
-- lookahead notify, entity Test
; STestSuite100 ((__p,  "notify") : __input, __end) ((e :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "notify") : __input, __end) (action81 __pos e) __stk
-- lookahead <eof>, entity Test
; STestSuite100 ([], __end) ((e :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ([], __end) (action81 __pos e) __stk
-- lookahead expect, entity Tuple
; STestSuite101 ((__p,  "expect") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForTestSuite ((__p,  "expect") : __input, __end) (action51 __pos es) __stk
-- lookahead guard, entity Tuple
; STestSuite101 ((__p,  "guard") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForTestSuite ((__p,  "guard") : __input, __end) (action51 __pos es) __stk
-- lookahead notify, entity Tuple
; STestSuite101 ((__p,  "notify") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForTestSuite ((__p,  "notify") : __input, __end) (action51 __pos es) __stk
-- lookahead <eof>, entity Tuple
; STestSuite101 ([], __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForTestSuite ([], __end) (action51 __pos es) __stk
-- lookahead <eof>, entity TestSuite
; STestSuite103 ([], __end) ((res :> __stk@(_, __pos, _))) ->
    pure res
-- lookahead <eof>, entity Tests
; STestSuite105 ([], __end) ((t :> __stk@(_, __pos, _))) ->
    __gotoTestsForTestSuite ([], __end) (action87 __pos t) __stk
-- lookahead <eof>, entity TestSuite
; STestSuite106 ([], __end) ((tests :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestSuiteForTestSuite ([], __end) (action84 __pos tests) __stk
-- lookahead <eof>, entity Tests
; STestSuite107 ([], __end) ((ts :> (_, _, t :> __stk@(_, __pos, _)))) ->
    __gotoTestsForTestSuite ([], __end) (action88 __pos t ts) __stk
; STestSuite0 __input _ -> Left  (currentPos __input, [")", ","])
; STestSuite1 __input _ ->
    Left  (currentPos __input, [")", "+", ",", "="])
; STestSuite2 __input _ ->
    Left  (currentPos __input, [")", "+", "="])
; STestSuite3 __input _ -> Left  (currentPos __input, [")", "+"])
; STestSuite4 __input _ ->
    Left  (currentPos __input, [")", "+", ","])
; STestSuite5 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite6 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite7 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite8 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite9 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite10 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite11 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite12 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite13 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite14 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite15 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite16 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite17 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite18 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite19 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite20 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite21 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite22 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite23 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite24 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite25 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite26 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite27 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite28 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite29 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite30 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite31 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite32 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite33 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite34 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite35 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite36 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite37 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite38 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite39 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite40 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite41 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite42 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite43 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite44 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite45 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite46 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite47 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite48 __input _ -> Left  (currentPos __input, [")"])
; STestSuite49 __input _ -> Left  (currentPos __input, [")"])
; STestSuite50 __input _ -> Left  (currentPos __input, [")"])
; STestSuite51 __input _ -> Left  (currentPos __input, [")"])
; STestSuite52 __input _ -> Left  (currentPos __input, [")"])
; STestSuite53 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite54 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite55 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite56 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite57 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite58 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite59 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite60 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite61 __input _ ->
    Left  (currentPos __input, ["*", "+", "=", "expect", "guard",
                                "notify", "<eof>"])
; STestSuite62 __input _ ->
    Left  (currentPos __input, ["*", "+", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite63 __input _ ->
    Left  (currentPos __input, ["*", "+", "=", "expect", "guard",
                                "notify", "<eof>"])
; STestSuite64 __input _ ->
    Left  (currentPos __input, ["*", "+", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite65 __input _ ->
    Left  (currentPos __input, ["*", "+", "=", "expect", "guard",
                                "notify", "<eof>"])
; STestSuite66 __input _ ->
    Left  (currentPos __input, ["*", "+", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite67 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite68 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite69 __input _ ->
    Left  (currentPos __input, ["*", "+", "=", "expect", "guard",
                                "notify", "<eof>"])
; STestSuite70 __input _ ->
    Left  (currentPos __input, ["*", "+", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite71 __input _ ->
    Left  (currentPos __input, ["*", "+", "=", "expect", "guard",
                                "notify", "<eof>"])
; STestSuite72 __input _ ->
    Left  (currentPos __input, ["*", "+", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite73 __input _ ->
    Left  (currentPos __input, ["*", "+", "=", "expect", "guard",
                                "notify", "<eof>"])
; STestSuite74 __input _ ->
    Left  (currentPos __input, ["*", "+", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite75 __input _ ->
    Left  (currentPos __input, ["*", "+", "=", "expect", "guard",
                                "notify", "<eof>"])
; STestSuite76 __input _ ->
    Left  (currentPos __input, ["*", "+", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite77 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite78 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite79 __input _ -> Left  (currentPos __input, [")"])
; STestSuite80 __input _ -> Left  (currentPos __input, [")"])
; STestSuite81 __input _ ->
    Left  (currentPos __input, ["*", "+", "=", "expect", "guard",
                                "notify", "<eof>"])
; STestSuite82 __input _ ->
    Left  (currentPos __input, ["*", "+", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite83 __input _ ->
    Left  (currentPos __input, ["*", "+", "=", "expect", "guard",
                                "notify", "<eof>"])
; STestSuite84 __input _ ->
    Left  (currentPos __input, ["*", "+", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite85 __input _ ->
    Left  (currentPos __input, ["+", "=", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite86 __input _ ->
    Left  (currentPos __input, ["+", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite87 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite88 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite89 __input _ -> Left  (currentPos __input, ["("])
; STestSuite90 __input _ ->
    Left  (currentPos __input, ["(", ")", "<Name>", "<name>", "<num>"])
; STestSuite91 __input _ -> Left  (currentPos __input, ["<name>"])
; STestSuite92 __input _ -> Left  (currentPos __input, ["<name>"])
; STestSuite93 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite94 __input _ ->
    Left  (currentPos __input, ["expect", "guard", "notify", "<eof>"])
; STestSuite95 __input _ ->
    Left  (currentPos __input, ["expect", "guard", "notify", "<eof>"])
; STestSuite96 __input _ -> Left  (currentPos __input, [")"])
; STestSuite97 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite98 __input _ ->
    Left  (currentPos __input, ["expect", "guard", "notify", "<eof>"])
; STestSuite99 __input _ ->
    Left  (currentPos __input, ["expect", "guard", "notify", "<eof>"])
; STestSuite100 __input _ ->
    Left  (currentPos __input, ["expect", "guard", "notify", "<eof>"])
; STestSuite101 __input _ ->
    Left  (currentPos __input, ["expect", "guard", "notify", "<eof>"])
; STestSuite102 __input _ -> Left  (currentPos __input, ["test"])
; STestSuite103 __input _ -> Left  (currentPos __input, ["<eof>"])
; STestSuite104 __input _ ->
    Left  (currentPos __input, ["expect", "guard", "notify"])
; STestSuite105 __input _ ->
    Left  (currentPos __input, ["expect", "guard", "notify", "<eof>"])
; STestSuite106 __input _ -> Left  (currentPos __input, ["<eof>"])
; STestSuite107 __input _ -> Left  (currentPos __input, ["<eof>"])
} where {
; action10 pos stmts =
          Program {stmts}
; action13 pos c cs =
    c : cs
; action14 pos c =
    [c]
; action17 pos c =
    StmtClause pos c
; action18 pos e =
    StmtEffect pos e
; action21 pos c ds =
    Effect pos c [] ds
; action22 pos c cs ds =
    Effect pos c cs ds
; action23 pos c cs =
    Effect pos c cs []
; action26 pos c =
    Clause pos c []
; action27 pos c cs =
    Clause pos c cs
; action30 pos c cs =
    c : cs
; action31 pos c =
    [c]
; action34 pos c =
    Assert pos c
; action35 pos c =
    Refute pos c
; action38 pos c cs =
    c : cs
; action39 pos c =
    [c]
; action42 pos c =
        CondAssert pos c
; action43 pos c =
    CondRefute pos c
; action44 pos e =
        CondGuard  pos e
; action47 pos pre t =
    Call pos pre t
; action50 pos =
    []
; action51 pos es =
    es
; action54 pos e es =
    e : es
; action55 pos e =
    [e]
; action58 pos a b =
    ExprBinary pos a Equals b
; action59 pos a =
    a
; action62 pos a b =
    ExprBinary pos a Add b
; action63 pos a =
    a
; action66 pos a b =
    ExprBinary pos a Mult b
; action67 pos a =
    a
; action70 pos e =
    e
; action71 pos n =
    ExprVar   pos n
; action72 pos n =
    ExprConst pos n
; action75 pos n =
    ConstNamed pos n
; action76 pos n =
    ConstInt   pos n
; action79 pos c =
    Expect pos c
; action80 pos c =
    Notify pos c
; action81 pos e =
    Guard  pos e
; action84 pos tests =
    TestSuite {tests}
; action87 pos t =
    [t]
; action88 pos t ts =
     t : ts
}

parseTestSuite :: FilePath -> IO (Either LexerError (Either (Pos, [String]) TestSuite))
parseTestSuite filepath = do
  text <- Text.readFile filepath
  case lexText filepath text ["(", ")", "*", "+", ",", "-", "->",
                              ".", "<-", "=", "=>", "expect", "guard", "notify", "test", "~"] of
    Left  err   -> pure (Left err)
    Right input -> pure (Right (__runTestSuite STestSuite102 input Nil))

currentPos :: ([Lexeme], Pos) -> Pos
currentPos = \case
  ([],           end) -> end
  ((pos, _) : _, _)   -> pos
