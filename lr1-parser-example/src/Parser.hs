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
  SProgram0 :: StProgram (() : Expr : a)
  SProgram1 :: StProgram (() : a)
  SProgram2 :: StProgram (() : a)
  SProgram3 :: StProgram (() : a)
  SProgram4 :: StProgram (() : a)
  SProgram5 :: StProgram (() : a)
  SProgram6 :: StProgram (() : a)
  SProgram7 :: StProgram (() : a)
  SProgram8 :: StProgram (() : a)
  SProgram9 :: StProgram (() : a)
  SProgram10 :: StProgram (() : a)
  SProgram11 :: StProgram (() : a)
  SProgram12 :: StProgram (() : Expr : a)
  SProgram13 :: StProgram (() : Expr : a)
  SProgram14 :: StProgram (() : Expr : a)
  SProgram15 :: StProgram (() : Expr : a)
  SProgram16 :: StProgram (() : Expr : a)
  SProgram17 :: StProgram (() : Expr : a)
  SProgram18 :: StProgram (() : Expr : a)
  SProgram19 :: StProgram (() : Expr : a)
  SProgram20 :: StProgram (() : Expr : a)
  SProgram21 :: StProgram (() : Expr : a)
  SProgram22 :: StProgram (Expr : a)
  SProgram23 :: StProgram (Expr : a)
  SProgram24 :: StProgram (Expr : a)
  SProgram25 :: StProgram (Expr : () : Expr : a)
  SProgram26 :: StProgram (Expr : () : Expr : a)
  SProgram27 :: StProgram (Expr : a)
  SProgram28 :: StProgram (Expr : a)
  SProgram29 :: StProgram (Expr : a)
  SProgram30 :: StProgram (Expr : a)
  SProgram31 :: StProgram (Expr : () : Expr : a)
  SProgram32 :: StProgram (Expr : () : Expr : a)
  SProgram33 :: StProgram (Expr : () : Expr : a)
  SProgram34 :: StProgram (Expr : () : Expr : a)
  SProgram35 :: StProgram (Expr : a)
  SProgram36 :: StProgram (Expr : a)
  SProgram37 :: StProgram (Expr : a)
  SProgram38 :: StProgram (Expr : a)
  SProgram39 :: StProgram (Text : a)
  SProgram40 :: StProgram (Text : a)
  SProgram41 :: StProgram (Text : a)
  SProgram42 :: StProgram (Text : a)
  SProgram43 :: StProgram (Const : a)
  SProgram44 :: StProgram (Const : a)
  SProgram45 :: StProgram (Const : a)
  SProgram46 :: StProgram (Const : a)
  SProgram47 :: StProgram (Text : a)
  SProgram48 :: StProgram (Text : a)
  SProgram49 :: StProgram (Text : a)
  SProgram50 :: StProgram (Text : a)
  SProgram51 :: StProgram (Integer : a)
  SProgram52 :: StProgram (Integer : a)
  SProgram53 :: StProgram (Integer : a)
  SProgram54 :: StProgram (Integer : a)
  SProgram55 :: StProgram (Expr : () : a)
  SProgram56 :: StProgram (Expr : () : a)
  SProgram57 :: StProgram (Expr : () : a)
  SProgram58 :: StProgram (Expr : () : a)
  SProgram59 :: StProgram ([Expr] : () : Expr : a)
  SProgram60 :: StProgram (Expr : () : Expr : a)
  SProgram61 :: StProgram (Expr : () : Expr : a)
  SProgram62 :: StProgram (Expr : () : Expr : a)
  SProgram63 :: StProgram (Expr : () : Expr : a)
  SProgram64 :: StProgram (() : Expr : () : a)
  SProgram65 :: StProgram (() : Expr : () : a)
  SProgram66 :: StProgram (() : Expr : () : a)
  SProgram67 :: StProgram (() : Expr : () : a)
  SProgram68 :: StProgram (() : Cond : a)
  SProgram69 :: StProgram (() : Cond : a)
  SProgram70 :: StProgram (() : Call : a)
  SProgram71 :: StProgram (() : Call : a)
  SProgram72 :: StProgram (() : Expr : a)
  SProgram73 :: StProgram (() : Expr : a)
  SProgram74 :: StProgram (() : Expr : a)
  SProgram75 :: StProgram (() : Expr : a)
  SProgram76 :: StProgram (() : Expr : a)
  SProgram77 :: StProgram (() : Expr : a)
  SProgram78 :: StProgram (() : Expr : a)
  SProgram79 :: StProgram (() : Expr : a)
  SProgram80 :: StProgram (() : Expr : a)
  SProgram81 :: StProgram (() : Expr : a)
  SProgram82 :: StProgram (Expr : a)
  SProgram83 :: StProgram (Expr : a)
  SProgram84 :: StProgram (Expr : a)
  SProgram85 :: StProgram (Expr : a)
  SProgram86 :: StProgram (Expr : () : Expr : a)
  SProgram87 :: StProgram (Expr : () : Expr : a)
  SProgram88 :: StProgram (Expr : () : Expr : a)
  SProgram89 :: StProgram (Expr : () : Expr : a)
  SProgram90 :: StProgram (Expr : a)
  SProgram91 :: StProgram (Expr : a)
  SProgram92 :: StProgram (Expr : a)
  SProgram93 :: StProgram (Expr : a)
  SProgram94 :: StProgram (Text : a)
  SProgram95 :: StProgram (Text : a)
  SProgram96 :: StProgram (Text : a)
  SProgram97 :: StProgram (Text : a)
  SProgram98 :: StProgram (Const : a)
  SProgram99 :: StProgram (Const : a)
  SProgram100 :: StProgram (Const : a)
  SProgram101 :: StProgram (Const : a)
  SProgram102 :: StProgram (Text : a)
  SProgram103 :: StProgram (Text : a)
  SProgram104 :: StProgram (Text : a)
  SProgram105 :: StProgram (Text : a)
  SProgram106 :: StProgram (Text : a)
  SProgram107 :: StProgram (Text : a)
  SProgram108 :: StProgram (Integer : a)
  SProgram109 :: StProgram (Integer : a)
  SProgram110 :: StProgram (Integer : a)
  SProgram111 :: StProgram (Integer : a)
  SProgram112 :: StProgram (Expr : () : a)
  SProgram113 :: StProgram (Expr : () : a)
  SProgram114 :: StProgram (Expr : () : a)
  SProgram115 :: StProgram (Expr : () : a)
  SProgram116 :: StProgram (Expr : () : Expr : a)
  SProgram117 :: StProgram (Expr : () : Expr : a)
  SProgram118 :: StProgram (Expr : () : Expr : a)
  SProgram119 :: StProgram (Expr : () : Expr : a)
  SProgram120 :: StProgram (() : Expr : () : a)
  SProgram121 :: StProgram (() : Expr : () : a)
  SProgram122 :: StProgram (() : Expr : () : a)
  SProgram123 :: StProgram (() : Expr : () : a)
  SProgram124 :: StProgram (Expr : a)
  SProgram125 :: StProgram (Expr : a)
  SProgram126 :: StProgram (Expr : () : Expr : a)
  SProgram127 :: StProgram (Expr : () : Expr : a)
  SProgram128 :: StProgram (() : Change : a)
  SProgram129 :: StProgram (() : Call : a)
  SProgram130 :: StProgram (() : [Cond] : () : Call : a)
  SProgram131 :: StProgram (() : a)
  SProgram132 :: StProgram (() : a)
  SProgram133 :: StProgram (() : a)
  SProgram134 :: StProgram (() : a)
  SProgram135 :: StProgram (Text : a)
  SProgram136 :: StProgram (Text : a)
  SProgram137 :: StProgram (Call : a)
  SProgram138 :: StProgram (Call : a)
  SProgram139 :: StProgram (Expr : a)
  SProgram140 :: StProgram (Expr : a)
  SProgram141 :: StProgram (Call : () : a)
  SProgram142 :: StProgram (Call : () : a)
  SProgram143 :: StProgram (Call : () : a)
  SProgram144 :: StProgram (Call : () : a)
  SProgram145 :: StProgram ([Expr] : Text : a)
  SProgram146 :: StProgram ([Expr] : Text : a)
  SProgram147 :: StProgram (() : () : a)
  SProgram148 :: StProgram (() : () : a)
  SProgram149 :: StProgram ([Expr] : () : a)
  SProgram150 :: StProgram ([Expr] : () : a)
  SProgram151 :: StProgram (() : [Expr] : () : a)
  SProgram152 :: StProgram (() : [Expr] : () : a)
  SProgram153 :: StProgram (a)
  SProgram154 :: StProgram (Stmt : a)
  SProgram155 :: StProgram (Text : a)
  SProgram156 :: StProgram ([Expr] : Text : a)
  SProgram157 :: StProgram (() : () : a)
  SProgram158 :: StProgram ([Expr] : () : a)
  SProgram159 :: StProgram (() : [Expr] : () : a)
  SProgram160 :: StProgram (Change : a)
  SProgram161 :: StProgram (Cond : a)
  SProgram162 :: StProgram (Cond : a)
  SProgram163 :: StProgram ([Change] : () : Change : a)
  SProgram164 :: StProgram ([Cond] : () : Cond : a)
  SProgram165 :: StProgram ([Cond] : () : Cond : a)
  SProgram166 :: StProgram (Clause : a)
  SProgram167 :: StProgram (Effect : a)
  SProgram168 :: StProgram (Call : a)
  SProgram169 :: StProgram (() : Call : a)
  SProgram170 :: StProgram ([Change] : () : Call : a)
  SProgram171 :: StProgram ([Cond] : () : Call : a)
  SProgram172 :: StProgram ([Cond] : () : Call : a)
  SProgram173 :: StProgram (() : [Change] : () : Call : a)
  SProgram174 :: StProgram (() : [Cond] : () : Call : a)
  SProgram175 :: StProgram (() : [Cond] : () : Call : a)
  SProgram176 :: StProgram ([Change] : () : [Cond] : () : Call : a)
  SProgram177 :: StProgram (() : [Change] : () : [Cond] : () : Call :
                            a)
  SProgram178 :: StProgram (Program : a)
  SProgram179 :: StProgram ([Stmt] : a)
  SProgram180 :: StProgram ([Stmt] : Stmt : a)

__gotoAddForProgram :: ([Lexeme], Pos) -> Expr -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoAddForProgram toks term stk@(state, _, _) = case state of
  SProgram0 -> __runProgram SProgram23 toks (term :> stk)
  SProgram1 -> __runProgram SProgram23 toks (term :> stk)
  SProgram2 -> __runProgram SProgram23 toks (term :> stk)
  SProgram3 -> __runProgram SProgram23 toks (term :> stk)
  SProgram4 -> __runProgram SProgram24 toks (term :> stk)
  SProgram5 -> __runProgram SProgram24 toks (term :> stk)
  SProgram6 -> __runProgram SProgram24 toks (term :> stk)
  SProgram7 -> __runProgram SProgram24 toks (term :> stk)
  SProgram8 -> __runProgram SProgram24 toks (term :> stk)
  SProgram9 -> __runProgram SProgram24 toks (term :> stk)
  SProgram10 -> __runProgram SProgram24 toks (term :> stk)
  SProgram11 -> __runProgram SProgram24 toks (term :> stk)
  SProgram12 -> __runProgram SProgram25 toks (term :> stk)
  SProgram13 -> __runProgram SProgram26 toks (term :> stk)
  SProgram68 -> __runProgram SProgram124 toks (term :> stk)
  SProgram69 -> __runProgram SProgram125 toks (term :> stk)
  SProgram70 -> __runProgram SProgram125 toks (term :> stk)
  SProgram71 -> __runProgram SProgram124 toks (term :> stk)
  SProgram72 -> __runProgram SProgram126 toks (term :> stk)
  SProgram73 -> __runProgram SProgram127 toks (term :> stk)
  _ -> error ""

__gotoCallForProgram :: ([Lexeme], Pos) -> Call -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoCallForProgram toks term stk@(state, _, _) = case state of
  SProgram68 -> __runProgram SProgram137 toks (term :> stk)
  SProgram69 -> __runProgram SProgram138 toks (term :> stk)
  SProgram70 -> __runProgram SProgram138 toks (term :> stk)
  SProgram71 -> __runProgram SProgram137 toks (term :> stk)
  SProgram131 -> __runProgram SProgram141 toks (term :> stk)
  SProgram132 -> __runProgram SProgram142 toks (term :> stk)
  SProgram133 -> __runProgram SProgram143 toks (term :> stk)
  SProgram134 -> __runProgram SProgram144 toks (term :> stk)
  SProgram153 -> __runProgram SProgram168 toks (term :> stk)
  SProgram154 -> __runProgram SProgram168 toks (term :> stk)
  _ -> error ""

__gotoChangeForProgram :: ([Lexeme], Pos) -> Change -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoChangeForProgram toks term stk@(state, _, _) = case state of
  SProgram128 -> __runProgram SProgram160 toks (term :> stk)
  SProgram129 -> __runProgram SProgram160 toks (term :> stk)
  SProgram130 -> __runProgram SProgram160 toks (term :> stk)
  _ -> error ""

__gotoChangesForProgram :: ([Lexeme], Pos) -> [Change] -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoChangesForProgram toks term stk@(state, _, _) = case state of
  SProgram128 -> __runProgram SProgram163 toks (term :> stk)
  SProgram129 -> __runProgram SProgram170 toks (term :> stk)
  SProgram130 -> __runProgram SProgram176 toks (term :> stk)
  _ -> error ""

__gotoClauseForProgram :: ([Lexeme], Pos) -> Clause -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoClauseForProgram toks term stk@(state, _, _) = case state of
  SProgram153 -> __runProgram SProgram166 toks (term :> stk)
  SProgram154 -> __runProgram SProgram166 toks (term :> stk)
  _ -> error ""

__gotoCondForProgram :: ([Lexeme], Pos) -> Cond -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoCondForProgram toks term stk@(state, _, _) = case state of
  SProgram68 -> __runProgram SProgram161 toks (term :> stk)
  SProgram69 -> __runProgram SProgram162 toks (term :> stk)
  SProgram70 -> __runProgram SProgram162 toks (term :> stk)
  SProgram71 -> __runProgram SProgram161 toks (term :> stk)
  _ -> error ""

__gotoCondsForProgram :: ([Lexeme], Pos) -> [Cond] -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoCondsForProgram toks term stk@(state, _, _) = case state of
  SProgram68 -> __runProgram SProgram164 toks (term :> stk)
  SProgram69 -> __runProgram SProgram165 toks (term :> stk)
  SProgram70 -> __runProgram SProgram171 toks (term :> stk)
  SProgram71 -> __runProgram SProgram172 toks (term :> stk)
  _ -> error ""

__gotoConstForProgram :: ([Lexeme], Pos) -> Const -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoConstForProgram toks term stk@(state, _, _) = case state of
  SProgram0 -> __runProgram SProgram45 toks (term :> stk)
  SProgram1 -> __runProgram SProgram45 toks (term :> stk)
  SProgram2 -> __runProgram SProgram45 toks (term :> stk)
  SProgram3 -> __runProgram SProgram45 toks (term :> stk)
  SProgram4 -> __runProgram SProgram46 toks (term :> stk)
  SProgram5 -> __runProgram SProgram46 toks (term :> stk)
  SProgram6 -> __runProgram SProgram46 toks (term :> stk)
  SProgram7 -> __runProgram SProgram46 toks (term :> stk)
  SProgram8 -> __runProgram SProgram46 toks (term :> stk)
  SProgram9 -> __runProgram SProgram46 toks (term :> stk)
  SProgram10 -> __runProgram SProgram46 toks (term :> stk)
  SProgram11 -> __runProgram SProgram46 toks (term :> stk)
  SProgram12 -> __runProgram SProgram43 toks (term :> stk)
  SProgram13 -> __runProgram SProgram44 toks (term :> stk)
  SProgram14 -> __runProgram SProgram43 toks (term :> stk)
  SProgram15 -> __runProgram SProgram44 toks (term :> stk)
  SProgram16 -> __runProgram SProgram45 toks (term :> stk)
  SProgram17 -> __runProgram SProgram46 toks (term :> stk)
  SProgram18 -> __runProgram SProgram43 toks (term :> stk)
  SProgram19 -> __runProgram SProgram44 toks (term :> stk)
  SProgram20 -> __runProgram SProgram45 toks (term :> stk)
  SProgram21 -> __runProgram SProgram46 toks (term :> stk)
  SProgram68 -> __runProgram SProgram99 toks (term :> stk)
  SProgram69 -> __runProgram SProgram100 toks (term :> stk)
  SProgram70 -> __runProgram SProgram100 toks (term :> stk)
  SProgram71 -> __runProgram SProgram99 toks (term :> stk)
  SProgram72 -> __runProgram SProgram98 toks (term :> stk)
  SProgram73 -> __runProgram SProgram101 toks (term :> stk)
  SProgram74 -> __runProgram SProgram98 toks (term :> stk)
  SProgram75 -> __runProgram SProgram99 toks (term :> stk)
  SProgram76 -> __runProgram SProgram100 toks (term :> stk)
  SProgram77 -> __runProgram SProgram101 toks (term :> stk)
  SProgram78 -> __runProgram SProgram98 toks (term :> stk)
  SProgram79 -> __runProgram SProgram99 toks (term :> stk)
  SProgram80 -> __runProgram SProgram100 toks (term :> stk)
  SProgram81 -> __runProgram SProgram101 toks (term :> stk)
  _ -> error ""

__gotoEffectForProgram :: ([Lexeme], Pos) -> Effect -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoEffectForProgram toks term stk@(state, _, _) = case state of
  SProgram153 -> __runProgram SProgram167 toks (term :> stk)
  SProgram154 -> __runProgram SProgram167 toks (term :> stk)
  _ -> error ""

__gotoExprForProgram :: ([Lexeme], Pos) -> Expr -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoExprForProgram toks term stk@(state, _, _) = case state of
  SProgram0 -> __runProgram SProgram22 toks (term :> stk)
  SProgram1 -> __runProgram SProgram22 toks (term :> stk)
  SProgram2 -> __runProgram SProgram22 toks (term :> stk)
  SProgram3 -> __runProgram SProgram22 toks (term :> stk)
  SProgram4 -> __runProgram SProgram56 toks (term :> stk)
  SProgram5 -> __runProgram SProgram57 toks (term :> stk)
  SProgram6 -> __runProgram SProgram55 toks (term :> stk)
  SProgram7 -> __runProgram SProgram58 toks (term :> stk)
  SProgram8 -> __runProgram SProgram112 toks (term :> stk)
  SProgram9 -> __runProgram SProgram113 toks (term :> stk)
  SProgram10 -> __runProgram SProgram114 toks (term :> stk)
  SProgram11 -> __runProgram SProgram115 toks (term :> stk)
  SProgram68 -> __runProgram SProgram139 toks (term :> stk)
  SProgram69 -> __runProgram SProgram140 toks (term :> stk)
  SProgram70 -> __runProgram SProgram140 toks (term :> stk)
  SProgram71 -> __runProgram SProgram139 toks (term :> stk)
  _ -> error ""

__gotoExprs1ForProgram :: ([Lexeme], Pos) -> [Expr] -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoExprs1ForProgram toks term stk@(state, _, _) = case state of
  SProgram0 -> __runProgram SProgram59 toks (term :> stk)
  SProgram1 -> __runProgram SProgram149 toks (term :> stk)
  SProgram2 -> __runProgram SProgram150 toks (term :> stk)
  SProgram3 -> __runProgram SProgram158 toks (term :> stk)
  _ -> error ""

__gotoMultForProgram :: ([Lexeme], Pos) -> Expr -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoMultForProgram toks term stk@(state, _, _) = case state of
  SProgram0 -> __runProgram SProgram29 toks (term :> stk)
  SProgram1 -> __runProgram SProgram29 toks (term :> stk)
  SProgram2 -> __runProgram SProgram29 toks (term :> stk)
  SProgram3 -> __runProgram SProgram29 toks (term :> stk)
  SProgram4 -> __runProgram SProgram30 toks (term :> stk)
  SProgram5 -> __runProgram SProgram30 toks (term :> stk)
  SProgram6 -> __runProgram SProgram30 toks (term :> stk)
  SProgram7 -> __runProgram SProgram30 toks (term :> stk)
  SProgram8 -> __runProgram SProgram30 toks (term :> stk)
  SProgram9 -> __runProgram SProgram30 toks (term :> stk)
  SProgram10 -> __runProgram SProgram30 toks (term :> stk)
  SProgram11 -> __runProgram SProgram30 toks (term :> stk)
  SProgram12 -> __runProgram SProgram27 toks (term :> stk)
  SProgram13 -> __runProgram SProgram28 toks (term :> stk)
  SProgram14 -> __runProgram SProgram31 toks (term :> stk)
  SProgram15 -> __runProgram SProgram32 toks (term :> stk)
  SProgram16 -> __runProgram SProgram33 toks (term :> stk)
  SProgram17 -> __runProgram SProgram34 toks (term :> stk)
  SProgram68 -> __runProgram SProgram83 toks (term :> stk)
  SProgram69 -> __runProgram SProgram84 toks (term :> stk)
  SProgram70 -> __runProgram SProgram84 toks (term :> stk)
  SProgram71 -> __runProgram SProgram83 toks (term :> stk)
  SProgram72 -> __runProgram SProgram82 toks (term :> stk)
  SProgram73 -> __runProgram SProgram85 toks (term :> stk)
  SProgram74 -> __runProgram SProgram86 toks (term :> stk)
  SProgram75 -> __runProgram SProgram87 toks (term :> stk)
  SProgram76 -> __runProgram SProgram88 toks (term :> stk)
  SProgram77 -> __runProgram SProgram89 toks (term :> stk)
  _ -> error ""

__gotoProgramForProgram :: ([Lexeme], Pos) -> Program -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoProgramForProgram toks term stk@(state, _, _) = case state of
  SProgram153 -> __runProgram SProgram178 toks (term :> stk)
  _ -> error ""

__gotoStmtForProgram :: ([Lexeme], Pos) -> Stmt -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoStmtForProgram toks term stk@(state, _, _) = case state of
  SProgram153 -> __runProgram SProgram154 toks (term :> stk)
  SProgram154 -> __runProgram SProgram154 toks (term :> stk)
  _ -> error ""

__gotoStmtsForProgram :: ([Lexeme], Pos) -> [Stmt] -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoStmtsForProgram toks term stk@(state, _, _) = case state of
  SProgram153 -> __runProgram SProgram179 toks (term :> stk)
  SProgram154 -> __runProgram SProgram180 toks (term :> stk)
  _ -> error ""

__gotoTermForProgram :: ([Lexeme], Pos) -> Expr -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoTermForProgram toks term stk@(state, _, _) = case state of
  SProgram0 -> __runProgram SProgram37 toks (term :> stk)
  SProgram1 -> __runProgram SProgram37 toks (term :> stk)
  SProgram2 -> __runProgram SProgram37 toks (term :> stk)
  SProgram3 -> __runProgram SProgram37 toks (term :> stk)
  SProgram4 -> __runProgram SProgram38 toks (term :> stk)
  SProgram5 -> __runProgram SProgram38 toks (term :> stk)
  SProgram6 -> __runProgram SProgram38 toks (term :> stk)
  SProgram7 -> __runProgram SProgram38 toks (term :> stk)
  SProgram8 -> __runProgram SProgram38 toks (term :> stk)
  SProgram9 -> __runProgram SProgram38 toks (term :> stk)
  SProgram10 -> __runProgram SProgram38 toks (term :> stk)
  SProgram11 -> __runProgram SProgram38 toks (term :> stk)
  SProgram12 -> __runProgram SProgram35 toks (term :> stk)
  SProgram13 -> __runProgram SProgram36 toks (term :> stk)
  SProgram14 -> __runProgram SProgram35 toks (term :> stk)
  SProgram15 -> __runProgram SProgram36 toks (term :> stk)
  SProgram16 -> __runProgram SProgram37 toks (term :> stk)
  SProgram17 -> __runProgram SProgram38 toks (term :> stk)
  SProgram18 -> __runProgram SProgram60 toks (term :> stk)
  SProgram19 -> __runProgram SProgram61 toks (term :> stk)
  SProgram20 -> __runProgram SProgram62 toks (term :> stk)
  SProgram21 -> __runProgram SProgram63 toks (term :> stk)
  SProgram68 -> __runProgram SProgram91 toks (term :> stk)
  SProgram69 -> __runProgram SProgram92 toks (term :> stk)
  SProgram70 -> __runProgram SProgram92 toks (term :> stk)
  SProgram71 -> __runProgram SProgram91 toks (term :> stk)
  SProgram72 -> __runProgram SProgram90 toks (term :> stk)
  SProgram73 -> __runProgram SProgram93 toks (term :> stk)
  SProgram74 -> __runProgram SProgram90 toks (term :> stk)
  SProgram75 -> __runProgram SProgram91 toks (term :> stk)
  SProgram76 -> __runProgram SProgram92 toks (term :> stk)
  SProgram77 -> __runProgram SProgram93 toks (term :> stk)
  SProgram78 -> __runProgram SProgram116 toks (term :> stk)
  SProgram79 -> __runProgram SProgram117 toks (term :> stk)
  SProgram80 -> __runProgram SProgram118 toks (term :> stk)
  SProgram81 -> __runProgram SProgram119 toks (term :> stk)
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
  SProgram102 -> __runProgram SProgram145 toks (term :> stk)
  SProgram103 -> __runProgram SProgram146 toks (term :> stk)
  SProgram135 -> __runProgram SProgram145 toks (term :> stk)
  SProgram136 -> __runProgram SProgram146 toks (term :> stk)
  SProgram155 -> __runProgram SProgram156 toks (term :> stk)
  _ -> error ""

__runProgram :: StProgram a -> ([Lexeme], Pos) -> Stack' StProgram a -> Either (Pos, [String]) Program
__runProgram = \cases {
; SProgram0 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram5 (__input, __end) (() :> (SProgram0, __p, __stk))
; SProgram0 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram41 (__input, __end) (n :> (SProgram0, __p, __stk))
; SProgram0 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram49 (__input, __end) (n :> (SProgram0, __p, __stk))
; SProgram0 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram53 (__input, __end) (n :> (SProgram0, __p, __stk))
; SProgram1 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram5 (__input, __end) (() :> (SProgram1, __p, __stk))
; SProgram1 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram147 (__input, __end) (() :> (SProgram1, __p, __stk))
; SProgram1 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram41 (__input, __end) (n :> (SProgram1, __p, __stk))
; SProgram1 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram49 (__input, __end) (n :> (SProgram1, __p, __stk))
; SProgram1 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram53 (__input, __end) (n :> (SProgram1, __p, __stk))
; SProgram2 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram5 (__input, __end) (() :> (SProgram2, __p, __stk))
; SProgram2 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram148 (__input, __end) (() :> (SProgram2, __p, __stk))
; SProgram2 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram41 (__input, __end) (n :> (SProgram2, __p, __stk))
; SProgram2 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram49 (__input, __end) (n :> (SProgram2, __p, __stk))
; SProgram2 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram53 (__input, __end) (n :> (SProgram2, __p, __stk))
; SProgram3 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram5 (__input, __end) (() :> (SProgram3, __p, __stk))
; SProgram3 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram157 (__input, __end) (() :> (SProgram3, __p, __stk))
; SProgram3 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram41 (__input, __end) (n :> (SProgram3, __p, __stk))
; SProgram3 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram49 (__input, __end) (n :> (SProgram3, __p, __stk))
; SProgram3 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram53 (__input, __end) (n :> (SProgram3, __p, __stk))
; SProgram4 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram7 (__input, __end) (() :> (SProgram4, __p, __stk))
; SProgram4 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram42 (__input, __end) (n :> (SProgram4, __p, __stk))
; SProgram4 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram50 (__input, __end) (n :> (SProgram4, __p, __stk))
; SProgram4 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram54 (__input, __end) (n :> (SProgram4, __p, __stk))
; SProgram5 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram7 (__input, __end) (() :> (SProgram5, __p, __stk))
; SProgram5 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram42 (__input, __end) (n :> (SProgram5, __p, __stk))
; SProgram5 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram50 (__input, __end) (n :> (SProgram5, __p, __stk))
; SProgram5 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram54 (__input, __end) (n :> (SProgram5, __p, __stk))
; SProgram6 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram7 (__input, __end) (() :> (SProgram6, __p, __stk))
; SProgram6 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram42 (__input, __end) (n :> (SProgram6, __p, __stk))
; SProgram6 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram50 (__input, __end) (n :> (SProgram6, __p, __stk))
; SProgram6 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram54 (__input, __end) (n :> (SProgram6, __p, __stk))
; SProgram7 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram7 (__input, __end) (() :> (SProgram7, __p, __stk))
; SProgram7 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram42 (__input, __end) (n :> (SProgram7, __p, __stk))
; SProgram7 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram50 (__input, __end) (n :> (SProgram7, __p, __stk))
; SProgram7 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram54 (__input, __end) (n :> (SProgram7, __p, __stk))
; SProgram8 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram7 (__input, __end) (() :> (SProgram8, __p, __stk))
; SProgram8 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram42 (__input, __end) (n :> (SProgram8, __p, __stk))
; SProgram8 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram50 (__input, __end) (n :> (SProgram8, __p, __stk))
; SProgram8 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram54 (__input, __end) (n :> (SProgram8, __p, __stk))
; SProgram9 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram7 (__input, __end) (() :> (SProgram9, __p, __stk))
; SProgram9 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram42 (__input, __end) (n :> (SProgram9, __p, __stk))
; SProgram9 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram50 (__input, __end) (n :> (SProgram9, __p, __stk))
; SProgram9 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram54 (__input, __end) (n :> (SProgram9, __p, __stk))
; SProgram10 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram7 (__input, __end) (() :> (SProgram10, __p, __stk))
; SProgram10 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram42 (__input, __end) (n :> (SProgram10, __p, __stk))
; SProgram10 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram50 (__input, __end) (n :> (SProgram10, __p, __stk))
; SProgram10 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram54 (__input, __end) (n :> (SProgram10, __p, __stk))
; SProgram11 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram7 (__input, __end) (() :> (SProgram11, __p, __stk))
; SProgram11 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram42 (__input, __end) (n :> (SProgram11, __p, __stk))
; SProgram11 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram50 (__input, __end) (n :> (SProgram11, __p, __stk))
; SProgram11 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram54 (__input, __end) (n :> (SProgram11, __p, __stk))
; SProgram12 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram6 (__input, __end) (() :> (SProgram12, __p, __stk))
; SProgram12 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram39 (__input, __end) (n :> (SProgram12, __p, __stk))
; SProgram12 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram47 (__input, __end) (n :> (SProgram12, __p, __stk))
; SProgram12 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram51 (__input, __end) (n :> (SProgram12, __p, __stk))
; SProgram13 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram4 (__input, __end) (() :> (SProgram13, __p, __stk))
; SProgram13 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram40 (__input, __end) (n :> (SProgram13, __p, __stk))
; SProgram13 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram48 (__input, __end) (n :> (SProgram13, __p, __stk))
; SProgram13 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram52 (__input, __end) (n :> (SProgram13, __p, __stk))
; SProgram14 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram6 (__input, __end) (() :> (SProgram14, __p, __stk))
; SProgram14 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram39 (__input, __end) (n :> (SProgram14, __p, __stk))
; SProgram14 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram47 (__input, __end) (n :> (SProgram14, __p, __stk))
; SProgram14 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram51 (__input, __end) (n :> (SProgram14, __p, __stk))
; SProgram15 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram4 (__input, __end) (() :> (SProgram15, __p, __stk))
; SProgram15 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram40 (__input, __end) (n :> (SProgram15, __p, __stk))
; SProgram15 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram48 (__input, __end) (n :> (SProgram15, __p, __stk))
; SProgram15 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram52 (__input, __end) (n :> (SProgram15, __p, __stk))
; SProgram16 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram5 (__input, __end) (() :> (SProgram16, __p, __stk))
; SProgram16 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram41 (__input, __end) (n :> (SProgram16, __p, __stk))
; SProgram16 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram49 (__input, __end) (n :> (SProgram16, __p, __stk))
; SProgram16 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram53 (__input, __end) (n :> (SProgram16, __p, __stk))
; SProgram17 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram7 (__input, __end) (() :> (SProgram17, __p, __stk))
; SProgram17 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram42 (__input, __end) (n :> (SProgram17, __p, __stk))
; SProgram17 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram50 (__input, __end) (n :> (SProgram17, __p, __stk))
; SProgram17 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram54 (__input, __end) (n :> (SProgram17, __p, __stk))
; SProgram18 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram6 (__input, __end) (() :> (SProgram18, __p, __stk))
; SProgram18 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram39 (__input, __end) (n :> (SProgram18, __p, __stk))
; SProgram18 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram47 (__input, __end) (n :> (SProgram18, __p, __stk))
; SProgram18 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram51 (__input, __end) (n :> (SProgram18, __p, __stk))
; SProgram19 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram4 (__input, __end) (() :> (SProgram19, __p, __stk))
; SProgram19 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram40 (__input, __end) (n :> (SProgram19, __p, __stk))
; SProgram19 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram48 (__input, __end) (n :> (SProgram19, __p, __stk))
; SProgram19 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram52 (__input, __end) (n :> (SProgram19, __p, __stk))
; SProgram20 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram5 (__input, __end) (() :> (SProgram20, __p, __stk))
; SProgram20 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram41 (__input, __end) (n :> (SProgram20, __p, __stk))
; SProgram20 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram49 (__input, __end) (n :> (SProgram20, __p, __stk))
; SProgram20 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram53 (__input, __end) (n :> (SProgram20, __p, __stk))
; SProgram21 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram7 (__input, __end) (() :> (SProgram21, __p, __stk))
; SProgram21 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram42 (__input, __end) (n :> (SProgram21, __p, __stk))
; SProgram21 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram50 (__input, __end) (n :> (SProgram21, __p, __stk))
; SProgram21 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram54 (__input, __end) (n :> (SProgram21, __p, __stk))
; SProgram22 ((__p,  ",") : __input, __end) __stk ->
    __runProgram SProgram0 (__input, __end) (() :> (SProgram22, __p, __stk))
; SProgram23 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram16 (__input, __end) (() :> (SProgram23, __p, __stk))
; SProgram23 ((__p,  "=") : __input, __end) __stk ->
    __runProgram SProgram13 (__input, __end) (() :> (SProgram23, __p, __stk))
; SProgram24 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram17 (__input, __end) (() :> (SProgram24, __p, __stk))
; SProgram24 ((__p,  "=") : __input, __end) __stk ->
    __runProgram SProgram12 (__input, __end) (() :> (SProgram24, __p, __stk))
; SProgram25 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram14 (__input, __end) (() :> (SProgram25, __p, __stk))
; SProgram26 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram15 (__input, __end) (() :> (SProgram26, __p, __stk))
; SProgram27 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram18 (__input, __end) (() :> (SProgram27, __p, __stk))
; SProgram28 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram19 (__input, __end) (() :> (SProgram28, __p, __stk))
; SProgram29 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram20 (__input, __end) (() :> (SProgram29, __p, __stk))
; SProgram30 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram21 (__input, __end) (() :> (SProgram30, __p, __stk))
; SProgram31 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram18 (__input, __end) (() :> (SProgram31, __p, __stk))
; SProgram32 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram19 (__input, __end) (() :> (SProgram32, __p, __stk))
; SProgram33 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram20 (__input, __end) (() :> (SProgram33, __p, __stk))
; SProgram34 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram21 (__input, __end) (() :> (SProgram34, __p, __stk))
; SProgram55 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram64 (__input, __end) (() :> (SProgram55, __p, __stk))
; SProgram56 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram65 (__input, __end) (() :> (SProgram56, __p, __stk))
; SProgram57 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram66 (__input, __end) (() :> (SProgram57, __p, __stk))
; SProgram58 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram67 (__input, __end) (() :> (SProgram58, __p, __stk))
; SProgram68 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram9 (__input, __end) (() :> (SProgram68, __p, __stk))
; SProgram68 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram95 (__input, __end) (n :> (SProgram68, __p, __stk))
; SProgram68 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram102 (__input, __end) (n :> (SProgram68, __p, __stk))
; SProgram68 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram109 (__input, __end) (n :> (SProgram68, __p, __stk))
; SProgram68 ((__p,  "~") : __input, __end) __stk ->
    __runProgram SProgram133 (__input, __end) (() :> (SProgram68, __p, __stk))
; SProgram69 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram10 (__input, __end) (() :> (SProgram69, __p, __stk))
; SProgram69 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram96 (__input, __end) (n :> (SProgram69, __p, __stk))
; SProgram69 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram103 (__input, __end) (n :> (SProgram69, __p, __stk))
; SProgram69 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram110 (__input, __end) (n :> (SProgram69, __p, __stk))
; SProgram69 ((__p,  "~") : __input, __end) __stk ->
    __runProgram SProgram134 (__input, __end) (() :> (SProgram69, __p, __stk))
; SProgram70 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram10 (__input, __end) (() :> (SProgram70, __p, __stk))
; SProgram70 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram96 (__input, __end) (n :> (SProgram70, __p, __stk))
; SProgram70 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram103 (__input, __end) (n :> (SProgram70, __p, __stk))
; SProgram70 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram110 (__input, __end) (n :> (SProgram70, __p, __stk))
; SProgram70 ((__p,  "~") : __input, __end) __stk ->
    __runProgram SProgram134 (__input, __end) (() :> (SProgram70, __p, __stk))
; SProgram71 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram9 (__input, __end) (() :> (SProgram71, __p, __stk))
; SProgram71 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram95 (__input, __end) (n :> (SProgram71, __p, __stk))
; SProgram71 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram102 (__input, __end) (n :> (SProgram71, __p, __stk))
; SProgram71 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram109 (__input, __end) (n :> (SProgram71, __p, __stk))
; SProgram71 ((__p,  "~") : __input, __end) __stk ->
    __runProgram SProgram133 (__input, __end) (() :> (SProgram71, __p, __stk))
; SProgram72 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram8 (__input, __end) (() :> (SProgram72, __p, __stk))
; SProgram72 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram94 (__input, __end) (n :> (SProgram72, __p, __stk))
; SProgram72 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram104 (__input, __end) (n :> (SProgram72, __p, __stk))
; SProgram72 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram108 (__input, __end) (n :> (SProgram72, __p, __stk))
; SProgram73 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram11 (__input, __end) (() :> (SProgram73, __p, __stk))
; SProgram73 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram97 (__input, __end) (n :> (SProgram73, __p, __stk))
; SProgram73 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram107 (__input, __end) (n :> (SProgram73, __p, __stk))
; SProgram73 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram111 (__input, __end) (n :> (SProgram73, __p, __stk))
; SProgram74 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram8 (__input, __end) (() :> (SProgram74, __p, __stk))
; SProgram74 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram94 (__input, __end) (n :> (SProgram74, __p, __stk))
; SProgram74 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram104 (__input, __end) (n :> (SProgram74, __p, __stk))
; SProgram74 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram108 (__input, __end) (n :> (SProgram74, __p, __stk))
; SProgram75 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram9 (__input, __end) (() :> (SProgram75, __p, __stk))
; SProgram75 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram95 (__input, __end) (n :> (SProgram75, __p, __stk))
; SProgram75 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram105 (__input, __end) (n :> (SProgram75, __p, __stk))
; SProgram75 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram109 (__input, __end) (n :> (SProgram75, __p, __stk))
; SProgram76 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram10 (__input, __end) (() :> (SProgram76, __p, __stk))
; SProgram76 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram96 (__input, __end) (n :> (SProgram76, __p, __stk))
; SProgram76 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram106 (__input, __end) (n :> (SProgram76, __p, __stk))
; SProgram76 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram110 (__input, __end) (n :> (SProgram76, __p, __stk))
; SProgram77 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram11 (__input, __end) (() :> (SProgram77, __p, __stk))
; SProgram77 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram97 (__input, __end) (n :> (SProgram77, __p, __stk))
; SProgram77 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram107 (__input, __end) (n :> (SProgram77, __p, __stk))
; SProgram77 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram111 (__input, __end) (n :> (SProgram77, __p, __stk))
; SProgram78 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram8 (__input, __end) (() :> (SProgram78, __p, __stk))
; SProgram78 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram94 (__input, __end) (n :> (SProgram78, __p, __stk))
; SProgram78 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram104 (__input, __end) (n :> (SProgram78, __p, __stk))
; SProgram78 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram108 (__input, __end) (n :> (SProgram78, __p, __stk))
; SProgram79 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram9 (__input, __end) (() :> (SProgram79, __p, __stk))
; SProgram79 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram95 (__input, __end) (n :> (SProgram79, __p, __stk))
; SProgram79 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram105 (__input, __end) (n :> (SProgram79, __p, __stk))
; SProgram79 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram109 (__input, __end) (n :> (SProgram79, __p, __stk))
; SProgram80 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram10 (__input, __end) (() :> (SProgram80, __p, __stk))
; SProgram80 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram96 (__input, __end) (n :> (SProgram80, __p, __stk))
; SProgram80 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram106 (__input, __end) (n :> (SProgram80, __p, __stk))
; SProgram80 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram110 (__input, __end) (n :> (SProgram80, __p, __stk))
; SProgram81 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram11 (__input, __end) (() :> (SProgram81, __p, __stk))
; SProgram81 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram97 (__input, __end) (n :> (SProgram81, __p, __stk))
; SProgram81 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram107 (__input, __end) (n :> (SProgram81, __p, __stk))
; SProgram81 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram111 (__input, __end) (n :> (SProgram81, __p, __stk))
; SProgram82 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram78 (__input, __end) (() :> (SProgram82, __p, __stk))
; SProgram83 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram79 (__input, __end) (() :> (SProgram83, __p, __stk))
; SProgram84 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram80 (__input, __end) (() :> (SProgram84, __p, __stk))
; SProgram85 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram81 (__input, __end) (() :> (SProgram85, __p, __stk))
; SProgram86 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram78 (__input, __end) (() :> (SProgram86, __p, __stk))
; SProgram87 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram79 (__input, __end) (() :> (SProgram87, __p, __stk))
; SProgram88 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram80 (__input, __end) (() :> (SProgram88, __p, __stk))
; SProgram89 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram81 (__input, __end) (() :> (SProgram89, __p, __stk))
; SProgram102 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram1 (__input, __end) (() :> (SProgram102, __p, __stk))
; SProgram103 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram2 (__input, __end) (() :> (SProgram103, __p, __stk))
; SProgram112 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram120 (__input, __end) (() :> (SProgram112, __p, __stk))
; SProgram113 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram121 (__input, __end) (() :> (SProgram113, __p, __stk))
; SProgram114 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram122 (__input, __end) (() :> (SProgram114, __p, __stk))
; SProgram115 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram123 (__input, __end) (() :> (SProgram115, __p, __stk))
; SProgram124 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram75 (__input, __end) (() :> (SProgram124, __p, __stk))
; SProgram124 ((__p,  "=") : __input, __end) __stk ->
    __runProgram SProgram72 (__input, __end) (() :> (SProgram124, __p, __stk))
; SProgram125 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram76 (__input, __end) (() :> (SProgram125, __p, __stk))
; SProgram125 ((__p,  "=") : __input, __end) __stk ->
    __runProgram SProgram73 (__input, __end) (() :> (SProgram125, __p, __stk))
; SProgram126 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram74 (__input, __end) (() :> (SProgram126, __p, __stk))
; SProgram127 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram77 (__input, __end) (() :> (SProgram127, __p, __stk))
; SProgram128 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram131 (__input, __end) (() :> (SProgram128, __p, __stk))
; SProgram128 ((__p,  "-") : __input, __end) __stk ->
    __runProgram SProgram132 (__input, __end) (() :> (SProgram128, __p, __stk))
; SProgram129 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram131 (__input, __end) (() :> (SProgram129, __p, __stk))
; SProgram129 ((__p,  "-") : __input, __end) __stk ->
    __runProgram SProgram132 (__input, __end) (() :> (SProgram129, __p, __stk))
; SProgram130 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram131 (__input, __end) (() :> (SProgram130, __p, __stk))
; SProgram130 ((__p,  "-") : __input, __end) __stk ->
    __runProgram SProgram132 (__input, __end) (() :> (SProgram130, __p, __stk))
; SProgram131 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram135 (__input, __end) (n :> (SProgram131, __p, __stk))
; SProgram132 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram135 (__input, __end) (n :> (SProgram132, __p, __stk))
; SProgram133 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram135 (__input, __end) (n :> (SProgram133, __p, __stk))
; SProgram134 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram136 (__input, __end) (n :> (SProgram134, __p, __stk))
; SProgram135 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram1 (__input, __end) (() :> (SProgram135, __p, __stk))
; SProgram136 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram2 (__input, __end) (() :> (SProgram136, __p, __stk))
; SProgram149 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram151 (__input, __end) (() :> (SProgram149, __p, __stk))
; SProgram150 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram152 (__input, __end) (() :> (SProgram150, __p, __stk))
; SProgram153 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram155 (__input, __end) (n :> (SProgram153, __p, __stk))
; SProgram154 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram155 (__input, __end) (n :> (SProgram154, __p, __stk))
; SProgram155 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram3 (__input, __end) (() :> (SProgram155, __p, __stk))
; SProgram158 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram159 (__input, __end) (() :> (SProgram158, __p, __stk))
; SProgram160 ((__p,  ",") : __input, __end) __stk ->
    __runProgram SProgram128 (__input, __end) (() :> (SProgram160, __p, __stk))
; SProgram161 ((__p,  ",") : __input, __end) __stk ->
    __runProgram SProgram68 (__input, __end) (() :> (SProgram161, __p, __stk))
; SProgram162 ((__p,  ",") : __input, __end) __stk ->
    __runProgram SProgram69 (__input, __end) (() :> (SProgram162, __p, __stk))
; SProgram168 ((__p,  "->") : __input, __end) __stk ->
    __runProgram SProgram70 (__input, __end) (() :> (SProgram168, __p, __stk))
; SProgram168 ((__p,  ".") : __input, __end) __stk ->
    __runProgram SProgram169 (__input, __end) (() :> (SProgram168, __p, __stk))
; SProgram168 ((__p,  "<-") : __input, __end) __stk ->
    __runProgram SProgram71 (__input, __end) (() :> (SProgram168, __p, __stk))
; SProgram168 ((__p,  "=>") : __input, __end) __stk ->
    __runProgram SProgram129 (__input, __end) (() :> (SProgram168, __p, __stk))
; SProgram170 ((__p,  ".") : __input, __end) __stk ->
    __runProgram SProgram173 (__input, __end) (() :> (SProgram170, __p, __stk))
; SProgram171 ((__p,  ".") : __input, __end) __stk ->
    __runProgram SProgram174 (__input, __end) (() :> (SProgram171, __p, __stk))
; SProgram171 ((__p,  "=>") : __input, __end) __stk ->
    __runProgram SProgram130 (__input, __end) (() :> (SProgram171, __p, __stk))
; SProgram172 ((__p,  ".") : __input, __end) __stk ->
    __runProgram SProgram175 (__input, __end) (() :> (SProgram172, __p, __stk))
; SProgram176 ((__p,  ".") : __input, __end) __stk ->
    __runProgram SProgram177 (__input, __end) (() :> (SProgram176, __p, __stk))
-- lookahead ), entity Exprs1
; SProgram22 ((__p,  ")") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoExprs1ForProgram ((__p,  ")") : __input, __end) (action55 __pos e) __stk
-- lookahead ), entity Expr
; SProgram23 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  ")") : __input, __end) (action59 __pos a) __stk
-- lookahead ,, entity Expr
; SProgram23 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  ",") : __input, __end) (action59 __pos a) __stk
-- lookahead ), entity Expr
; SProgram24 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  ")") : __input, __end) (action59 __pos a) __stk
-- lookahead ), entity Expr
; SProgram25 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  ")") : __input, __end) (action58 __pos a
                                                                        b) __stk
-- lookahead ), entity Expr
; SProgram26 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  ")") : __input, __end) (action58 __pos a
                                                                        b) __stk
-- lookahead ,, entity Expr
; SProgram26 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  ",") : __input, __end) (action58 __pos a
                                                                        b) __stk
-- lookahead ), entity Add
; SProgram27 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; SProgram27 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead ), entity Add
; SProgram28 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; SProgram28 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead ,, entity Add
; SProgram28 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead ), entity Add
; SProgram29 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; SProgram29 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead ,, entity Add
; SProgram29 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead =, entity Add
; SProgram29 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action63 __pos a) __stk
-- lookahead ), entity Add
; SProgram30 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; SProgram30 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead =, entity Add
; SProgram30 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action63 __pos a) __stk
-- lookahead ), entity Add
; SProgram31 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead +, entity Add
; SProgram31 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ), entity Add
; SProgram32 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead +, entity Add
; SProgram32 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ,, entity Add
; SProgram32 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ), entity Add
; SProgram33 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead +, entity Add
; SProgram33 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ,, entity Add
; SProgram33 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead =, entity Add
; SProgram33 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ), entity Add
; SProgram34 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead +, entity Add
; SProgram34 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead =, entity Add
; SProgram34 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ), entity Mult
; SProgram35 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; SProgram35 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; SProgram35 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead ), entity Mult
; SProgram36 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; SProgram36 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; SProgram36 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead ,, entity Mult
; SProgram36 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead ), entity Mult
; SProgram37 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; SProgram37 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; SProgram37 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead ,, entity Mult
; SProgram37 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead =, entity Mult
; SProgram37 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action67 __pos a) __stk
-- lookahead ), entity Mult
; SProgram38 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; SProgram38 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; SProgram38 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead =, entity Mult
; SProgram38 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action67 __pos a) __stk
-- lookahead ), entity Term
; SProgram39 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; SProgram39 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; SProgram39 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead ), entity Term
; SProgram40 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; SProgram40 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; SProgram40 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead ,, entity Term
; SProgram40 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead ), entity Term
; SProgram41 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; SProgram41 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; SProgram41 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead ,, entity Term
; SProgram41 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead =, entity Term
; SProgram41 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action71 __pos n) __stk
-- lookahead ), entity Term
; SProgram42 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; SProgram42 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; SProgram42 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead =, entity Term
; SProgram42 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action71 __pos n) __stk
-- lookahead ), entity Term
; SProgram43 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; SProgram43 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; SProgram43 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead ), entity Term
; SProgram44 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; SProgram44 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; SProgram44 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead ,, entity Term
; SProgram44 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead ), entity Term
; SProgram45 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; SProgram45 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; SProgram45 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead ,, entity Term
; SProgram45 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead =, entity Term
; SProgram45 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action72 __pos n) __stk
-- lookahead ), entity Term
; SProgram46 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; SProgram46 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; SProgram46 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead =, entity Term
; SProgram46 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action72 __pos n) __stk
-- lookahead ), entity Const
; SProgram47 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; SProgram47 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; SProgram47 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ), entity Const
; SProgram48 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; SProgram48 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; SProgram48 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ,, entity Const
; SProgram48 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead ), entity Const
; SProgram49 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; SProgram49 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; SProgram49 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ,, entity Const
; SProgram49 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead =, entity Const
; SProgram49 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead ), entity Const
; SProgram50 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; SProgram50 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; SProgram50 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead =, entity Const
; SProgram50 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead ), entity Const
; SProgram51 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; SProgram51 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; SProgram51 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead ), entity Const
; SProgram52 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; SProgram52 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; SProgram52 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead ,, entity Const
; SProgram52 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead ), entity Const
; SProgram53 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; SProgram53 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; SProgram53 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead ,, entity Const
; SProgram53 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead =, entity Const
; SProgram53 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action76 __pos n) __stk
-- lookahead ), entity Const
; SProgram54 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; SProgram54 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; SProgram54 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead =, entity Const
; SProgram54 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action76 __pos n) __stk
-- lookahead ), entity Exprs1
; SProgram59 ((__p,  ")") : __input, __end) ((es :> (_, _, _ :> (_, _, e :> __stk@(_, __pos, _))))) ->
    __gotoExprs1ForProgram ((__p,  ")") : __input, __end) (action54 __pos e
                                                                          es) __stk
-- lookahead ), entity Mult
; SProgram60 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead *, entity Mult
; SProgram60 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead +, entity Mult
; SProgram60 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ), entity Mult
; SProgram61 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead *, entity Mult
; SProgram61 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead +, entity Mult
; SProgram61 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ,, entity Mult
; SProgram61 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ), entity Mult
; SProgram62 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead *, entity Mult
; SProgram62 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead +, entity Mult
; SProgram62 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ,, entity Mult
; SProgram62 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead =, entity Mult
; SProgram62 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ), entity Mult
; SProgram63 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead *, entity Mult
; SProgram63 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead +, entity Mult
; SProgram63 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead =, entity Mult
; SProgram63 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ), entity Term
; SProgram64 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; SProgram64 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; SProgram64 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead ), entity Term
; SProgram65 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; SProgram65 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; SProgram65 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead ,, entity Term
; SProgram65 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead ), entity Term
; SProgram66 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; SProgram66 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; SProgram66 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead ,, entity Term
; SProgram66 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead =, entity Term
; SProgram66 ((__p,  "=") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action70 __pos e) __stk
-- lookahead ), entity Term
; SProgram67 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; SProgram67 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; SProgram67 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead =, entity Term
; SProgram67 ((__p,  "=") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Add
; SProgram82 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead ,, entity Add
; SProgram82 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead ., entity Add
; SProgram82 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; SProgram83 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead ,, entity Add
; SProgram83 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead ., entity Add
; SProgram83 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action63 __pos a) __stk
-- lookahead =, entity Add
; SProgram83 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; SProgram84 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead ,, entity Add
; SProgram84 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead ., entity Add
; SProgram84 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action63 __pos a) __stk
-- lookahead =, entity Add
; SProgram84 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action63 __pos a) __stk
-- lookahead =>, entity Add
; SProgram84 ((__p,  "=>") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "=>") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; SProgram85 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead ,, entity Add
; SProgram85 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead ., entity Add
; SProgram85 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action63 __pos a) __stk
-- lookahead =>, entity Add
; SProgram85 ((__p,  "=>") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "=>") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; SProgram86 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ,, entity Add
; SProgram86 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ., entity Add
; SProgram86 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead +, entity Add
; SProgram87 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ,, entity Add
; SProgram87 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ., entity Add
; SProgram87 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead =, entity Add
; SProgram87 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead +, entity Add
; SProgram88 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ,, entity Add
; SProgram88 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ., entity Add
; SProgram88 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead =, entity Add
; SProgram88 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead =>, entity Add
; SProgram88 ((__p,  "=>") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "=>") : __input, __end) (action62 __pos a
                                                                        b) __stk
-- lookahead +, entity Add
; SProgram89 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ,, entity Add
; SProgram89 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead ., entity Add
; SProgram89 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead =>, entity Add
; SProgram89 ((__p,  "=>") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "=>") : __input, __end) (action62 __pos a
                                                                        b) __stk
-- lookahead *, entity Mult
; SProgram90 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; SProgram90 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead ,, entity Mult
; SProgram90 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead ., entity Mult
; SProgram90 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; SProgram91 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; SProgram91 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead ,, entity Mult
; SProgram91 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead ., entity Mult
; SProgram91 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action67 __pos a) __stk
-- lookahead =, entity Mult
; SProgram91 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; SProgram92 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; SProgram92 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead ,, entity Mult
; SProgram92 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead ., entity Mult
; SProgram92 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action67 __pos a) __stk
-- lookahead =, entity Mult
; SProgram92 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action67 __pos a) __stk
-- lookahead =>, entity Mult
; SProgram92 ((__p,  "=>") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "=>") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; SProgram93 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; SProgram93 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead ,, entity Mult
; SProgram93 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead ., entity Mult
; SProgram93 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action67 __pos a) __stk
-- lookahead =>, entity Mult
; SProgram93 ((__p,  "=>") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "=>") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Term
; SProgram94 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; SProgram94 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead ,, entity Term
; SProgram94 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead ., entity Term
; SProgram94 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; SProgram95 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; SProgram95 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead ,, entity Term
; SProgram95 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead ., entity Term
; SProgram95 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action71 __pos n) __stk
-- lookahead =, entity Term
; SProgram95 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; SProgram96 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; SProgram96 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead ,, entity Term
; SProgram96 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead ., entity Term
; SProgram96 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action71 __pos n) __stk
-- lookahead =, entity Term
; SProgram96 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action71 __pos n) __stk
-- lookahead =>, entity Term
; SProgram96 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=>") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; SProgram97 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; SProgram97 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead ,, entity Term
; SProgram97 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead ., entity Term
; SProgram97 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action71 __pos n) __stk
-- lookahead =>, entity Term
; SProgram97 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=>") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; SProgram98 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; SProgram98 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead ,, entity Term
; SProgram98 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead ., entity Term
; SProgram98 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; SProgram99 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; SProgram99 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead ,, entity Term
; SProgram99 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead ., entity Term
; SProgram99 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action72 __pos n) __stk
-- lookahead =, entity Term
; SProgram99 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; SProgram100 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; SProgram100 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead ,, entity Term
; SProgram100 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead ., entity Term
; SProgram100 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action72 __pos n) __stk
-- lookahead =, entity Term
; SProgram100 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action72 __pos n) __stk
-- lookahead =>, entity Term
; SProgram100 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=>") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; SProgram101 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; SProgram101 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead ,, entity Term
; SProgram101 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead ., entity Term
; SProgram101 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action72 __pos n) __stk
-- lookahead =>, entity Term
; SProgram101 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=>") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Const
; SProgram102 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; SProgram102 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ,, entity Const
; SProgram102 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead ., entity Const
; SProgram102 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action75 __pos n) __stk
-- lookahead =, entity Const
; SProgram102 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; SProgram103 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; SProgram103 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ,, entity Const
; SProgram103 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead ., entity Const
; SProgram103 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action75 __pos n) __stk
-- lookahead =, entity Const
; SProgram103 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead =>, entity Const
; SProgram103 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=>") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; SProgram104 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; SProgram104 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ,, entity Const
; SProgram104 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead ., entity Const
; SProgram104 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; SProgram105 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; SProgram105 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ,, entity Const
; SProgram105 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead ., entity Const
; SProgram105 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action75 __pos n) __stk
-- lookahead =, entity Const
; SProgram105 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; SProgram106 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; SProgram106 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ,, entity Const
; SProgram106 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead ., entity Const
; SProgram106 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action75 __pos n) __stk
-- lookahead =, entity Const
; SProgram106 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead =>, entity Const
; SProgram106 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=>") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; SProgram107 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; SProgram107 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ,, entity Const
; SProgram107 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead ., entity Const
; SProgram107 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action75 __pos n) __stk
-- lookahead =>, entity Const
; SProgram107 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=>") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; SProgram108 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; SProgram108 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead ,, entity Const
; SProgram108 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead ., entity Const
; SProgram108 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; SProgram109 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; SProgram109 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead ,, entity Const
; SProgram109 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead ., entity Const
; SProgram109 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action76 __pos n) __stk
-- lookahead =, entity Const
; SProgram109 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; SProgram110 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; SProgram110 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead ,, entity Const
; SProgram110 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead ., entity Const
; SProgram110 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action76 __pos n) __stk
-- lookahead =, entity Const
; SProgram110 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action76 __pos n) __stk
-- lookahead =>, entity Const
; SProgram110 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=>") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; SProgram111 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; SProgram111 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead ,, entity Const
; SProgram111 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead ., entity Const
; SProgram111 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action76 __pos n) __stk
-- lookahead =>, entity Const
; SProgram111 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=>") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Mult
; SProgram116 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead +, entity Mult
; SProgram116 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ,, entity Mult
; SProgram116 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ., entity Mult
; SProgram116 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead *, entity Mult
; SProgram117 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead +, entity Mult
; SProgram117 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ,, entity Mult
; SProgram117 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ., entity Mult
; SProgram117 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead =, entity Mult
; SProgram117 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead *, entity Mult
; SProgram118 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead +, entity Mult
; SProgram118 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ,, entity Mult
; SProgram118 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ., entity Mult
; SProgram118 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead =, entity Mult
; SProgram118 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead =>, entity Mult
; SProgram118 ((__p,  "=>") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "=>") : __input, __end) (action66 __pos a
                                                                         b) __stk
-- lookahead *, entity Mult
; SProgram119 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead +, entity Mult
; SProgram119 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ,, entity Mult
; SProgram119 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead ., entity Mult
; SProgram119 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead =>, entity Mult
; SProgram119 ((__p,  "=>") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "=>") : __input, __end) (action66 __pos a
                                                                         b) __stk
-- lookahead *, entity Term
; SProgram120 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; SProgram120 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead ,, entity Term
; SProgram120 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead ., entity Term
; SProgram120 ((__p,  ".") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; SProgram121 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; SProgram121 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead ,, entity Term
; SProgram121 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead ., entity Term
; SProgram121 ((__p,  ".") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action70 __pos e) __stk
-- lookahead =, entity Term
; SProgram121 ((__p,  "=") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; SProgram122 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; SProgram122 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead ,, entity Term
; SProgram122 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead ., entity Term
; SProgram122 ((__p,  ".") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action70 __pos e) __stk
-- lookahead =, entity Term
; SProgram122 ((__p,  "=") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action70 __pos e) __stk
-- lookahead =>, entity Term
; SProgram122 ((__p,  "=>") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "=>") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; SProgram123 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; SProgram123 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead ,, entity Term
; SProgram123 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead ., entity Term
; SProgram123 ((__p,  ".") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action70 __pos e) __stk
-- lookahead =>, entity Term
; SProgram123 ((__p,  "=>") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "=>") : __input, __end) (action70 __pos e) __stk
-- lookahead ,, entity Expr
; SProgram124 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  ",") : __input, __end) (action59 __pos a) __stk
-- lookahead ., entity Expr
; SProgram124 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  ".") : __input, __end) (action59 __pos a) __stk
-- lookahead ,, entity Expr
; SProgram125 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  ",") : __input, __end) (action59 __pos a) __stk
-- lookahead ., entity Expr
; SProgram125 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  ".") : __input, __end) (action59 __pos a) __stk
-- lookahead =>, entity Expr
; SProgram125 ((__p,  "=>") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  "=>") : __input, __end) (action59 __pos a) __stk
-- lookahead ,, entity Expr
; SProgram126 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  ",") : __input, __end) (action58 __pos a
                                                                        b) __stk
-- lookahead ., entity Expr
; SProgram126 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  ".") : __input, __end) (action58 __pos a
                                                                        b) __stk
-- lookahead ,, entity Expr
; SProgram127 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  ",") : __input, __end) (action58 __pos a
                                                                        b) __stk
-- lookahead ., entity Expr
; SProgram127 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  ".") : __input, __end) (action58 __pos a
                                                                        b) __stk
-- lookahead =>, entity Expr
; SProgram127 ((__p,  "=>") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  "=>") : __input, __end) (action58 __pos a
                                                                         b) __stk
-- lookahead ,, entity Cond
; SProgram137 ((__p,  ",") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ",") : __input, __end) (action42 __pos c) __stk
-- lookahead ., entity Cond
; SProgram137 ((__p,  ".") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ".") : __input, __end) (action42 __pos c) __stk
-- lookahead ,, entity Cond
; SProgram138 ((__p,  ",") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ",") : __input, __end) (action42 __pos c) __stk
-- lookahead ., entity Cond
; SProgram138 ((__p,  ".") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ".") : __input, __end) (action42 __pos c) __stk
-- lookahead =>, entity Cond
; SProgram138 ((__p,  "=>") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  "=>") : __input, __end) (action42 __pos c) __stk
-- lookahead ,, entity Cond
; SProgram139 ((__p,  ",") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ",") : __input, __end) (action44 __pos e) __stk
-- lookahead ., entity Cond
; SProgram139 ((__p,  ".") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ".") : __input, __end) (action44 __pos e) __stk
-- lookahead ,, entity Cond
; SProgram140 ((__p,  ",") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ",") : __input, __end) (action44 __pos e) __stk
-- lookahead ., entity Cond
; SProgram140 ((__p,  ".") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ".") : __input, __end) (action44 __pos e) __stk
-- lookahead =>, entity Cond
; SProgram140 ((__p,  "=>") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  "=>") : __input, __end) (action44 __pos e) __stk
-- lookahead ,, entity Change
; SProgram141 ((__p,  ",") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoChangeForProgram ((__p,  ",") : __input, __end) (action34 __pos c) __stk
-- lookahead ., entity Change
; SProgram141 ((__p,  ".") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoChangeForProgram ((__p,  ".") : __input, __end) (action34 __pos c) __stk
-- lookahead ,, entity Change
; SProgram142 ((__p,  ",") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoChangeForProgram ((__p,  ",") : __input, __end) (action35 __pos c) __stk
-- lookahead ., entity Change
; SProgram142 ((__p,  ".") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoChangeForProgram ((__p,  ".") : __input, __end) (action35 __pos c) __stk
-- lookahead ,, entity Cond
; SProgram143 ((__p,  ",") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoCondForProgram ((__p,  ",") : __input, __end) (action43 __pos c) __stk
-- lookahead ., entity Cond
; SProgram143 ((__p,  ".") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoCondForProgram ((__p,  ".") : __input, __end) (action43 __pos c) __stk
-- lookahead ,, entity Cond
; SProgram144 ((__p,  ",") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoCondForProgram ((__p,  ",") : __input, __end) (action43 __pos c) __stk
-- lookahead ., entity Cond
; SProgram144 ((__p,  ".") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoCondForProgram ((__p,  ".") : __input, __end) (action43 __pos c) __stk
-- lookahead =>, entity Cond
; SProgram144 ((__p,  "=>") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoCondForProgram ((__p,  "=>") : __input, __end) (action43 __pos c) __stk
-- lookahead ,, entity Call
; SProgram145 ((__p,  ",") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  ",") : __input, __end) (action47 __pos pre
                                                                        t) __stk
-- lookahead ., entity Call
; SProgram145 ((__p,  ".") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  ".") : __input, __end) (action47 __pos pre
                                                                        t) __stk
-- lookahead ,, entity Call
; SProgram146 ((__p,  ",") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  ",") : __input, __end) (action47 __pos pre
                                                                        t) __stk
-- lookahead ., entity Call
; SProgram146 ((__p,  ".") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  ".") : __input, __end) (action47 __pos pre
                                                                        t) __stk
-- lookahead =>, entity Call
; SProgram146 ((__p,  "=>") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  "=>") : __input, __end) (action47 __pos pre
                                                                         t) __stk
-- lookahead ,, entity Tuple
; SProgram147 ((__p,  ",") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  ",") : __input, __end) (action50 __pos ) __stk
-- lookahead ., entity Tuple
; SProgram147 ((__p,  ".") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  ".") : __input, __end) (action50 __pos ) __stk
-- lookahead ,, entity Tuple
; SProgram148 ((__p,  ",") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  ",") : __input, __end) (action50 __pos ) __stk
-- lookahead ., entity Tuple
; SProgram148 ((__p,  ".") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  ".") : __input, __end) (action50 __pos ) __stk
-- lookahead =>, entity Tuple
; SProgram148 ((__p,  "=>") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  "=>") : __input, __end) (action50 __pos ) __stk
-- lookahead ,, entity Tuple
; SProgram151 ((__p,  ",") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  ",") : __input, __end) (action51 __pos es) __stk
-- lookahead ., entity Tuple
; SProgram151 ((__p,  ".") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  ".") : __input, __end) (action51 __pos es) __stk
-- lookahead ,, entity Tuple
; SProgram152 ((__p,  ",") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  ",") : __input, __end) (action51 __pos es) __stk
-- lookahead ., entity Tuple
; SProgram152 ((__p,  ".") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  ".") : __input, __end) (action51 __pos es) __stk
-- lookahead =>, entity Tuple
; SProgram152 ((__p,  "=>") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  "=>") : __input, __end) (action51 __pos es) __stk
-- lookahead <eof>, entity Stmts
; SProgram154 ([], __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoStmtsForProgram ([], __end) (action14 __pos c) __stk
-- lookahead ->, entity Call
; SProgram156 ((__p,  "->") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  "->") : __input, __end) (action47 __pos pre
                                                                         t) __stk
-- lookahead ., entity Call
; SProgram156 ((__p,  ".") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  ".") : __input, __end) (action47 __pos pre
                                                                        t) __stk
-- lookahead <-, entity Call
; SProgram156 ((__p,  "<-") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  "<-") : __input, __end) (action47 __pos pre
                                                                         t) __stk
-- lookahead =>, entity Call
; SProgram156 ((__p,  "=>") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  "=>") : __input, __end) (action47 __pos pre
                                                                         t) __stk
-- lookahead ->, entity Tuple
; SProgram157 ((__p,  "->") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  "->") : __input, __end) (action50 __pos ) __stk
-- lookahead ., entity Tuple
; SProgram157 ((__p,  ".") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  ".") : __input, __end) (action50 __pos ) __stk
-- lookahead <-, entity Tuple
; SProgram157 ((__p,  "<-") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  "<-") : __input, __end) (action50 __pos ) __stk
-- lookahead =>, entity Tuple
; SProgram157 ((__p,  "=>") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  "=>") : __input, __end) (action50 __pos ) __stk
-- lookahead ->, entity Tuple
; SProgram159 ((__p,  "->") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  "->") : __input, __end) (action51 __pos es) __stk
-- lookahead ., entity Tuple
; SProgram159 ((__p,  ".") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  ".") : __input, __end) (action51 __pos es) __stk
-- lookahead <-, entity Tuple
; SProgram159 ((__p,  "<-") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  "<-") : __input, __end) (action51 __pos es) __stk
-- lookahead =>, entity Tuple
; SProgram159 ((__p,  "=>") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  "=>") : __input, __end) (action51 __pos es) __stk
-- lookahead ., entity Changes
; SProgram160 ((__p,  ".") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoChangesForProgram ((__p,  ".") : __input, __end) (action31 __pos c) __stk
-- lookahead ., entity Conds
; SProgram161 ((__p,  ".") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondsForProgram ((__p,  ".") : __input, __end) (action39 __pos c) __stk
-- lookahead ., entity Conds
; SProgram162 ((__p,  ".") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondsForProgram ((__p,  ".") : __input, __end) (action39 __pos c) __stk
-- lookahead =>, entity Conds
; SProgram162 ((__p,  "=>") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondsForProgram ((__p,  "=>") : __input, __end) (action39 __pos c) __stk
-- lookahead ., entity Changes
; SProgram163 ((__p,  ".") : __input, __end) ((cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _))))) ->
    __gotoChangesForProgram ((__p,  ".") : __input, __end) (action30 __pos c
                                                                           cs) __stk
-- lookahead ., entity Conds
; SProgram164 ((__p,  ".") : __input, __end) ((cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _))))) ->
    __gotoCondsForProgram ((__p,  ".") : __input, __end) (action38 __pos c
                                                                         cs) __stk
-- lookahead ., entity Conds
; SProgram165 ((__p,  ".") : __input, __end) ((cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _))))) ->
    __gotoCondsForProgram ((__p,  ".") : __input, __end) (action38 __pos c
                                                                         cs) __stk
-- lookahead =>, entity Conds
; SProgram165 ((__p,  "=>") : __input, __end) ((cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _))))) ->
    __gotoCondsForProgram ((__p,  "=>") : __input, __end) (action38 __pos c
                                                                          cs) __stk
-- lookahead <name>, entity Stmt
; SProgram166 ((__p, LowercaseName tok) : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoStmtForProgram ((__p, LowercaseName tok) : __input, __end) (action17 __pos c) __stk
-- lookahead <eof>, entity Stmt
; SProgram166 ([], __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoStmtForProgram ([], __end) (action17 __pos c) __stk
-- lookahead <name>, entity Stmt
; SProgram167 ((__p, LowercaseName tok) : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoStmtForProgram ((__p, LowercaseName tok) : __input, __end) (action18 __pos e) __stk
-- lookahead <eof>, entity Stmt
; SProgram167 ([], __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoStmtForProgram ([], __end) (action18 __pos e) __stk
-- lookahead <name>, entity Clause
; SProgram169 ((__p, LowercaseName tok) : __input, __end) ((_ :> (_, _, c :> __stk@(_, __pos, _)))) ->
    __gotoClauseForProgram ((__p, LowercaseName tok) : __input, __end) (action26 __pos c) __stk
-- lookahead <eof>, entity Clause
; SProgram169 ([], __end) ((_ :> (_, _, c :> __stk@(_, __pos, _)))) ->
    __gotoClauseForProgram ([], __end) (action26 __pos c) __stk
-- lookahead <name>, entity Effect
; SProgram173 ((__p, LowercaseName tok) : __input, __end) ((_ :> (_, _, ds :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))) ->
    __gotoEffectForProgram ((__p, LowercaseName tok) : __input, __end) (action21 __pos c
                                                                                       ds) __stk
-- lookahead <eof>, entity Effect
; SProgram173 ([], __end) ((_ :> (_, _, ds :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))) ->
    __gotoEffectForProgram ([], __end) (action21 __pos c ds) __stk
-- lookahead <name>, entity Effect
; SProgram174 ((__p, LowercaseName tok) : __input, __end) ((_ :> (_, _, cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))) ->
    __gotoEffectForProgram ((__p, LowercaseName tok) : __input, __end) (action23 __pos c
                                                                                       cs) __stk
-- lookahead <eof>, entity Effect
; SProgram174 ([], __end) ((_ :> (_, _, cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))) ->
    __gotoEffectForProgram ([], __end) (action23 __pos c cs) __stk
-- lookahead <name>, entity Clause
; SProgram175 ((__p, LowercaseName tok) : __input, __end) ((_ :> (_, _, cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))) ->
    __gotoClauseForProgram ((__p, LowercaseName tok) : __input, __end) (action27 __pos c
                                                                                       cs) __stk
-- lookahead <eof>, entity Clause
; SProgram175 ([], __end) ((_ :> (_, _, cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))) ->
    __gotoClauseForProgram ([], __end) (action27 __pos c cs) __stk
-- lookahead <name>, entity Effect
; SProgram177 ((__p, LowercaseName tok) : __input, __end) ((_ :> (_, _, ds :> (_, _, _ :> (_, _, cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))))) ->
    __gotoEffectForProgram ((__p, LowercaseName tok) : __input, __end) (action22 __pos c
                                                                                       cs ds) __stk
-- lookahead <eof>, entity Effect
; SProgram177 ([], __end) ((_ :> (_, _, ds :> (_, _, _ :> (_, _, cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))))) ->
    __gotoEffectForProgram ([], __end) (action22 __pos c cs ds) __stk
-- lookahead <eof>, entity Program
; SProgram178 ([], __end) ((res :> __stk@(_, __pos, _))) ->
    pure res
-- lookahead <eof>, entity Program
; SProgram179 ([], __end) ((stmts :> __stk@(_, __pos, _))) ->
    __gotoProgramForProgram ([], __end) (action10 __pos stmts) __stk
-- lookahead <eof>, entity Stmts
; SProgram180 ([], __end) ((cs :> (_, _, c :> __stk@(_, __pos, _)))) ->
    __gotoStmtsForProgram ([], __end) (action13 __pos c cs) __stk
; SProgram0 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram1 __input _ ->
    Left  (currentPos __input, ["(", ")", "<Name>", "<name>", "<num>"])
; SProgram2 __input _ ->
    Left  (currentPos __input, ["(", ")", "<Name>", "<name>", "<num>"])
; SProgram3 __input _ ->
    Left  (currentPos __input, ["(", ")", "<Name>", "<name>", "<num>"])
; SProgram4 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram5 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram6 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram7 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram8 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram9 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram10 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram11 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram12 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram13 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram14 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram15 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram16 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram17 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram18 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram19 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram20 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram21 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram22 __input _ -> Left  (currentPos __input, [")", ","])
; SProgram23 __input _ ->
    Left  (currentPos __input, [")", "+", ",", "="])
; SProgram24 __input _ ->
    Left  (currentPos __input, [")", "+", "="])
; SProgram25 __input _ -> Left  (currentPos __input, [")", "+"])
; SProgram26 __input _ ->
    Left  (currentPos __input, [")", "+", ","])
; SProgram27 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram28 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram29 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram30 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram31 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram32 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram33 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram34 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram35 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram36 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram37 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram38 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram39 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram40 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram41 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram42 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram43 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram44 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram45 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram46 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram47 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram48 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram49 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram50 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram51 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram52 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram53 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram54 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram55 __input _ -> Left  (currentPos __input, [")"])
; SProgram56 __input _ -> Left  (currentPos __input, [")"])
; SProgram57 __input _ -> Left  (currentPos __input, [")"])
; SProgram58 __input _ -> Left  (currentPos __input, [")"])
; SProgram59 __input _ -> Left  (currentPos __input, [")"])
; SProgram60 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram61 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram62 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram63 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram64 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram65 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram66 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram67 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram68 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>", "~"])
; SProgram69 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>", "~"])
; SProgram70 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>", "~"])
; SProgram71 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>", "~"])
; SProgram72 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram73 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram74 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram75 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram76 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram77 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram78 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram79 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram80 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram81 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram82 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram83 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram84 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram85 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram86 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram87 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram88 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram89 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram90 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram91 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram92 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram93 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram94 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram95 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram96 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram97 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram98 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram99 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram100 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram101 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram102 __input _ ->
    Left  (currentPos __input, ["(", "*", "+", ",", ".", "="])
; SProgram103 __input _ ->
    Left  (currentPos __input, ["(", "*", "+", ",", ".", "=", "=>"])
; SProgram104 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram105 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram106 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram107 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram108 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram109 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram110 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram111 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram112 __input _ -> Left  (currentPos __input, [")"])
; SProgram113 __input _ -> Left  (currentPos __input, [")"])
; SProgram114 __input _ -> Left  (currentPos __input, [")"])
; SProgram115 __input _ -> Left  (currentPos __input, [")"])
; SProgram116 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram117 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram118 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram119 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram120 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram121 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram122 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram123 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram124 __input _ ->
    Left  (currentPos __input, ["+", ",", ".", "="])
; SProgram125 __input _ ->
    Left  (currentPos __input, ["+", ",", ".", "=", "=>"])
; SProgram126 __input _ ->
    Left  (currentPos __input, ["+", ",", "."])
; SProgram127 __input _ ->
    Left  (currentPos __input, ["+", ",", ".", "=>"])
; SProgram128 __input _ -> Left  (currentPos __input, ["+", "-"])
; SProgram129 __input _ -> Left  (currentPos __input, ["+", "-"])
; SProgram130 __input _ -> Left  (currentPos __input, ["+", "-"])
; SProgram131 __input _ -> Left  (currentPos __input, ["<name>"])
; SProgram132 __input _ -> Left  (currentPos __input, ["<name>"])
; SProgram133 __input _ -> Left  (currentPos __input, ["<name>"])
; SProgram134 __input _ -> Left  (currentPos __input, ["<name>"])
; SProgram135 __input _ -> Left  (currentPos __input, ["("])
; SProgram136 __input _ -> Left  (currentPos __input, ["("])
; SProgram137 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram138 __input _ ->
    Left  (currentPos __input, [",", ".", "=>"])
; SProgram139 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram140 __input _ ->
    Left  (currentPos __input, [",", ".", "=>"])
; SProgram141 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram142 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram143 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram144 __input _ ->
    Left  (currentPos __input, [",", ".", "=>"])
; SProgram145 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram146 __input _ ->
    Left  (currentPos __input, [",", ".", "=>"])
; SProgram147 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram148 __input _ ->
    Left  (currentPos __input, [",", ".", "=>"])
; SProgram149 __input _ -> Left  (currentPos __input, [")"])
; SProgram150 __input _ -> Left  (currentPos __input, [")"])
; SProgram151 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram152 __input _ ->
    Left  (currentPos __input, [",", ".", "=>"])
; SProgram153 __input _ -> Left  (currentPos __input, ["<name>"])
; SProgram154 __input _ ->
    Left  (currentPos __input, ["<name>", "<eof>"])
; SProgram155 __input _ -> Left  (currentPos __input, ["("])
; SProgram156 __input _ ->
    Left  (currentPos __input, ["->", ".", "<-", "=>"])
; SProgram157 __input _ ->
    Left  (currentPos __input, ["->", ".", "<-", "=>"])
; SProgram158 __input _ -> Left  (currentPos __input, [")"])
; SProgram159 __input _ ->
    Left  (currentPos __input, ["->", ".", "<-", "=>"])
; SProgram160 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram161 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram162 __input _ ->
    Left  (currentPos __input, [",", ".", "=>"])
; SProgram163 __input _ -> Left  (currentPos __input, ["."])
; SProgram164 __input _ -> Left  (currentPos __input, ["."])
; SProgram165 __input _ -> Left  (currentPos __input, [".", "=>"])
; SProgram166 __input _ ->
    Left  (currentPos __input, ["<name>", "<eof>"])
; SProgram167 __input _ ->
    Left  (currentPos __input, ["<name>", "<eof>"])
; SProgram168 __input _ ->
    Left  (currentPos __input, ["->", ".", "<-", "=>"])
; SProgram169 __input _ ->
    Left  (currentPos __input, ["<name>", "<eof>"])
; SProgram170 __input _ -> Left  (currentPos __input, ["."])
; SProgram171 __input _ -> Left  (currentPos __input, [".", "=>"])
; SProgram172 __input _ -> Left  (currentPos __input, ["."])
; SProgram173 __input _ ->
    Left  (currentPos __input, ["<name>", "<eof>"])
; SProgram174 __input _ ->
    Left  (currentPos __input, ["<name>", "<eof>"])
; SProgram175 __input _ ->
    Left  (currentPos __input, ["<name>", "<eof>"])
; SProgram176 __input _ -> Left  (currentPos __input, ["."])
; SProgram177 __input _ ->
    Left  (currentPos __input, ["<name>", "<eof>"])
; SProgram178 __input _ -> Left  (currentPos __input, ["<eof>"])
; SProgram179 __input _ -> Left  (currentPos __input, ["<eof>"])
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
    Right input -> pure (Right (__runProgram SProgram153 input Nil))
data StTestSuite :: [Kind.Type] -> Kind.Type where
  STestSuite0 :: StTestSuite (() : Expr : a)
  STestSuite1 :: StTestSuite (() : a)
  STestSuite2 :: StTestSuite (() : a)
  STestSuite3 :: StTestSuite (() : a)
  STestSuite4 :: StTestSuite (() : a)
  STestSuite5 :: StTestSuite (() : a)
  STestSuite6 :: StTestSuite (() : a)
  STestSuite7 :: StTestSuite (() : a)
  STestSuite8 :: StTestSuite (() : Expr : a)
  STestSuite9 :: StTestSuite (() : Expr : a)
  STestSuite10 :: StTestSuite (() : Expr : a)
  STestSuite11 :: StTestSuite (() : Expr : a)
  STestSuite12 :: StTestSuite (() : Expr : a)
  STestSuite13 :: StTestSuite (() : Expr : a)
  STestSuite14 :: StTestSuite (() : Expr : a)
  STestSuite15 :: StTestSuite (() : Expr : a)
  STestSuite16 :: StTestSuite (() : Expr : a)
  STestSuite17 :: StTestSuite (() : Expr : a)
  STestSuite18 :: StTestSuite (Expr : a)
  STestSuite19 :: StTestSuite (Expr : a)
  STestSuite20 :: StTestSuite (Expr : a)
  STestSuite21 :: StTestSuite (Expr : () : Expr : a)
  STestSuite22 :: StTestSuite (Expr : () : Expr : a)
  STestSuite23 :: StTestSuite (Expr : a)
  STestSuite24 :: StTestSuite (Expr : a)
  STestSuite25 :: StTestSuite (Expr : a)
  STestSuite26 :: StTestSuite (Expr : a)
  STestSuite27 :: StTestSuite (Expr : () : Expr : a)
  STestSuite28 :: StTestSuite (Expr : () : Expr : a)
  STestSuite29 :: StTestSuite (Expr : () : Expr : a)
  STestSuite30 :: StTestSuite (Expr : () : Expr : a)
  STestSuite31 :: StTestSuite (Expr : a)
  STestSuite32 :: StTestSuite (Expr : a)
  STestSuite33 :: StTestSuite (Expr : a)
  STestSuite34 :: StTestSuite (Expr : a)
  STestSuite35 :: StTestSuite (Text : a)
  STestSuite36 :: StTestSuite (Text : a)
  STestSuite37 :: StTestSuite (Text : a)
  STestSuite38 :: StTestSuite (Text : a)
  STestSuite39 :: StTestSuite (Const : a)
  STestSuite40 :: StTestSuite (Const : a)
  STestSuite41 :: StTestSuite (Const : a)
  STestSuite42 :: StTestSuite (Const : a)
  STestSuite43 :: StTestSuite (Text : a)
  STestSuite44 :: StTestSuite (Text : a)
  STestSuite45 :: StTestSuite (Text : a)
  STestSuite46 :: StTestSuite (Text : a)
  STestSuite47 :: StTestSuite (Integer : a)
  STestSuite48 :: StTestSuite (Integer : a)
  STestSuite49 :: StTestSuite (Integer : a)
  STestSuite50 :: StTestSuite (Integer : a)
  STestSuite51 :: StTestSuite (Expr : () : a)
  STestSuite52 :: StTestSuite (Expr : () : a)
  STestSuite53 :: StTestSuite (Expr : () : a)
  STestSuite54 :: StTestSuite (Expr : () : a)
  STestSuite55 :: StTestSuite ([Expr] : () : Expr : a)
  STestSuite56 :: StTestSuite (Expr : () : Expr : a)
  STestSuite57 :: StTestSuite (Expr : () : Expr : a)
  STestSuite58 :: StTestSuite (Expr : () : Expr : a)
  STestSuite59 :: StTestSuite (Expr : () : Expr : a)
  STestSuite60 :: StTestSuite (() : Expr : () : a)
  STestSuite61 :: StTestSuite (() : Expr : () : a)
  STestSuite62 :: StTestSuite (() : Expr : () : a)
  STestSuite63 :: StTestSuite (() : Expr : () : a)
  STestSuite64 :: StTestSuite (() : a)
  STestSuite65 :: StTestSuite (() : Expr : a)
  STestSuite66 :: StTestSuite (() : Expr : a)
  STestSuite67 :: StTestSuite (() : Expr : a)
  STestSuite68 :: StTestSuite (() : Expr : a)
  STestSuite69 :: StTestSuite (() : Expr : a)
  STestSuite70 :: StTestSuite (Expr : a)
  STestSuite71 :: StTestSuite (Expr : a)
  STestSuite72 :: StTestSuite (Expr : () : Expr : a)
  STestSuite73 :: StTestSuite (Expr : () : Expr : a)
  STestSuite74 :: StTestSuite (Expr : a)
  STestSuite75 :: StTestSuite (Expr : a)
  STestSuite76 :: StTestSuite (Text : a)
  STestSuite77 :: StTestSuite (Text : a)
  STestSuite78 :: StTestSuite (Const : a)
  STestSuite79 :: StTestSuite (Const : a)
  STestSuite80 :: StTestSuite (Text : a)
  STestSuite81 :: StTestSuite (Text : a)
  STestSuite82 :: StTestSuite (Integer : a)
  STestSuite83 :: StTestSuite (Integer : a)
  STestSuite84 :: StTestSuite (Expr : () : a)
  STestSuite85 :: StTestSuite (Expr : () : a)
  STestSuite86 :: StTestSuite (Expr : () : Expr : a)
  STestSuite87 :: StTestSuite (Expr : () : Expr : a)
  STestSuite88 :: StTestSuite (() : Expr : () : a)
  STestSuite89 :: StTestSuite (() : Expr : () : a)
  STestSuite90 :: StTestSuite (Expr : a)
  STestSuite91 :: StTestSuite (Expr : () : Expr : a)
  STestSuite92 :: StTestSuite (() : a)
  STestSuite93 :: StTestSuite (() : a)
  STestSuite94 :: StTestSuite (Text : a)
  STestSuite95 :: StTestSuite (() : a)
  STestSuite96 :: StTestSuite (Test : a)
  STestSuite97 :: StTestSuite ([Expr] : Text : a)
  STestSuite98 :: StTestSuite (() : () : a)
  STestSuite99 :: StTestSuite ([Expr] : () : a)
  STestSuite100 :: StTestSuite (Call : () : a)
  STestSuite101 :: StTestSuite (Call : () : a)
  STestSuite102 :: StTestSuite (Expr : () : a)
  STestSuite103 :: StTestSuite (() : [Expr] : () : a)
  STestSuite104 :: StTestSuite (a)
  STestSuite105 :: StTestSuite (TestSuite : a)
  STestSuite106 :: StTestSuite ([Test] : () : a)
  STestSuite107 :: StTestSuite ([Test] : Test : a)

__gotoAddForTestSuite :: ([Lexeme], Pos) -> Expr -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoAddForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite0 -> __runTestSuite STestSuite19 toks (term :> stk)
  STestSuite1 -> __runTestSuite STestSuite19 toks (term :> stk)
  STestSuite2 -> __runTestSuite STestSuite20 toks (term :> stk)
  STestSuite3 -> __runTestSuite STestSuite20 toks (term :> stk)
  STestSuite4 -> __runTestSuite STestSuite20 toks (term :> stk)
  STestSuite5 -> __runTestSuite STestSuite20 toks (term :> stk)
  STestSuite6 -> __runTestSuite STestSuite20 toks (term :> stk)
  STestSuite7 -> __runTestSuite STestSuite20 toks (term :> stk)
  STestSuite8 -> __runTestSuite STestSuite21 toks (term :> stk)
  STestSuite9 -> __runTestSuite STestSuite22 toks (term :> stk)
  STestSuite64 -> __runTestSuite STestSuite90 toks (term :> stk)
  STestSuite65 -> __runTestSuite STestSuite91 toks (term :> stk)
  _ -> error ""

__gotoCallForTestSuite :: ([Lexeme], Pos) -> Call -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoCallForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite92 -> __runTestSuite STestSuite100 toks (term :> stk)
  STestSuite93 -> __runTestSuite STestSuite101 toks (term :> stk)
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
  STestSuite0 -> __runTestSuite STestSuite41 toks (term :> stk)
  STestSuite1 -> __runTestSuite STestSuite41 toks (term :> stk)
  STestSuite2 -> __runTestSuite STestSuite42 toks (term :> stk)
  STestSuite3 -> __runTestSuite STestSuite42 toks (term :> stk)
  STestSuite4 -> __runTestSuite STestSuite42 toks (term :> stk)
  STestSuite5 -> __runTestSuite STestSuite42 toks (term :> stk)
  STestSuite6 -> __runTestSuite STestSuite42 toks (term :> stk)
  STestSuite7 -> __runTestSuite STestSuite42 toks (term :> stk)
  STestSuite8 -> __runTestSuite STestSuite39 toks (term :> stk)
  STestSuite9 -> __runTestSuite STestSuite40 toks (term :> stk)
  STestSuite10 -> __runTestSuite STestSuite39 toks (term :> stk)
  STestSuite11 -> __runTestSuite STestSuite40 toks (term :> stk)
  STestSuite12 -> __runTestSuite STestSuite41 toks (term :> stk)
  STestSuite13 -> __runTestSuite STestSuite42 toks (term :> stk)
  STestSuite14 -> __runTestSuite STestSuite39 toks (term :> stk)
  STestSuite15 -> __runTestSuite STestSuite40 toks (term :> stk)
  STestSuite16 -> __runTestSuite STestSuite41 toks (term :> stk)
  STestSuite17 -> __runTestSuite STestSuite42 toks (term :> stk)
  STestSuite64 -> __runTestSuite STestSuite78 toks (term :> stk)
  STestSuite65 -> __runTestSuite STestSuite79 toks (term :> stk)
  STestSuite66 -> __runTestSuite STestSuite78 toks (term :> stk)
  STestSuite67 -> __runTestSuite STestSuite79 toks (term :> stk)
  STestSuite68 -> __runTestSuite STestSuite78 toks (term :> stk)
  STestSuite69 -> __runTestSuite STestSuite79 toks (term :> stk)
  _ -> error ""

__gotoEffectForTestSuite :: ([Lexeme], Pos) -> Effect -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoEffectForTestSuite toks term stk@(state, _, _) = case state of
  _ -> error ""

__gotoExprForTestSuite :: ([Lexeme], Pos) -> Expr -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoExprForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite0 -> __runTestSuite STestSuite18 toks (term :> stk)
  STestSuite1 -> __runTestSuite STestSuite18 toks (term :> stk)
  STestSuite2 -> __runTestSuite STestSuite52 toks (term :> stk)
  STestSuite3 -> __runTestSuite STestSuite53 toks (term :> stk)
  STestSuite4 -> __runTestSuite STestSuite51 toks (term :> stk)
  STestSuite5 -> __runTestSuite STestSuite54 toks (term :> stk)
  STestSuite6 -> __runTestSuite STestSuite84 toks (term :> stk)
  STestSuite7 -> __runTestSuite STestSuite85 toks (term :> stk)
  STestSuite64 -> __runTestSuite STestSuite102 toks (term :> stk)
  _ -> error ""

__gotoExprs1ForTestSuite :: ([Lexeme], Pos) -> [Expr] -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoExprs1ForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite0 -> __runTestSuite STestSuite55 toks (term :> stk)
  STestSuite1 -> __runTestSuite STestSuite99 toks (term :> stk)
  _ -> error ""

__gotoMultForTestSuite :: ([Lexeme], Pos) -> Expr -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoMultForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite0 -> __runTestSuite STestSuite25 toks (term :> stk)
  STestSuite1 -> __runTestSuite STestSuite25 toks (term :> stk)
  STestSuite2 -> __runTestSuite STestSuite26 toks (term :> stk)
  STestSuite3 -> __runTestSuite STestSuite26 toks (term :> stk)
  STestSuite4 -> __runTestSuite STestSuite26 toks (term :> stk)
  STestSuite5 -> __runTestSuite STestSuite26 toks (term :> stk)
  STestSuite6 -> __runTestSuite STestSuite26 toks (term :> stk)
  STestSuite7 -> __runTestSuite STestSuite26 toks (term :> stk)
  STestSuite8 -> __runTestSuite STestSuite23 toks (term :> stk)
  STestSuite9 -> __runTestSuite STestSuite24 toks (term :> stk)
  STestSuite10 -> __runTestSuite STestSuite27 toks (term :> stk)
  STestSuite11 -> __runTestSuite STestSuite28 toks (term :> stk)
  STestSuite12 -> __runTestSuite STestSuite29 toks (term :> stk)
  STestSuite13 -> __runTestSuite STestSuite30 toks (term :> stk)
  STestSuite64 -> __runTestSuite STestSuite70 toks (term :> stk)
  STestSuite65 -> __runTestSuite STestSuite71 toks (term :> stk)
  STestSuite66 -> __runTestSuite STestSuite72 toks (term :> stk)
  STestSuite67 -> __runTestSuite STestSuite73 toks (term :> stk)
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
  STestSuite0 -> __runTestSuite STestSuite33 toks (term :> stk)
  STestSuite1 -> __runTestSuite STestSuite33 toks (term :> stk)
  STestSuite2 -> __runTestSuite STestSuite34 toks (term :> stk)
  STestSuite3 -> __runTestSuite STestSuite34 toks (term :> stk)
  STestSuite4 -> __runTestSuite STestSuite34 toks (term :> stk)
  STestSuite5 -> __runTestSuite STestSuite34 toks (term :> stk)
  STestSuite6 -> __runTestSuite STestSuite34 toks (term :> stk)
  STestSuite7 -> __runTestSuite STestSuite34 toks (term :> stk)
  STestSuite8 -> __runTestSuite STestSuite31 toks (term :> stk)
  STestSuite9 -> __runTestSuite STestSuite32 toks (term :> stk)
  STestSuite10 -> __runTestSuite STestSuite31 toks (term :> stk)
  STestSuite11 -> __runTestSuite STestSuite32 toks (term :> stk)
  STestSuite12 -> __runTestSuite STestSuite33 toks (term :> stk)
  STestSuite13 -> __runTestSuite STestSuite34 toks (term :> stk)
  STestSuite14 -> __runTestSuite STestSuite56 toks (term :> stk)
  STestSuite15 -> __runTestSuite STestSuite57 toks (term :> stk)
  STestSuite16 -> __runTestSuite STestSuite58 toks (term :> stk)
  STestSuite17 -> __runTestSuite STestSuite59 toks (term :> stk)
  STestSuite64 -> __runTestSuite STestSuite74 toks (term :> stk)
  STestSuite65 -> __runTestSuite STestSuite75 toks (term :> stk)
  STestSuite66 -> __runTestSuite STestSuite74 toks (term :> stk)
  STestSuite67 -> __runTestSuite STestSuite75 toks (term :> stk)
  STestSuite68 -> __runTestSuite STestSuite86 toks (term :> stk)
  STestSuite69 -> __runTestSuite STestSuite87 toks (term :> stk)
  _ -> error ""

__gotoTestForTestSuite :: ([Lexeme], Pos) -> Test -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoTestForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite95 -> __runTestSuite STestSuite96 toks (term :> stk)
  STestSuite96 -> __runTestSuite STestSuite96 toks (term :> stk)
  _ -> error ""

__gotoTestSuiteForTestSuite :: ([Lexeme], Pos) -> TestSuite -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoTestSuiteForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite104 -> __runTestSuite STestSuite105 toks (term :> stk)
  _ -> error ""

__gotoTestsForTestSuite :: ([Lexeme], Pos) -> [Test] -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoTestsForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite95 -> __runTestSuite STestSuite106 toks (term :> stk)
  STestSuite96 -> __runTestSuite STestSuite107 toks (term :> stk)
  _ -> error ""

__gotoTupleForTestSuite :: ([Lexeme], Pos) -> [Expr] -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoTupleForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite94 -> __runTestSuite STestSuite97 toks (term :> stk)
  _ -> error ""

__runTestSuite :: StTestSuite a -> ([Lexeme], Pos) -> Stack' StTestSuite a -> Either (Pos, [String]) TestSuite
__runTestSuite = \cases {
; STestSuite0 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite3 (__input, __end) (() :> (STestSuite0, __p, __stk))
; STestSuite0 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite37 (__input, __end) (n :> (STestSuite0, __p, __stk))
; STestSuite0 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite45 (__input, __end) (n :> (STestSuite0, __p, __stk))
; STestSuite0 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite49 (__input, __end) (n :> (STestSuite0, __p, __stk))
; STestSuite1 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite3 (__input, __end) (() :> (STestSuite1, __p, __stk))
; STestSuite1 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite98 (__input, __end) (() :> (STestSuite1, __p, __stk))
; STestSuite1 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite37 (__input, __end) (n :> (STestSuite1, __p, __stk))
; STestSuite1 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite45 (__input, __end) (n :> (STestSuite1, __p, __stk))
; STestSuite1 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite49 (__input, __end) (n :> (STestSuite1, __p, __stk))
; STestSuite2 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite5 (__input, __end) (() :> (STestSuite2, __p, __stk))
; STestSuite2 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite38 (__input, __end) (n :> (STestSuite2, __p, __stk))
; STestSuite2 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite46 (__input, __end) (n :> (STestSuite2, __p, __stk))
; STestSuite2 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite50 (__input, __end) (n :> (STestSuite2, __p, __stk))
; STestSuite3 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite5 (__input, __end) (() :> (STestSuite3, __p, __stk))
; STestSuite3 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite38 (__input, __end) (n :> (STestSuite3, __p, __stk))
; STestSuite3 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite46 (__input, __end) (n :> (STestSuite3, __p, __stk))
; STestSuite3 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite50 (__input, __end) (n :> (STestSuite3, __p, __stk))
; STestSuite4 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite5 (__input, __end) (() :> (STestSuite4, __p, __stk))
; STestSuite4 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite38 (__input, __end) (n :> (STestSuite4, __p, __stk))
; STestSuite4 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite46 (__input, __end) (n :> (STestSuite4, __p, __stk))
; STestSuite4 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite50 (__input, __end) (n :> (STestSuite4, __p, __stk))
; STestSuite5 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite5 (__input, __end) (() :> (STestSuite5, __p, __stk))
; STestSuite5 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite38 (__input, __end) (n :> (STestSuite5, __p, __stk))
; STestSuite5 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite46 (__input, __end) (n :> (STestSuite5, __p, __stk))
; STestSuite5 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite50 (__input, __end) (n :> (STestSuite5, __p, __stk))
; STestSuite6 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite5 (__input, __end) (() :> (STestSuite6, __p, __stk))
; STestSuite6 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite38 (__input, __end) (n :> (STestSuite6, __p, __stk))
; STestSuite6 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite46 (__input, __end) (n :> (STestSuite6, __p, __stk))
; STestSuite6 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite50 (__input, __end) (n :> (STestSuite6, __p, __stk))
; STestSuite7 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite5 (__input, __end) (() :> (STestSuite7, __p, __stk))
; STestSuite7 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite38 (__input, __end) (n :> (STestSuite7, __p, __stk))
; STestSuite7 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite46 (__input, __end) (n :> (STestSuite7, __p, __stk))
; STestSuite7 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite50 (__input, __end) (n :> (STestSuite7, __p, __stk))
; STestSuite8 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite4 (__input, __end) (() :> (STestSuite8, __p, __stk))
; STestSuite8 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite35 (__input, __end) (n :> (STestSuite8, __p, __stk))
; STestSuite8 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite43 (__input, __end) (n :> (STestSuite8, __p, __stk))
; STestSuite8 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite47 (__input, __end) (n :> (STestSuite8, __p, __stk))
; STestSuite9 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite2 (__input, __end) (() :> (STestSuite9, __p, __stk))
; STestSuite9 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite36 (__input, __end) (n :> (STestSuite9, __p, __stk))
; STestSuite9 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite44 (__input, __end) (n :> (STestSuite9, __p, __stk))
; STestSuite9 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite48 (__input, __end) (n :> (STestSuite9, __p, __stk))
; STestSuite10 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite4 (__input, __end) (() :> (STestSuite10, __p, __stk))
; STestSuite10 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite35 (__input, __end) (n :> (STestSuite10, __p, __stk))
; STestSuite10 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite43 (__input, __end) (n :> (STestSuite10, __p, __stk))
; STestSuite10 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite47 (__input, __end) (n :> (STestSuite10, __p, __stk))
; STestSuite11 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite2 (__input, __end) (() :> (STestSuite11, __p, __stk))
; STestSuite11 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite36 (__input, __end) (n :> (STestSuite11, __p, __stk))
; STestSuite11 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite44 (__input, __end) (n :> (STestSuite11, __p, __stk))
; STestSuite11 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite48 (__input, __end) (n :> (STestSuite11, __p, __stk))
; STestSuite12 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite3 (__input, __end) (() :> (STestSuite12, __p, __stk))
; STestSuite12 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite37 (__input, __end) (n :> (STestSuite12, __p, __stk))
; STestSuite12 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite45 (__input, __end) (n :> (STestSuite12, __p, __stk))
; STestSuite12 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite49 (__input, __end) (n :> (STestSuite12, __p, __stk))
; STestSuite13 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite5 (__input, __end) (() :> (STestSuite13, __p, __stk))
; STestSuite13 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite38 (__input, __end) (n :> (STestSuite13, __p, __stk))
; STestSuite13 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite46 (__input, __end) (n :> (STestSuite13, __p, __stk))
; STestSuite13 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite50 (__input, __end) (n :> (STestSuite13, __p, __stk))
; STestSuite14 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite4 (__input, __end) (() :> (STestSuite14, __p, __stk))
; STestSuite14 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite35 (__input, __end) (n :> (STestSuite14, __p, __stk))
; STestSuite14 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite43 (__input, __end) (n :> (STestSuite14, __p, __stk))
; STestSuite14 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite47 (__input, __end) (n :> (STestSuite14, __p, __stk))
; STestSuite15 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite2 (__input, __end) (() :> (STestSuite15, __p, __stk))
; STestSuite15 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite36 (__input, __end) (n :> (STestSuite15, __p, __stk))
; STestSuite15 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite44 (__input, __end) (n :> (STestSuite15, __p, __stk))
; STestSuite15 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite48 (__input, __end) (n :> (STestSuite15, __p, __stk))
; STestSuite16 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite3 (__input, __end) (() :> (STestSuite16, __p, __stk))
; STestSuite16 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite37 (__input, __end) (n :> (STestSuite16, __p, __stk))
; STestSuite16 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite45 (__input, __end) (n :> (STestSuite16, __p, __stk))
; STestSuite16 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite49 (__input, __end) (n :> (STestSuite16, __p, __stk))
; STestSuite17 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite5 (__input, __end) (() :> (STestSuite17, __p, __stk))
; STestSuite17 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite38 (__input, __end) (n :> (STestSuite17, __p, __stk))
; STestSuite17 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite46 (__input, __end) (n :> (STestSuite17, __p, __stk))
; STestSuite17 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite50 (__input, __end) (n :> (STestSuite17, __p, __stk))
; STestSuite18 ((__p,  ",") : __input, __end) __stk ->
    __runTestSuite STestSuite0 (__input, __end) (() :> (STestSuite18, __p, __stk))
; STestSuite19 ((__p,  "+") : __input, __end) __stk ->
    __runTestSuite STestSuite12 (__input, __end) (() :> (STestSuite19, __p, __stk))
; STestSuite19 ((__p,  "=") : __input, __end) __stk ->
    __runTestSuite STestSuite9 (__input, __end) (() :> (STestSuite19, __p, __stk))
; STestSuite20 ((__p,  "+") : __input, __end) __stk ->
    __runTestSuite STestSuite13 (__input, __end) (() :> (STestSuite20, __p, __stk))
; STestSuite20 ((__p,  "=") : __input, __end) __stk ->
    __runTestSuite STestSuite8 (__input, __end) (() :> (STestSuite20, __p, __stk))
; STestSuite21 ((__p,  "+") : __input, __end) __stk ->
    __runTestSuite STestSuite10 (__input, __end) (() :> (STestSuite21, __p, __stk))
; STestSuite22 ((__p,  "+") : __input, __end) __stk ->
    __runTestSuite STestSuite11 (__input, __end) (() :> (STestSuite22, __p, __stk))
; STestSuite23 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite14 (__input, __end) (() :> (STestSuite23, __p, __stk))
; STestSuite24 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite15 (__input, __end) (() :> (STestSuite24, __p, __stk))
; STestSuite25 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite16 (__input, __end) (() :> (STestSuite25, __p, __stk))
; STestSuite26 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite17 (__input, __end) (() :> (STestSuite26, __p, __stk))
; STestSuite27 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite14 (__input, __end) (() :> (STestSuite27, __p, __stk))
; STestSuite28 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite15 (__input, __end) (() :> (STestSuite28, __p, __stk))
; STestSuite29 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite16 (__input, __end) (() :> (STestSuite29, __p, __stk))
; STestSuite30 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite17 (__input, __end) (() :> (STestSuite30, __p, __stk))
; STestSuite51 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite60 (__input, __end) (() :> (STestSuite51, __p, __stk))
; STestSuite52 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite61 (__input, __end) (() :> (STestSuite52, __p, __stk))
; STestSuite53 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite62 (__input, __end) (() :> (STestSuite53, __p, __stk))
; STestSuite54 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite63 (__input, __end) (() :> (STestSuite54, __p, __stk))
; STestSuite64 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite6 (__input, __end) (() :> (STestSuite64, __p, __stk))
; STestSuite64 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite76 (__input, __end) (n :> (STestSuite64, __p, __stk))
; STestSuite64 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite80 (__input, __end) (n :> (STestSuite64, __p, __stk))
; STestSuite64 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite82 (__input, __end) (n :> (STestSuite64, __p, __stk))
; STestSuite65 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite7 (__input, __end) (() :> (STestSuite65, __p, __stk))
; STestSuite65 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite77 (__input, __end) (n :> (STestSuite65, __p, __stk))
; STestSuite65 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite81 (__input, __end) (n :> (STestSuite65, __p, __stk))
; STestSuite65 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite83 (__input, __end) (n :> (STestSuite65, __p, __stk))
; STestSuite66 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite6 (__input, __end) (() :> (STestSuite66, __p, __stk))
; STestSuite66 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite76 (__input, __end) (n :> (STestSuite66, __p, __stk))
; STestSuite66 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite80 (__input, __end) (n :> (STestSuite66, __p, __stk))
; STestSuite66 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite82 (__input, __end) (n :> (STestSuite66, __p, __stk))
; STestSuite67 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite7 (__input, __end) (() :> (STestSuite67, __p, __stk))
; STestSuite67 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite77 (__input, __end) (n :> (STestSuite67, __p, __stk))
; STestSuite67 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite81 (__input, __end) (n :> (STestSuite67, __p, __stk))
; STestSuite67 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite83 (__input, __end) (n :> (STestSuite67, __p, __stk))
; STestSuite68 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite6 (__input, __end) (() :> (STestSuite68, __p, __stk))
; STestSuite68 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite76 (__input, __end) (n :> (STestSuite68, __p, __stk))
; STestSuite68 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite80 (__input, __end) (n :> (STestSuite68, __p, __stk))
; STestSuite68 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite82 (__input, __end) (n :> (STestSuite68, __p, __stk))
; STestSuite69 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite7 (__input, __end) (() :> (STestSuite69, __p, __stk))
; STestSuite69 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite77 (__input, __end) (n :> (STestSuite69, __p, __stk))
; STestSuite69 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite81 (__input, __end) (n :> (STestSuite69, __p, __stk))
; STestSuite69 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite83 (__input, __end) (n :> (STestSuite69, __p, __stk))
; STestSuite70 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite68 (__input, __end) (() :> (STestSuite70, __p, __stk))
; STestSuite71 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite69 (__input, __end) (() :> (STestSuite71, __p, __stk))
; STestSuite72 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite68 (__input, __end) (() :> (STestSuite72, __p, __stk))
; STestSuite73 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite69 (__input, __end) (() :> (STestSuite73, __p, __stk))
; STestSuite84 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite88 (__input, __end) (() :> (STestSuite84, __p, __stk))
; STestSuite85 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite89 (__input, __end) (() :> (STestSuite85, __p, __stk))
; STestSuite90 ((__p,  "+") : __input, __end) __stk ->
    __runTestSuite STestSuite66 (__input, __end) (() :> (STestSuite90, __p, __stk))
; STestSuite90 ((__p,  "=") : __input, __end) __stk ->
    __runTestSuite STestSuite65 (__input, __end) (() :> (STestSuite90, __p, __stk))
; STestSuite91 ((__p,  "+") : __input, __end) __stk ->
    __runTestSuite STestSuite67 (__input, __end) (() :> (STestSuite91, __p, __stk))
; STestSuite92 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite94 (__input, __end) (n :> (STestSuite92, __p, __stk))
; STestSuite93 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite94 (__input, __end) (n :> (STestSuite93, __p, __stk))
; STestSuite94 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite1 (__input, __end) (() :> (STestSuite94, __p, __stk))
; STestSuite95 ((__p,  "expect") : __input, __end) __stk ->
    __runTestSuite STestSuite92 (__input, __end) (() :> (STestSuite95, __p, __stk))
; STestSuite95 ((__p,  "guard") : __input, __end) __stk ->
    __runTestSuite STestSuite64 (__input, __end) (() :> (STestSuite95, __p, __stk))
; STestSuite95 ((__p,  "notify") : __input, __end) __stk ->
    __runTestSuite STestSuite93 (__input, __end) (() :> (STestSuite95, __p, __stk))
; STestSuite96 ((__p,  "expect") : __input, __end) __stk ->
    __runTestSuite STestSuite92 (__input, __end) (() :> (STestSuite96, __p, __stk))
; STestSuite96 ((__p,  "guard") : __input, __end) __stk ->
    __runTestSuite STestSuite64 (__input, __end) (() :> (STestSuite96, __p, __stk))
; STestSuite96 ((__p,  "notify") : __input, __end) __stk ->
    __runTestSuite STestSuite93 (__input, __end) (() :> (STestSuite96, __p, __stk))
; STestSuite99 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite103 (__input, __end) (() :> (STestSuite99, __p, __stk))
; STestSuite104 ((__p,  "test") : __input, __end) __stk ->
    __runTestSuite STestSuite95 (__input, __end) (() :> (STestSuite104, __p, __stk))
-- lookahead ), entity Exprs1
; STestSuite18 ((__p,  ")") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoExprs1ForTestSuite ((__p,  ")") : __input, __end) (action55 __pos e) __stk
-- lookahead ), entity Expr
; STestSuite19 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForTestSuite ((__p,  ")") : __input, __end) (action59 __pos a) __stk
-- lookahead ,, entity Expr
; STestSuite19 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForTestSuite ((__p,  ",") : __input, __end) (action59 __pos a) __stk
-- lookahead ), entity Expr
; STestSuite20 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForTestSuite ((__p,  ")") : __input, __end) (action59 __pos a) __stk
-- lookahead ), entity Expr
; STestSuite21 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForTestSuite ((__p,  ")") : __input, __end) (action58 __pos a
                                                                          b) __stk
-- lookahead ), entity Expr
; STestSuite22 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForTestSuite ((__p,  ")") : __input, __end) (action58 __pos a
                                                                          b) __stk
-- lookahead ,, entity Expr
; STestSuite22 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForTestSuite ((__p,  ",") : __input, __end) (action58 __pos a
                                                                          b) __stk
-- lookahead ), entity Add
; STestSuite23 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; STestSuite23 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead ), entity Add
; STestSuite24 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; STestSuite24 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead ,, entity Add
; STestSuite24 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead ), entity Add
; STestSuite25 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; STestSuite25 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead ,, entity Add
; STestSuite25 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead =, entity Add
; STestSuite25 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "=") : __input, __end) (action63 __pos a) __stk
-- lookahead ), entity Add
; STestSuite26 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; STestSuite26 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead =, entity Add
; STestSuite26 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "=") : __input, __end) (action63 __pos a) __stk
-- lookahead ), entity Add
; STestSuite27 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead +, entity Add
; STestSuite27 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead ), entity Add
; STestSuite28 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead +, entity Add
; STestSuite28 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead ,, entity Add
; STestSuite28 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  ",") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead ), entity Add
; STestSuite29 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead +, entity Add
; STestSuite29 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead ,, entity Add
; STestSuite29 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  ",") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead =, entity Add
; STestSuite29 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "=") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead ), entity Add
; STestSuite30 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead +, entity Add
; STestSuite30 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead =, entity Add
; STestSuite30 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "=") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead ), entity Mult
; STestSuite31 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; STestSuite31 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; STestSuite31 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead ), entity Mult
; STestSuite32 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; STestSuite32 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; STestSuite32 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead ,, entity Mult
; STestSuite32 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead ), entity Mult
; STestSuite33 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; STestSuite33 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; STestSuite33 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead ,, entity Mult
; STestSuite33 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead =, entity Mult
; STestSuite33 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "=") : __input, __end) (action67 __pos a) __stk
-- lookahead ), entity Mult
; STestSuite34 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; STestSuite34 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; STestSuite34 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead =, entity Mult
; STestSuite34 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "=") : __input, __end) (action67 __pos a) __stk
-- lookahead ), entity Term
; STestSuite35 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; STestSuite35 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; STestSuite35 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead ), entity Term
; STestSuite36 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; STestSuite36 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; STestSuite36 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead ,, entity Term
; STestSuite36 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead ), entity Term
; STestSuite37 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; STestSuite37 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; STestSuite37 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead ,, entity Term
; STestSuite37 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead =, entity Term
; STestSuite37 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action71 __pos n) __stk
-- lookahead ), entity Term
; STestSuite38 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; STestSuite38 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; STestSuite38 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead =, entity Term
; STestSuite38 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action71 __pos n) __stk
-- lookahead ), entity Term
; STestSuite39 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; STestSuite39 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; STestSuite39 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead ), entity Term
; STestSuite40 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; STestSuite40 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; STestSuite40 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead ,, entity Term
; STestSuite40 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead ), entity Term
; STestSuite41 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; STestSuite41 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; STestSuite41 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead ,, entity Term
; STestSuite41 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead =, entity Term
; STestSuite41 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action72 __pos n) __stk
-- lookahead ), entity Term
; STestSuite42 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; STestSuite42 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; STestSuite42 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead =, entity Term
; STestSuite42 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action72 __pos n) __stk
-- lookahead ), entity Const
; STestSuite43 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; STestSuite43 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; STestSuite43 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ), entity Const
; STestSuite44 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; STestSuite44 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; STestSuite44 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ,, entity Const
; STestSuite44 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead ), entity Const
; STestSuite45 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; STestSuite45 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; STestSuite45 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead ,, entity Const
; STestSuite45 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead =, entity Const
; STestSuite45 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead ), entity Const
; STestSuite46 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; STestSuite46 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; STestSuite46 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead =, entity Const
; STestSuite46 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead ), entity Const
; STestSuite47 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; STestSuite47 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; STestSuite47 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead ), entity Const
; STestSuite48 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; STestSuite48 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; STestSuite48 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead ,, entity Const
; STestSuite48 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead ), entity Const
; STestSuite49 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; STestSuite49 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; STestSuite49 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead ,, entity Const
; STestSuite49 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead =, entity Const
; STestSuite49 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "=") : __input, __end) (action76 __pos n) __stk
-- lookahead ), entity Const
; STestSuite50 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; STestSuite50 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; STestSuite50 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead =, entity Const
; STestSuite50 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "=") : __input, __end) (action76 __pos n) __stk
-- lookahead ), entity Exprs1
; STestSuite55 ((__p,  ")") : __input, __end) ((es :> (_, _, _ :> (_, _, e :> __stk@(_, __pos, _))))) ->
    __gotoExprs1ForTestSuite ((__p,  ")") : __input, __end) (action54 __pos e
                                                                            es) __stk
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
-- lookahead ), entity Mult
; STestSuite57 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead *, entity Mult
; STestSuite57 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead +, entity Mult
; STestSuite57 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead ,, entity Mult
; STestSuite57 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  ",") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead ), entity Mult
; STestSuite58 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead *, entity Mult
; STestSuite58 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead +, entity Mult
; STestSuite58 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead ,, entity Mult
; STestSuite58 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  ",") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead =, entity Mult
; STestSuite58 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "=") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead ), entity Mult
; STestSuite59 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead *, entity Mult
; STestSuite59 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead +, entity Mult
; STestSuite59 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead =, entity Mult
; STestSuite59 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "=") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead ), entity Term
; STestSuite60 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; STestSuite60 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; STestSuite60 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead ), entity Term
; STestSuite61 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; STestSuite61 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; STestSuite61 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead ,, entity Term
; STestSuite61 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead ), entity Term
; STestSuite62 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; STestSuite62 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; STestSuite62 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead ,, entity Term
; STestSuite62 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead =, entity Term
; STestSuite62 ((__p,  "=") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action70 __pos e) __stk
-- lookahead ), entity Term
; STestSuite63 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; STestSuite63 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; STestSuite63 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead =, entity Term
; STestSuite63 ((__p,  "=") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Add
; STestSuite70 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead =, entity Add
; STestSuite70 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "=") : __input, __end) (action63 __pos a) __stk
-- lookahead expect, entity Add
; STestSuite70 ((__p,  "expect") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "expect") : __input, __end) (action63 __pos a) __stk
-- lookahead guard, entity Add
; STestSuite70 ((__p,  "guard") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "guard") : __input, __end) (action63 __pos a) __stk
-- lookahead notify, entity Add
; STestSuite70 ((__p,  "notify") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "notify") : __input, __end) (action63 __pos a) __stk
-- lookahead <eof>, entity Add
; STestSuite70 ([], __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ([], __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; STestSuite71 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead expect, entity Add
; STestSuite71 ((__p,  "expect") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "expect") : __input, __end) (action63 __pos a) __stk
-- lookahead guard, entity Add
; STestSuite71 ((__p,  "guard") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "guard") : __input, __end) (action63 __pos a) __stk
-- lookahead notify, entity Add
; STestSuite71 ((__p,  "notify") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "notify") : __input, __end) (action63 __pos a) __stk
-- lookahead <eof>, entity Add
; STestSuite71 ([], __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ([], __end) (action63 __pos a) __stk
-- lookahead +, entity Add
; STestSuite72 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead =, entity Add
; STestSuite72 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "=") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead expect, entity Add
; STestSuite72 ((__p,  "expect") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "expect") : __input, __end) (action62 __pos a
                                                                              b) __stk
-- lookahead guard, entity Add
; STestSuite72 ((__p,  "guard") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "guard") : __input, __end) (action62 __pos a
                                                                             b) __stk
-- lookahead notify, entity Add
; STestSuite72 ((__p,  "notify") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "notify") : __input, __end) (action62 __pos a
                                                                              b) __stk
-- lookahead <eof>, entity Add
; STestSuite72 ([], __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ([], __end) (action62 __pos a b) __stk
-- lookahead +, entity Add
; STestSuite73 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead expect, entity Add
; STestSuite73 ((__p,  "expect") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "expect") : __input, __end) (action62 __pos a
                                                                              b) __stk
-- lookahead guard, entity Add
; STestSuite73 ((__p,  "guard") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "guard") : __input, __end) (action62 __pos a
                                                                             b) __stk
-- lookahead notify, entity Add
; STestSuite73 ((__p,  "notify") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "notify") : __input, __end) (action62 __pos a
                                                                              b) __stk
-- lookahead <eof>, entity Add
; STestSuite73 ([], __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ([], __end) (action62 __pos a b) __stk
-- lookahead *, entity Mult
; STestSuite74 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; STestSuite74 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead =, entity Mult
; STestSuite74 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "=") : __input, __end) (action67 __pos a) __stk
-- lookahead expect, entity Mult
; STestSuite74 ((__p,  "expect") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "expect") : __input, __end) (action67 __pos a) __stk
-- lookahead guard, entity Mult
; STestSuite74 ((__p,  "guard") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "guard") : __input, __end) (action67 __pos a) __stk
-- lookahead notify, entity Mult
; STestSuite74 ((__p,  "notify") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "notify") : __input, __end) (action67 __pos a) __stk
-- lookahead <eof>, entity Mult
; STestSuite74 ([], __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ([], __end) (action67 __pos a) __stk
-- lookahead *, entity Mult
; STestSuite75 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead +, entity Mult
; STestSuite75 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead expect, entity Mult
; STestSuite75 ((__p,  "expect") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "expect") : __input, __end) (action67 __pos a) __stk
-- lookahead guard, entity Mult
; STestSuite75 ((__p,  "guard") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "guard") : __input, __end) (action67 __pos a) __stk
-- lookahead notify, entity Mult
; STestSuite75 ((__p,  "notify") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "notify") : __input, __end) (action67 __pos a) __stk
-- lookahead <eof>, entity Mult
; STestSuite75 ([], __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ([], __end) (action67 __pos a) __stk
-- lookahead *, entity Term
; STestSuite76 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; STestSuite76 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead =, entity Term
; STestSuite76 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action71 __pos n) __stk
-- lookahead expect, entity Term
; STestSuite76 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "expect") : __input, __end) (action71 __pos n) __stk
-- lookahead guard, entity Term
; STestSuite76 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "guard") : __input, __end) (action71 __pos n) __stk
-- lookahead notify, entity Term
; STestSuite76 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "notify") : __input, __end) (action71 __pos n) __stk
-- lookahead <eof>, entity Term
; STestSuite76 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ([], __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; STestSuite77 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead +, entity Term
; STestSuite77 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead expect, entity Term
; STestSuite77 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "expect") : __input, __end) (action71 __pos n) __stk
-- lookahead guard, entity Term
; STestSuite77 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "guard") : __input, __end) (action71 __pos n) __stk
-- lookahead notify, entity Term
; STestSuite77 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "notify") : __input, __end) (action71 __pos n) __stk
-- lookahead <eof>, entity Term
; STestSuite77 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ([], __end) (action71 __pos n) __stk
-- lookahead *, entity Term
; STestSuite78 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; STestSuite78 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead =, entity Term
; STestSuite78 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action72 __pos n) __stk
-- lookahead expect, entity Term
; STestSuite78 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "expect") : __input, __end) (action72 __pos n) __stk
-- lookahead guard, entity Term
; STestSuite78 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "guard") : __input, __end) (action72 __pos n) __stk
-- lookahead notify, entity Term
; STestSuite78 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "notify") : __input, __end) (action72 __pos n) __stk
-- lookahead <eof>, entity Term
; STestSuite78 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ([], __end) (action72 __pos n) __stk
-- lookahead *, entity Term
; STestSuite79 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead +, entity Term
; STestSuite79 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead expect, entity Term
; STestSuite79 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "expect") : __input, __end) (action72 __pos n) __stk
-- lookahead guard, entity Term
; STestSuite79 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "guard") : __input, __end) (action72 __pos n) __stk
-- lookahead notify, entity Term
; STestSuite79 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "notify") : __input, __end) (action72 __pos n) __stk
-- lookahead <eof>, entity Term
; STestSuite79 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ([], __end) (action72 __pos n) __stk
-- lookahead *, entity Const
; STestSuite80 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; STestSuite80 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead =, entity Const
; STestSuite80 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead expect, entity Const
; STestSuite80 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "expect") : __input, __end) (action75 __pos n) __stk
-- lookahead guard, entity Const
; STestSuite80 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "guard") : __input, __end) (action75 __pos n) __stk
-- lookahead notify, entity Const
; STestSuite80 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "notify") : __input, __end) (action75 __pos n) __stk
-- lookahead <eof>, entity Const
; STestSuite80 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ([], __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; STestSuite81 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead +, entity Const
; STestSuite81 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead expect, entity Const
; STestSuite81 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "expect") : __input, __end) (action75 __pos n) __stk
-- lookahead guard, entity Const
; STestSuite81 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "guard") : __input, __end) (action75 __pos n) __stk
-- lookahead notify, entity Const
; STestSuite81 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "notify") : __input, __end) (action75 __pos n) __stk
-- lookahead <eof>, entity Const
; STestSuite81 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ([], __end) (action75 __pos n) __stk
-- lookahead *, entity Const
; STestSuite82 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; STestSuite82 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead =, entity Const
; STestSuite82 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "=") : __input, __end) (action76 __pos n) __stk
-- lookahead expect, entity Const
; STestSuite82 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "expect") : __input, __end) (action76 __pos n) __stk
-- lookahead guard, entity Const
; STestSuite82 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "guard") : __input, __end) (action76 __pos n) __stk
-- lookahead notify, entity Const
; STestSuite82 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "notify") : __input, __end) (action76 __pos n) __stk
-- lookahead <eof>, entity Const
; STestSuite82 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ([], __end) (action76 __pos n) __stk
-- lookahead *, entity Const
; STestSuite83 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead +, entity Const
; STestSuite83 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead expect, entity Const
; STestSuite83 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "expect") : __input, __end) (action76 __pos n) __stk
-- lookahead guard, entity Const
; STestSuite83 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "guard") : __input, __end) (action76 __pos n) __stk
-- lookahead notify, entity Const
; STestSuite83 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "notify") : __input, __end) (action76 __pos n) __stk
-- lookahead <eof>, entity Const
; STestSuite83 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ([], __end) (action76 __pos n) __stk
-- lookahead *, entity Mult
; STestSuite86 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead +, entity Mult
; STestSuite86 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead =, entity Mult
; STestSuite86 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "=") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead expect, entity Mult
; STestSuite86 ((__p,  "expect") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "expect") : __input, __end) (action66 __pos a
                                                                               b) __stk
-- lookahead guard, entity Mult
; STestSuite86 ((__p,  "guard") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "guard") : __input, __end) (action66 __pos a
                                                                              b) __stk
-- lookahead notify, entity Mult
; STestSuite86 ((__p,  "notify") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "notify") : __input, __end) (action66 __pos a
                                                                               b) __stk
-- lookahead <eof>, entity Mult
; STestSuite86 ([], __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ([], __end) (action66 __pos a b) __stk
-- lookahead *, entity Mult
; STestSuite87 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead +, entity Mult
; STestSuite87 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead expect, entity Mult
; STestSuite87 ((__p,  "expect") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "expect") : __input, __end) (action66 __pos a
                                                                               b) __stk
-- lookahead guard, entity Mult
; STestSuite87 ((__p,  "guard") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "guard") : __input, __end) (action66 __pos a
                                                                              b) __stk
-- lookahead notify, entity Mult
; STestSuite87 ((__p,  "notify") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "notify") : __input, __end) (action66 __pos a
                                                                               b) __stk
-- lookahead <eof>, entity Mult
; STestSuite87 ([], __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ([], __end) (action66 __pos a b) __stk
-- lookahead *, entity Term
; STestSuite88 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; STestSuite88 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead =, entity Term
; STestSuite88 ((__p,  "=") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action70 __pos e) __stk
-- lookahead expect, entity Term
; STestSuite88 ((__p,  "expect") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "expect") : __input, __end) (action70 __pos e) __stk
-- lookahead guard, entity Term
; STestSuite88 ((__p,  "guard") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "guard") : __input, __end) (action70 __pos e) __stk
-- lookahead notify, entity Term
; STestSuite88 ((__p,  "notify") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "notify") : __input, __end) (action70 __pos e) __stk
-- lookahead <eof>, entity Term
; STestSuite88 ([], __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ([], __end) (action70 __pos e) __stk
-- lookahead *, entity Term
; STestSuite89 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead +, entity Term
; STestSuite89 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead expect, entity Term
; STestSuite89 ((__p,  "expect") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "expect") : __input, __end) (action70 __pos e) __stk
-- lookahead guard, entity Term
; STestSuite89 ((__p,  "guard") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "guard") : __input, __end) (action70 __pos e) __stk
-- lookahead notify, entity Term
; STestSuite89 ((__p,  "notify") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "notify") : __input, __end) (action70 __pos e) __stk
-- lookahead <eof>, entity Term
; STestSuite89 ([], __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ([], __end) (action70 __pos e) __stk
-- lookahead expect, entity Expr
; STestSuite90 ((__p,  "expect") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForTestSuite ((__p,  "expect") : __input, __end) (action59 __pos a) __stk
-- lookahead guard, entity Expr
; STestSuite90 ((__p,  "guard") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForTestSuite ((__p,  "guard") : __input, __end) (action59 __pos a) __stk
-- lookahead notify, entity Expr
; STestSuite90 ((__p,  "notify") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForTestSuite ((__p,  "notify") : __input, __end) (action59 __pos a) __stk
-- lookahead <eof>, entity Expr
; STestSuite90 ([], __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForTestSuite ([], __end) (action59 __pos a) __stk
-- lookahead expect, entity Expr
; STestSuite91 ((__p,  "expect") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForTestSuite ((__p,  "expect") : __input, __end) (action58 __pos a
                                                                               b) __stk
-- lookahead guard, entity Expr
; STestSuite91 ((__p,  "guard") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForTestSuite ((__p,  "guard") : __input, __end) (action58 __pos a
                                                                              b) __stk
-- lookahead notify, entity Expr
; STestSuite91 ((__p,  "notify") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForTestSuite ((__p,  "notify") : __input, __end) (action58 __pos a
                                                                               b) __stk
-- lookahead <eof>, entity Expr
; STestSuite91 ([], __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForTestSuite ([], __end) (action58 __pos a b) __stk
-- lookahead <eof>, entity Tests
; STestSuite96 ([], __end) ((t :> __stk@(_, __pos, _))) ->
    __gotoTestsForTestSuite ([], __end) (action87 __pos t) __stk
-- lookahead expect, entity Call
; STestSuite97 ((__p,  "expect") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForTestSuite ((__p,  "expect") : __input, __end) (action47 __pos pre
                                                                               t) __stk
-- lookahead guard, entity Call
; STestSuite97 ((__p,  "guard") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForTestSuite ((__p,  "guard") : __input, __end) (action47 __pos pre
                                                                              t) __stk
-- lookahead notify, entity Call
; STestSuite97 ((__p,  "notify") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForTestSuite ((__p,  "notify") : __input, __end) (action47 __pos pre
                                                                               t) __stk
-- lookahead <eof>, entity Call
; STestSuite97 ([], __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForTestSuite ([], __end) (action47 __pos pre t) __stk
-- lookahead expect, entity Tuple
; STestSuite98 ((__p,  "expect") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForTestSuite ((__p,  "expect") : __input, __end) (action50 __pos ) __stk
-- lookahead guard, entity Tuple
; STestSuite98 ((__p,  "guard") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForTestSuite ((__p,  "guard") : __input, __end) (action50 __pos ) __stk
-- lookahead notify, entity Tuple
; STestSuite98 ((__p,  "notify") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForTestSuite ((__p,  "notify") : __input, __end) (action50 __pos ) __stk
-- lookahead <eof>, entity Tuple
; STestSuite98 ([], __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForTestSuite ([], __end) (action50 __pos ) __stk
-- lookahead expect, entity Test
; STestSuite100 ((__p,  "expect") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "expect") : __input, __end) (action79 __pos c) __stk
-- lookahead guard, entity Test
; STestSuite100 ((__p,  "guard") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "guard") : __input, __end) (action79 __pos c) __stk
-- lookahead notify, entity Test
; STestSuite100 ((__p,  "notify") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "notify") : __input, __end) (action79 __pos c) __stk
-- lookahead <eof>, entity Test
; STestSuite100 ([], __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ([], __end) (action79 __pos c) __stk
-- lookahead expect, entity Test
; STestSuite101 ((__p,  "expect") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "expect") : __input, __end) (action80 __pos c) __stk
-- lookahead guard, entity Test
; STestSuite101 ((__p,  "guard") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "guard") : __input, __end) (action80 __pos c) __stk
-- lookahead notify, entity Test
; STestSuite101 ((__p,  "notify") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "notify") : __input, __end) (action80 __pos c) __stk
-- lookahead <eof>, entity Test
; STestSuite101 ([], __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ([], __end) (action80 __pos c) __stk
-- lookahead expect, entity Test
; STestSuite102 ((__p,  "expect") : __input, __end) ((e :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "expect") : __input, __end) (action81 __pos e) __stk
-- lookahead guard, entity Test
; STestSuite102 ((__p,  "guard") : __input, __end) ((e :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "guard") : __input, __end) (action81 __pos e) __stk
-- lookahead notify, entity Test
; STestSuite102 ((__p,  "notify") : __input, __end) ((e :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "notify") : __input, __end) (action81 __pos e) __stk
-- lookahead <eof>, entity Test
; STestSuite102 ([], __end) ((e :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ([], __end) (action81 __pos e) __stk
-- lookahead expect, entity Tuple
; STestSuite103 ((__p,  "expect") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForTestSuite ((__p,  "expect") : __input, __end) (action51 __pos es) __stk
-- lookahead guard, entity Tuple
; STestSuite103 ((__p,  "guard") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForTestSuite ((__p,  "guard") : __input, __end) (action51 __pos es) __stk
-- lookahead notify, entity Tuple
; STestSuite103 ((__p,  "notify") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForTestSuite ((__p,  "notify") : __input, __end) (action51 __pos es) __stk
-- lookahead <eof>, entity Tuple
; STestSuite103 ([], __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForTestSuite ([], __end) (action51 __pos es) __stk
-- lookahead <eof>, entity TestSuite
; STestSuite105 ([], __end) ((res :> __stk@(_, __pos, _))) ->
    pure res
-- lookahead <eof>, entity TestSuite
; STestSuite106 ([], __end) ((tests :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestSuiteForTestSuite ([], __end) (action84 __pos tests) __stk
-- lookahead <eof>, entity Tests
; STestSuite107 ([], __end) ((ts :> (_, _, t :> __stk@(_, __pos, _)))) ->
    __gotoTestsForTestSuite ([], __end) (action88 __pos t ts) __stk
; STestSuite0 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite1 __input _ ->
    Left  (currentPos __input, ["(", ")", "<Name>", "<name>", "<num>"])
; STestSuite2 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite3 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite4 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite5 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite6 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite7 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite8 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite9 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite10 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite11 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite12 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite13 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite14 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite15 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite16 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite17 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite18 __input _ -> Left  (currentPos __input, [")", ","])
; STestSuite19 __input _ ->
    Left  (currentPos __input, [")", "+", ",", "="])
; STestSuite20 __input _ ->
    Left  (currentPos __input, [")", "+", "="])
; STestSuite21 __input _ -> Left  (currentPos __input, [")", "+"])
; STestSuite22 __input _ ->
    Left  (currentPos __input, [")", "+", ","])
; STestSuite23 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite24 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite25 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite26 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite27 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite28 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite29 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite30 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite31 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite32 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite33 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite34 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite35 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite36 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite37 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite38 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite39 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite40 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite41 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite42 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite43 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite44 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite45 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite46 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite47 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite48 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite49 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite50 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite51 __input _ -> Left  (currentPos __input, [")"])
; STestSuite52 __input _ -> Left  (currentPos __input, [")"])
; STestSuite53 __input _ -> Left  (currentPos __input, [")"])
; STestSuite54 __input _ -> Left  (currentPos __input, [")"])
; STestSuite55 __input _ -> Left  (currentPos __input, [")"])
; STestSuite56 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite57 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite58 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite59 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite60 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite61 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite62 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite63 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite64 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite65 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite66 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite67 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite68 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite69 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite70 __input _ ->
    Left  (currentPos __input, ["*", "+", "=", "expect", "guard",
                                "notify", "<eof>"])
; STestSuite71 __input _ ->
    Left  (currentPos __input, ["*", "+", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite72 __input _ ->
    Left  (currentPos __input, ["*", "+", "=", "expect", "guard",
                                "notify", "<eof>"])
; STestSuite73 __input _ ->
    Left  (currentPos __input, ["*", "+", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite74 __input _ ->
    Left  (currentPos __input, ["*", "+", "=", "expect", "guard",
                                "notify", "<eof>"])
; STestSuite75 __input _ ->
    Left  (currentPos __input, ["*", "+", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite76 __input _ ->
    Left  (currentPos __input, ["*", "+", "=", "expect", "guard",
                                "notify", "<eof>"])
; STestSuite77 __input _ ->
    Left  (currentPos __input, ["*", "+", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite78 __input _ ->
    Left  (currentPos __input, ["*", "+", "=", "expect", "guard",
                                "notify", "<eof>"])
; STestSuite79 __input _ ->
    Left  (currentPos __input, ["*", "+", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite80 __input _ ->
    Left  (currentPos __input, ["*", "+", "=", "expect", "guard",
                                "notify", "<eof>"])
; STestSuite81 __input _ ->
    Left  (currentPos __input, ["*", "+", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite82 __input _ ->
    Left  (currentPos __input, ["*", "+", "=", "expect", "guard",
                                "notify", "<eof>"])
; STestSuite83 __input _ ->
    Left  (currentPos __input, ["*", "+", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite84 __input _ -> Left  (currentPos __input, [")"])
; STestSuite85 __input _ -> Left  (currentPos __input, [")"])
; STestSuite86 __input _ ->
    Left  (currentPos __input, ["*", "+", "=", "expect", "guard",
                                "notify", "<eof>"])
; STestSuite87 __input _ ->
    Left  (currentPos __input, ["*", "+", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite88 __input _ ->
    Left  (currentPos __input, ["*", "+", "=", "expect", "guard",
                                "notify", "<eof>"])
; STestSuite89 __input _ ->
    Left  (currentPos __input, ["*", "+", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite90 __input _ ->
    Left  (currentPos __input, ["+", "=", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite91 __input _ ->
    Left  (currentPos __input, ["+", "expect", "guard", "notify",
                                "<eof>"])
; STestSuite92 __input _ -> Left  (currentPos __input, ["<name>"])
; STestSuite93 __input _ -> Left  (currentPos __input, ["<name>"])
; STestSuite94 __input _ -> Left  (currentPos __input, ["("])
; STestSuite95 __input _ ->
    Left  (currentPos __input, ["expect", "guard", "notify"])
; STestSuite96 __input _ ->
    Left  (currentPos __input, ["expect", "guard", "notify", "<eof>"])
; STestSuite97 __input _ ->
    Left  (currentPos __input, ["expect", "guard", "notify", "<eof>"])
; STestSuite98 __input _ ->
    Left  (currentPos __input, ["expect", "guard", "notify", "<eof>"])
; STestSuite99 __input _ -> Left  (currentPos __input, [")"])
; STestSuite100 __input _ ->
    Left  (currentPos __input, ["expect", "guard", "notify", "<eof>"])
; STestSuite101 __input _ ->
    Left  (currentPos __input, ["expect", "guard", "notify", "<eof>"])
; STestSuite102 __input _ ->
    Left  (currentPos __input, ["expect", "guard", "notify", "<eof>"])
; STestSuite103 __input _ ->
    Left  (currentPos __input, ["expect", "guard", "notify", "<eof>"])
; STestSuite104 __input _ -> Left  (currentPos __input, ["test"])
; STestSuite105 __input _ -> Left  (currentPos __input, ["<eof>"])
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
    Right input -> pure (Right (__runTestSuite STestSuite104 input Nil))

currentPos :: ([Lexeme], Pos) -> Pos
currentPos = \case
  ([],           end) -> end
  ((pos, _) : _, _)   -> pos
