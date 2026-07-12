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
  SProgram0 :: StProgram (a)
  SProgram1 :: StProgram (Program : a)
  SProgram2 :: StProgram ([Stmt] : a)
  SProgram3 :: StProgram (Stmt : a)
  SProgram4 :: StProgram (Clause : a)
  SProgram5 :: StProgram (Effect : a)
  SProgram6 :: StProgram (Call : a)
  SProgram7 :: StProgram ([Stmt] : Stmt : a)
  SProgram8 :: StProgram (() : Call : a)
  SProgram9 :: StProgram (() : Call : a)
  SProgram10 :: StProgram (() : Call : a)
  SProgram11 :: StProgram (() : Call : a)
  SProgram12 :: StProgram ([Change] : () : Call : a)
  SProgram13 :: StProgram ([Cond] : () : Call : a)
  SProgram14 :: StProgram ([Cond] : () : Call : a)
  SProgram15 :: StProgram (() : [Change] : () : Call : a)
  SProgram16 :: StProgram (() : [Cond] : () : Call : a)
  SProgram17 :: StProgram (() : [Cond] : () : Call : a)
  SProgram18 :: StProgram (() : [Cond] : () : Call : a)
  SProgram19 :: StProgram ([Change] : () : [Cond] : () : Call : a)
  SProgram20 :: StProgram (() : [Change] : () : [Cond] : () : Call :
                           a)
  SProgram21 :: StProgram (Expr : a)
  SProgram22 :: StProgram (Expr : a)
  SProgram23 :: StProgram (Expr : a)
  SProgram24 :: StProgram (Expr : () : Expr : a)
  SProgram25 :: StProgram (Expr : () : Expr : a)
  SProgram26 :: StProgram (Expr : a)
  SProgram27 :: StProgram (Expr : a)
  SProgram28 :: StProgram (Expr : a)
  SProgram29 :: StProgram (Expr : a)
  SProgram30 :: StProgram (Expr : () : Expr : a)
  SProgram31 :: StProgram (Expr : () : Expr : a)
  SProgram32 :: StProgram (Expr : () : Expr : a)
  SProgram33 :: StProgram (Expr : () : Expr : a)
  SProgram34 :: StProgram (Expr : a)
  SProgram35 :: StProgram (Expr : a)
  SProgram36 :: StProgram (Expr : a)
  SProgram37 :: StProgram (Expr : a)
  SProgram38 :: StProgram (() : a)
  SProgram39 :: StProgram (() : a)
  SProgram40 :: StProgram (() : a)
  SProgram41 :: StProgram (() : a)
  SProgram42 :: StProgram (Text : a)
  SProgram43 :: StProgram (Text : a)
  SProgram44 :: StProgram (Text : a)
  SProgram45 :: StProgram (Text : a)
  SProgram46 :: StProgram (Const : a)
  SProgram47 :: StProgram (Const : a)
  SProgram48 :: StProgram (Const : a)
  SProgram49 :: StProgram (Const : a)
  SProgram50 :: StProgram (Text : a)
  SProgram51 :: StProgram (Text : a)
  SProgram52 :: StProgram (Text : a)
  SProgram53 :: StProgram (Text : a)
  SProgram54 :: StProgram (Integer : a)
  SProgram55 :: StProgram (Integer : a)
  SProgram56 :: StProgram (Integer : a)
  SProgram57 :: StProgram (Integer : a)
  SProgram58 :: StProgram (() : Expr : a)
  SProgram59 :: StProgram (() : Expr : a)
  SProgram60 :: StProgram (() : Expr : a)
  SProgram61 :: StProgram (() : Expr : a)
  SProgram62 :: StProgram (() : Expr : a)
  SProgram63 :: StProgram (() : Expr : a)
  SProgram64 :: StProgram (() : Expr : a)
  SProgram65 :: StProgram (() : Expr : a)
  SProgram66 :: StProgram (() : Expr : a)
  SProgram67 :: StProgram (() : Expr : a)
  SProgram68 :: StProgram (() : Expr : a)
  SProgram69 :: StProgram (Expr : () : a)
  SProgram70 :: StProgram (Expr : () : a)
  SProgram71 :: StProgram (Expr : () : a)
  SProgram72 :: StProgram (Expr : () : a)
  SProgram73 :: StProgram ([Expr] : () : Expr : a)
  SProgram74 :: StProgram (Expr : () : Expr : a)
  SProgram75 :: StProgram (Expr : () : Expr : a)
  SProgram76 :: StProgram (Expr : () : Expr : a)
  SProgram77 :: StProgram (Expr : () : Expr : a)
  SProgram78 :: StProgram (() : Expr : () : a)
  SProgram79 :: StProgram (() : Expr : () : a)
  SProgram80 :: StProgram (() : Expr : () : a)
  SProgram81 :: StProgram (() : Expr : () : a)
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
  SProgram94 :: StProgram (() : a)
  SProgram95 :: StProgram (() : a)
  SProgram96 :: StProgram (() : a)
  SProgram97 :: StProgram (() : a)
  SProgram98 :: StProgram (Text : a)
  SProgram99 :: StProgram (Text : a)
  SProgram100 :: StProgram (Text : a)
  SProgram101 :: StProgram (Text : a)
  SProgram102 :: StProgram (Const : a)
  SProgram103 :: StProgram (Const : a)
  SProgram104 :: StProgram (Const : a)
  SProgram105 :: StProgram (Const : a)
  SProgram106 :: StProgram (Text : a)
  SProgram107 :: StProgram (Text : a)
  SProgram108 :: StProgram (Text : a)
  SProgram109 :: StProgram (Text : a)
  SProgram110 :: StProgram (Text : a)
  SProgram111 :: StProgram (Text : a)
  SProgram112 :: StProgram (Integer : a)
  SProgram113 :: StProgram (Integer : a)
  SProgram114 :: StProgram (Integer : a)
  SProgram115 :: StProgram (Integer : a)
  SProgram116 :: StProgram (() : Expr : a)
  SProgram117 :: StProgram (() : Expr : a)
  SProgram118 :: StProgram (() : Expr : a)
  SProgram119 :: StProgram (() : Expr : a)
  SProgram120 :: StProgram (Expr : () : a)
  SProgram121 :: StProgram (Expr : () : a)
  SProgram122 :: StProgram (Expr : () : a)
  SProgram123 :: StProgram (Expr : () : a)
  SProgram124 :: StProgram (Expr : () : Expr : a)
  SProgram125 :: StProgram (Expr : () : Expr : a)
  SProgram126 :: StProgram (Expr : () : Expr : a)
  SProgram127 :: StProgram (Expr : () : Expr : a)
  SProgram128 :: StProgram (() : Expr : () : a)
  SProgram129 :: StProgram (() : Expr : () : a)
  SProgram130 :: StProgram (() : Expr : () : a)
  SProgram131 :: StProgram (() : Expr : () : a)
  SProgram132 :: StProgram (Expr : a)
  SProgram133 :: StProgram (Expr : a)
  SProgram134 :: StProgram (Expr : () : Expr : a)
  SProgram135 :: StProgram (Expr : () : Expr : a)
  SProgram136 :: StProgram (() : Expr : a)
  SProgram137 :: StProgram (() : Expr : a)
  SProgram138 :: StProgram (() : Expr : a)
  SProgram139 :: StProgram (() : Expr : a)
  SProgram140 :: StProgram (() : a)
  SProgram141 :: StProgram (() : a)
  SProgram142 :: StProgram (Call : a)
  SProgram143 :: StProgram (Call : a)
  SProgram144 :: StProgram (() : a)
  SProgram145 :: StProgram (() : a)
  SProgram146 :: StProgram (Expr : a)
  SProgram147 :: StProgram (Expr : a)
  SProgram148 :: StProgram (Text : a)
  SProgram149 :: StProgram (Text : a)
  SProgram150 :: StProgram (() : a)
  SProgram151 :: StProgram (() : a)
  SProgram152 :: StProgram (Call : () : a)
  SProgram153 :: StProgram (Call : () : a)
  SProgram154 :: StProgram (Call : () : a)
  SProgram155 :: StProgram (Call : () : a)
  SProgram156 :: StProgram ([Expr] : Text : a)
  SProgram157 :: StProgram ([Expr] : Text : a)
  SProgram158 :: StProgram (() : () : a)
  SProgram159 :: StProgram (() : () : a)
  SProgram160 :: StProgram ([Expr] : () : a)
  SProgram161 :: StProgram ([Expr] : () : a)
  SProgram162 :: StProgram (() : Expr : a)
  SProgram163 :: StProgram (() : Expr : a)
  SProgram164 :: StProgram (() : [Expr] : () : a)
  SProgram165 :: StProgram (() : [Expr] : () : a)
  SProgram166 :: StProgram (Text : a)
  SProgram167 :: StProgram (() : a)
  SProgram168 :: StProgram ([Expr] : Text : a)
  SProgram169 :: StProgram (() : () : a)
  SProgram170 :: StProgram ([Expr] : () : a)
  SProgram171 :: StProgram (() : [Expr] : () : a)
  SProgram172 :: StProgram (Change : a)
  SProgram173 :: StProgram (Cond : a)
  SProgram174 :: StProgram (Cond : a)
  SProgram175 :: StProgram (() : Change : a)
  SProgram176 :: StProgram (() : Cond : a)
  SProgram177 :: StProgram (() : Cond : a)
  SProgram178 :: StProgram ([Change] : () : Change : a)
  SProgram179 :: StProgram ([Cond] : () : Cond : a)
  SProgram180 :: StProgram ([Cond] : () : Cond : a)
  
__gotoAddForProgram :: ([Lexeme], Pos) -> Expr -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoAddForProgram toks term stk@(state, _, _) = case state of
  SProgram9 -> __runProgram SProgram133 toks (term :> stk)
  SProgram11 -> __runProgram SProgram132 toks (term :> stk)
  SProgram38 -> __runProgram SProgram23 toks (term :> stk)
  SProgram39 -> __runProgram SProgram23 toks (term :> stk)
  SProgram40 -> __runProgram SProgram23 toks (term :> stk)
  SProgram41 -> __runProgram SProgram23 toks (term :> stk)
  SProgram58 -> __runProgram SProgram22 toks (term :> stk)
  SProgram59 -> __runProgram SProgram24 toks (term :> stk)
  SProgram60 -> __runProgram SProgram25 toks (term :> stk)
  SProgram94 -> __runProgram SProgram23 toks (term :> stk)
  SProgram95 -> __runProgram SProgram23 toks (term :> stk)
  SProgram96 -> __runProgram SProgram23 toks (term :> stk)
  SProgram97 -> __runProgram SProgram23 toks (term :> stk)
  SProgram150 -> __runProgram SProgram22 toks (term :> stk)
  SProgram151 -> __runProgram SProgram22 toks (term :> stk)
  SProgram162 -> __runProgram SProgram134 toks (term :> stk)
  SProgram163 -> __runProgram SProgram135 toks (term :> stk)
  SProgram167 -> __runProgram SProgram22 toks (term :> stk)
  SProgram176 -> __runProgram SProgram132 toks (term :> stk)
  SProgram177 -> __runProgram SProgram133 toks (term :> stk)
  _ -> error ""

__gotoCallForProgram :: ([Lexeme], Pos) -> Call -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoCallForProgram toks term stk@(state, _, _) = case state of
  SProgram0 -> __runProgram SProgram6 toks (term :> stk)
  SProgram3 -> __runProgram SProgram6 toks (term :> stk)
  SProgram9 -> __runProgram SProgram143 toks (term :> stk)
  SProgram11 -> __runProgram SProgram142 toks (term :> stk)
  SProgram140 -> __runProgram SProgram152 toks (term :> stk)
  SProgram141 -> __runProgram SProgram153 toks (term :> stk)
  SProgram144 -> __runProgram SProgram154 toks (term :> stk)
  SProgram145 -> __runProgram SProgram155 toks (term :> stk)
  SProgram176 -> __runProgram SProgram142 toks (term :> stk)
  SProgram177 -> __runProgram SProgram143 toks (term :> stk)
  _ -> error ""

__gotoChangeForProgram :: ([Lexeme], Pos) -> Change -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoChangeForProgram toks term stk@(state, _, _) = case state of
  SProgram8 -> __runProgram SProgram172 toks (term :> stk)
  SProgram16 -> __runProgram SProgram172 toks (term :> stk)
  SProgram175 -> __runProgram SProgram172 toks (term :> stk)
  _ -> error ""

__gotoChangesForProgram :: ([Lexeme], Pos) -> [Change] -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoChangesForProgram toks term stk@(state, _, _) = case state of
  SProgram8 -> __runProgram SProgram12 toks (term :> stk)
  SProgram16 -> __runProgram SProgram19 toks (term :> stk)
  SProgram175 -> __runProgram SProgram178 toks (term :> stk)
  _ -> error ""

__gotoClauseForProgram :: ([Lexeme], Pos) -> Clause -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoClauseForProgram toks term stk@(state, _, _) = case state of
  SProgram0 -> __runProgram SProgram4 toks (term :> stk)
  SProgram3 -> __runProgram SProgram4 toks (term :> stk)
  _ -> error ""

__gotoCondForProgram :: ([Lexeme], Pos) -> Cond -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoCondForProgram toks term stk@(state, _, _) = case state of
  SProgram9 -> __runProgram SProgram174 toks (term :> stk)
  SProgram11 -> __runProgram SProgram173 toks (term :> stk)
  SProgram176 -> __runProgram SProgram173 toks (term :> stk)
  SProgram177 -> __runProgram SProgram174 toks (term :> stk)
  _ -> error ""

__gotoCondsForProgram :: ([Lexeme], Pos) -> [Cond] -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoCondsForProgram toks term stk@(state, _, _) = case state of
  SProgram9 -> __runProgram SProgram13 toks (term :> stk)
  SProgram11 -> __runProgram SProgram14 toks (term :> stk)
  SProgram176 -> __runProgram SProgram179 toks (term :> stk)
  SProgram177 -> __runProgram SProgram180 toks (term :> stk)
  _ -> error ""

__gotoConstForProgram :: ([Lexeme], Pos) -> Const -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoConstForProgram toks term stk@(state, _, _) = case state of
  SProgram9 -> __runProgram SProgram104 toks (term :> stk)
  SProgram11 -> __runProgram SProgram103 toks (term :> stk)
  SProgram38 -> __runProgram SProgram49 toks (term :> stk)
  SProgram39 -> __runProgram SProgram49 toks (term :> stk)
  SProgram40 -> __runProgram SProgram49 toks (term :> stk)
  SProgram41 -> __runProgram SProgram49 toks (term :> stk)
  SProgram58 -> __runProgram SProgram48 toks (term :> stk)
  SProgram59 -> __runProgram SProgram46 toks (term :> stk)
  SProgram60 -> __runProgram SProgram47 toks (term :> stk)
  SProgram61 -> __runProgram SProgram46 toks (term :> stk)
  SProgram62 -> __runProgram SProgram47 toks (term :> stk)
  SProgram63 -> __runProgram SProgram48 toks (term :> stk)
  SProgram64 -> __runProgram SProgram49 toks (term :> stk)
  SProgram65 -> __runProgram SProgram46 toks (term :> stk)
  SProgram66 -> __runProgram SProgram47 toks (term :> stk)
  SProgram67 -> __runProgram SProgram48 toks (term :> stk)
  SProgram68 -> __runProgram SProgram49 toks (term :> stk)
  SProgram94 -> __runProgram SProgram49 toks (term :> stk)
  SProgram95 -> __runProgram SProgram49 toks (term :> stk)
  SProgram96 -> __runProgram SProgram49 toks (term :> stk)
  SProgram97 -> __runProgram SProgram49 toks (term :> stk)
  SProgram116 -> __runProgram SProgram102 toks (term :> stk)
  SProgram117 -> __runProgram SProgram103 toks (term :> stk)
  SProgram118 -> __runProgram SProgram104 toks (term :> stk)
  SProgram119 -> __runProgram SProgram105 toks (term :> stk)
  SProgram136 -> __runProgram SProgram102 toks (term :> stk)
  SProgram137 -> __runProgram SProgram103 toks (term :> stk)
  SProgram138 -> __runProgram SProgram104 toks (term :> stk)
  SProgram139 -> __runProgram SProgram105 toks (term :> stk)
  SProgram150 -> __runProgram SProgram48 toks (term :> stk)
  SProgram151 -> __runProgram SProgram48 toks (term :> stk)
  SProgram162 -> __runProgram SProgram102 toks (term :> stk)
  SProgram163 -> __runProgram SProgram105 toks (term :> stk)
  SProgram167 -> __runProgram SProgram48 toks (term :> stk)
  SProgram176 -> __runProgram SProgram103 toks (term :> stk)
  SProgram177 -> __runProgram SProgram104 toks (term :> stk)
  _ -> error ""

__gotoEffectForProgram :: ([Lexeme], Pos) -> Effect -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoEffectForProgram toks term stk@(state, _, _) = case state of
  SProgram0 -> __runProgram SProgram5 toks (term :> stk)
  SProgram3 -> __runProgram SProgram5 toks (term :> stk)
  _ -> error ""

__gotoExprForProgram :: ([Lexeme], Pos) -> Expr -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoExprForProgram toks term stk@(state, _, _) = case state of
  SProgram9 -> __runProgram SProgram147 toks (term :> stk)
  SProgram11 -> __runProgram SProgram146 toks (term :> stk)
  SProgram38 -> __runProgram SProgram69 toks (term :> stk)
  SProgram39 -> __runProgram SProgram70 toks (term :> stk)
  SProgram40 -> __runProgram SProgram71 toks (term :> stk)
  SProgram41 -> __runProgram SProgram72 toks (term :> stk)
  SProgram58 -> __runProgram SProgram21 toks (term :> stk)
  SProgram94 -> __runProgram SProgram120 toks (term :> stk)
  SProgram95 -> __runProgram SProgram121 toks (term :> stk)
  SProgram96 -> __runProgram SProgram122 toks (term :> stk)
  SProgram97 -> __runProgram SProgram123 toks (term :> stk)
  SProgram150 -> __runProgram SProgram21 toks (term :> stk)
  SProgram151 -> __runProgram SProgram21 toks (term :> stk)
  SProgram167 -> __runProgram SProgram21 toks (term :> stk)
  SProgram176 -> __runProgram SProgram146 toks (term :> stk)
  SProgram177 -> __runProgram SProgram147 toks (term :> stk)
  _ -> error ""

__gotoExprs1ForProgram :: ([Lexeme], Pos) -> [Expr] -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoExprs1ForProgram toks term stk@(state, _, _) = case state of
  SProgram58 -> __runProgram SProgram73 toks (term :> stk)
  SProgram150 -> __runProgram SProgram160 toks (term :> stk)
  SProgram151 -> __runProgram SProgram161 toks (term :> stk)
  SProgram167 -> __runProgram SProgram170 toks (term :> stk)
  _ -> error ""

__gotoMultForProgram :: ([Lexeme], Pos) -> Expr -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoMultForProgram toks term stk@(state, _, _) = case state of
  SProgram9 -> __runProgram SProgram84 toks (term :> stk)
  SProgram11 -> __runProgram SProgram83 toks (term :> stk)
  SProgram38 -> __runProgram SProgram29 toks (term :> stk)
  SProgram39 -> __runProgram SProgram29 toks (term :> stk)
  SProgram40 -> __runProgram SProgram29 toks (term :> stk)
  SProgram41 -> __runProgram SProgram29 toks (term :> stk)
  SProgram58 -> __runProgram SProgram28 toks (term :> stk)
  SProgram59 -> __runProgram SProgram26 toks (term :> stk)
  SProgram60 -> __runProgram SProgram27 toks (term :> stk)
  SProgram61 -> __runProgram SProgram30 toks (term :> stk)
  SProgram62 -> __runProgram SProgram31 toks (term :> stk)
  SProgram63 -> __runProgram SProgram32 toks (term :> stk)
  SProgram64 -> __runProgram SProgram33 toks (term :> stk)
  SProgram94 -> __runProgram SProgram29 toks (term :> stk)
  SProgram95 -> __runProgram SProgram29 toks (term :> stk)
  SProgram96 -> __runProgram SProgram29 toks (term :> stk)
  SProgram97 -> __runProgram SProgram29 toks (term :> stk)
  SProgram136 -> __runProgram SProgram86 toks (term :> stk)
  SProgram137 -> __runProgram SProgram87 toks (term :> stk)
  SProgram138 -> __runProgram SProgram88 toks (term :> stk)
  SProgram139 -> __runProgram SProgram89 toks (term :> stk)
  SProgram150 -> __runProgram SProgram28 toks (term :> stk)
  SProgram151 -> __runProgram SProgram28 toks (term :> stk)
  SProgram162 -> __runProgram SProgram82 toks (term :> stk)
  SProgram163 -> __runProgram SProgram85 toks (term :> stk)
  SProgram167 -> __runProgram SProgram28 toks (term :> stk)
  SProgram176 -> __runProgram SProgram83 toks (term :> stk)
  SProgram177 -> __runProgram SProgram84 toks (term :> stk)
  _ -> error ""

__gotoProgramForProgram :: ([Lexeme], Pos) -> Program -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoProgramForProgram toks term stk@(state, _, _) = case state of
  SProgram0 -> __runProgram SProgram1 toks (term :> stk)
  _ -> error ""

__gotoStmtForProgram :: ([Lexeme], Pos) -> Stmt -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoStmtForProgram toks term stk@(state, _, _) = case state of
  SProgram0 -> __runProgram SProgram3 toks (term :> stk)
  SProgram3 -> __runProgram SProgram3 toks (term :> stk)
  _ -> error ""

__gotoStmtsForProgram :: ([Lexeme], Pos) -> [Stmt] -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoStmtsForProgram toks term stk@(state, _, _) = case state of
  SProgram0 -> __runProgram SProgram2 toks (term :> stk)
  SProgram3 -> __runProgram SProgram7 toks (term :> stk)
  _ -> error ""

__gotoTermForProgram :: ([Lexeme], Pos) -> Expr -> Stack StProgram a -> Either (Pos, [String]) Program
__gotoTermForProgram toks term stk@(state, _, _) = case state of
  SProgram9 -> __runProgram SProgram92 toks (term :> stk)
  SProgram11 -> __runProgram SProgram91 toks (term :> stk)
  SProgram38 -> __runProgram SProgram37 toks (term :> stk)
  SProgram39 -> __runProgram SProgram37 toks (term :> stk)
  SProgram40 -> __runProgram SProgram37 toks (term :> stk)
  SProgram41 -> __runProgram SProgram37 toks (term :> stk)
  SProgram58 -> __runProgram SProgram36 toks (term :> stk)
  SProgram59 -> __runProgram SProgram34 toks (term :> stk)
  SProgram60 -> __runProgram SProgram35 toks (term :> stk)
  SProgram61 -> __runProgram SProgram34 toks (term :> stk)
  SProgram62 -> __runProgram SProgram35 toks (term :> stk)
  SProgram63 -> __runProgram SProgram36 toks (term :> stk)
  SProgram64 -> __runProgram SProgram37 toks (term :> stk)
  SProgram65 -> __runProgram SProgram74 toks (term :> stk)
  SProgram66 -> __runProgram SProgram75 toks (term :> stk)
  SProgram67 -> __runProgram SProgram76 toks (term :> stk)
  SProgram68 -> __runProgram SProgram77 toks (term :> stk)
  SProgram94 -> __runProgram SProgram37 toks (term :> stk)
  SProgram95 -> __runProgram SProgram37 toks (term :> stk)
  SProgram96 -> __runProgram SProgram37 toks (term :> stk)
  SProgram97 -> __runProgram SProgram37 toks (term :> stk)
  SProgram116 -> __runProgram SProgram124 toks (term :> stk)
  SProgram117 -> __runProgram SProgram125 toks (term :> stk)
  SProgram118 -> __runProgram SProgram126 toks (term :> stk)
  SProgram119 -> __runProgram SProgram127 toks (term :> stk)
  SProgram136 -> __runProgram SProgram90 toks (term :> stk)
  SProgram137 -> __runProgram SProgram91 toks (term :> stk)
  SProgram138 -> __runProgram SProgram92 toks (term :> stk)
  SProgram139 -> __runProgram SProgram93 toks (term :> stk)
  SProgram150 -> __runProgram SProgram36 toks (term :> stk)
  SProgram151 -> __runProgram SProgram36 toks (term :> stk)
  SProgram162 -> __runProgram SProgram90 toks (term :> stk)
  SProgram163 -> __runProgram SProgram93 toks (term :> stk)
  SProgram167 -> __runProgram SProgram36 toks (term :> stk)
  SProgram176 -> __runProgram SProgram91 toks (term :> stk)
  SProgram177 -> __runProgram SProgram92 toks (term :> stk)
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
  SProgram106 -> __runProgram SProgram156 toks (term :> stk)
  SProgram107 -> __runProgram SProgram157 toks (term :> stk)
  SProgram148 -> __runProgram SProgram156 toks (term :> stk)
  SProgram149 -> __runProgram SProgram157 toks (term :> stk)
  SProgram166 -> __runProgram SProgram168 toks (term :> stk)
  _ -> error ""
  
__runProgram :: StProgram a -> ([Lexeme], Pos) -> Stack' StProgram a -> Either (Pos, [String]) Program
__runProgram = \cases {
; SProgram0 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram166 (__input, __end) (n :> (SProgram0, __p, __stk))
; SProgram3 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram166 (__input, __end) (n :> (SProgram3, __p, __stk))
; SProgram6 ((__p,  "->") : __input, __end) __stk ->
    __runProgram SProgram9 (__input, __end) (() :> (SProgram6, __p, __stk))
; SProgram6 ((__p,  ".") : __input, __end) __stk ->
    __runProgram SProgram10 (__input, __end) (() :> (SProgram6, __p, __stk))
; SProgram6 ((__p,  "<-") : __input, __end) __stk ->
    __runProgram SProgram11 (__input, __end) (() :> (SProgram6, __p, __stk))
; SProgram6 ((__p,  "=>") : __input, __end) __stk ->
    __runProgram SProgram8 (__input, __end) (() :> (SProgram6, __p, __stk))
; SProgram8 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram140 (__input, __end) (() :> (SProgram8, __p, __stk))
; SProgram8 ((__p,  "-") : __input, __end) __stk ->
    __runProgram SProgram141 (__input, __end) (() :> (SProgram8, __p, __stk))
; SProgram9 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram96 (__input, __end) (() :> (SProgram9, __p, __stk))
; SProgram9 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram100 (__input, __end) (n :> (SProgram9, __p, __stk))
; SProgram9 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram107 (__input, __end) (n :> (SProgram9, __p, __stk))
; SProgram9 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram114 (__input, __end) (n :> (SProgram9, __p, __stk))
; SProgram9 ((__p,  "~") : __input, __end) __stk ->
    __runProgram SProgram145 (__input, __end) (() :> (SProgram9, __p, __stk))
; SProgram11 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram95 (__input, __end) (() :> (SProgram11, __p, __stk))
; SProgram11 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram99 (__input, __end) (n :> (SProgram11, __p, __stk))
; SProgram11 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram106 (__input, __end) (n :> (SProgram11, __p, __stk))
; SProgram11 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram113 (__input, __end) (n :> (SProgram11, __p, __stk))
; SProgram11 ((__p,  "~") : __input, __end) __stk ->
    __runProgram SProgram144 (__input, __end) (() :> (SProgram11, __p, __stk))
; SProgram12 ((__p,  ".") : __input, __end) __stk ->
    __runProgram SProgram15 (__input, __end) (() :> (SProgram12, __p, __stk))
; SProgram13 ((__p,  ".") : __input, __end) __stk ->
    __runProgram SProgram17 (__input, __end) (() :> (SProgram13, __p, __stk))
; SProgram13 ((__p,  "=>") : __input, __end) __stk ->
    __runProgram SProgram16 (__input, __end) (() :> (SProgram13, __p, __stk))
; SProgram14 ((__p,  ".") : __input, __end) __stk ->
    __runProgram SProgram18 (__input, __end) (() :> (SProgram14, __p, __stk))
; SProgram16 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram140 (__input, __end) (() :> (SProgram16, __p, __stk))
; SProgram16 ((__p,  "-") : __input, __end) __stk ->
    __runProgram SProgram141 (__input, __end) (() :> (SProgram16, __p, __stk))
; SProgram19 ((__p,  ".") : __input, __end) __stk ->
    __runProgram SProgram20 (__input, __end) (() :> (SProgram19, __p, __stk))
; SProgram21 ((__p,  ",") : __input, __end) __stk ->
    __runProgram SProgram58 (__input, __end) (() :> (SProgram21, __p, __stk))
; SProgram22 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram63 (__input, __end) (() :> (SProgram22, __p, __stk))
; SProgram22 ((__p,  "=") : __input, __end) __stk ->
    __runProgram SProgram60 (__input, __end) (() :> (SProgram22, __p, __stk))
; SProgram23 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram64 (__input, __end) (() :> (SProgram23, __p, __stk))
; SProgram23 ((__p,  "=") : __input, __end) __stk ->
    __runProgram SProgram59 (__input, __end) (() :> (SProgram23, __p, __stk))
; SProgram24 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram61 (__input, __end) (() :> (SProgram24, __p, __stk))
; SProgram25 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram62 (__input, __end) (() :> (SProgram25, __p, __stk))
; SProgram26 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram65 (__input, __end) (() :> (SProgram26, __p, __stk))
; SProgram27 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram66 (__input, __end) (() :> (SProgram27, __p, __stk))
; SProgram28 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram67 (__input, __end) (() :> (SProgram28, __p, __stk))
; SProgram29 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram68 (__input, __end) (() :> (SProgram29, __p, __stk))
; SProgram30 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram65 (__input, __end) (() :> (SProgram30, __p, __stk))
; SProgram31 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram66 (__input, __end) (() :> (SProgram31, __p, __stk))
; SProgram32 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram67 (__input, __end) (() :> (SProgram32, __p, __stk))
; SProgram33 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram68 (__input, __end) (() :> (SProgram33, __p, __stk))
; SProgram38 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram41 (__input, __end) (() :> (SProgram38, __p, __stk))
; SProgram38 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram45 (__input, __end) (n :> (SProgram38, __p, __stk))
; SProgram38 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram53 (__input, __end) (n :> (SProgram38, __p, __stk))
; SProgram38 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram57 (__input, __end) (n :> (SProgram38, __p, __stk))
; SProgram39 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram41 (__input, __end) (() :> (SProgram39, __p, __stk))
; SProgram39 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram45 (__input, __end) (n :> (SProgram39, __p, __stk))
; SProgram39 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram53 (__input, __end) (n :> (SProgram39, __p, __stk))
; SProgram39 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram57 (__input, __end) (n :> (SProgram39, __p, __stk))
; SProgram40 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram41 (__input, __end) (() :> (SProgram40, __p, __stk))
; SProgram40 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram45 (__input, __end) (n :> (SProgram40, __p, __stk))
; SProgram40 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram53 (__input, __end) (n :> (SProgram40, __p, __stk))
; SProgram40 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram57 (__input, __end) (n :> (SProgram40, __p, __stk))
; SProgram41 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram41 (__input, __end) (() :> (SProgram41, __p, __stk))
; SProgram41 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram45 (__input, __end) (n :> (SProgram41, __p, __stk))
; SProgram41 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram53 (__input, __end) (n :> (SProgram41, __p, __stk))
; SProgram41 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram57 (__input, __end) (n :> (SProgram41, __p, __stk))
; SProgram58 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram40 (__input, __end) (() :> (SProgram58, __p, __stk))
; SProgram58 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram44 (__input, __end) (n :> (SProgram58, __p, __stk))
; SProgram58 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram52 (__input, __end) (n :> (SProgram58, __p, __stk))
; SProgram58 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram56 (__input, __end) (n :> (SProgram58, __p, __stk))
; SProgram59 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram38 (__input, __end) (() :> (SProgram59, __p, __stk))
; SProgram59 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram42 (__input, __end) (n :> (SProgram59, __p, __stk))
; SProgram59 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram50 (__input, __end) (n :> (SProgram59, __p, __stk))
; SProgram59 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram54 (__input, __end) (n :> (SProgram59, __p, __stk))
; SProgram60 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram39 (__input, __end) (() :> (SProgram60, __p, __stk))
; SProgram60 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram43 (__input, __end) (n :> (SProgram60, __p, __stk))
; SProgram60 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram51 (__input, __end) (n :> (SProgram60, __p, __stk))
; SProgram60 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram55 (__input, __end) (n :> (SProgram60, __p, __stk))
; SProgram61 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram38 (__input, __end) (() :> (SProgram61, __p, __stk))
; SProgram61 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram42 (__input, __end) (n :> (SProgram61, __p, __stk))
; SProgram61 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram50 (__input, __end) (n :> (SProgram61, __p, __stk))
; SProgram61 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram54 (__input, __end) (n :> (SProgram61, __p, __stk))
; SProgram62 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram39 (__input, __end) (() :> (SProgram62, __p, __stk))
; SProgram62 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram43 (__input, __end) (n :> (SProgram62, __p, __stk))
; SProgram62 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram51 (__input, __end) (n :> (SProgram62, __p, __stk))
; SProgram62 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram55 (__input, __end) (n :> (SProgram62, __p, __stk))
; SProgram63 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram40 (__input, __end) (() :> (SProgram63, __p, __stk))
; SProgram63 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram44 (__input, __end) (n :> (SProgram63, __p, __stk))
; SProgram63 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram52 (__input, __end) (n :> (SProgram63, __p, __stk))
; SProgram63 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram56 (__input, __end) (n :> (SProgram63, __p, __stk))
; SProgram64 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram41 (__input, __end) (() :> (SProgram64, __p, __stk))
; SProgram64 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram45 (__input, __end) (n :> (SProgram64, __p, __stk))
; SProgram64 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram53 (__input, __end) (n :> (SProgram64, __p, __stk))
; SProgram64 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram57 (__input, __end) (n :> (SProgram64, __p, __stk))
; SProgram65 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram38 (__input, __end) (() :> (SProgram65, __p, __stk))
; SProgram65 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram42 (__input, __end) (n :> (SProgram65, __p, __stk))
; SProgram65 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram50 (__input, __end) (n :> (SProgram65, __p, __stk))
; SProgram65 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram54 (__input, __end) (n :> (SProgram65, __p, __stk))
; SProgram66 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram39 (__input, __end) (() :> (SProgram66, __p, __stk))
; SProgram66 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram43 (__input, __end) (n :> (SProgram66, __p, __stk))
; SProgram66 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram51 (__input, __end) (n :> (SProgram66, __p, __stk))
; SProgram66 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram55 (__input, __end) (n :> (SProgram66, __p, __stk))
; SProgram67 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram40 (__input, __end) (() :> (SProgram67, __p, __stk))
; SProgram67 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram44 (__input, __end) (n :> (SProgram67, __p, __stk))
; SProgram67 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram52 (__input, __end) (n :> (SProgram67, __p, __stk))
; SProgram67 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram56 (__input, __end) (n :> (SProgram67, __p, __stk))
; SProgram68 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram41 (__input, __end) (() :> (SProgram68, __p, __stk))
; SProgram68 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram45 (__input, __end) (n :> (SProgram68, __p, __stk))
; SProgram68 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram53 (__input, __end) (n :> (SProgram68, __p, __stk))
; SProgram68 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram57 (__input, __end) (n :> (SProgram68, __p, __stk))
; SProgram69 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram78 (__input, __end) (() :> (SProgram69, __p, __stk))
; SProgram70 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram79 (__input, __end) (() :> (SProgram70, __p, __stk))
; SProgram71 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram80 (__input, __end) (() :> (SProgram71, __p, __stk))
; SProgram72 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram81 (__input, __end) (() :> (SProgram72, __p, __stk))
; SProgram82 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram116 (__input, __end) (() :> (SProgram82, __p, __stk))
; SProgram83 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram117 (__input, __end) (() :> (SProgram83, __p, __stk))
; SProgram84 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram118 (__input, __end) (() :> (SProgram84, __p, __stk))
; SProgram85 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram119 (__input, __end) (() :> (SProgram85, __p, __stk))
; SProgram86 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram116 (__input, __end) (() :> (SProgram86, __p, __stk))
; SProgram87 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram117 (__input, __end) (() :> (SProgram87, __p, __stk))
; SProgram88 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram118 (__input, __end) (() :> (SProgram88, __p, __stk))
; SProgram89 ((__p,  "*") : __input, __end) __stk ->
    __runProgram SProgram119 (__input, __end) (() :> (SProgram89, __p, __stk))
; SProgram94 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram41 (__input, __end) (() :> (SProgram94, __p, __stk))
; SProgram94 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram45 (__input, __end) (n :> (SProgram94, __p, __stk))
; SProgram94 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram53 (__input, __end) (n :> (SProgram94, __p, __stk))
; SProgram94 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram57 (__input, __end) (n :> (SProgram94, __p, __stk))
; SProgram95 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram41 (__input, __end) (() :> (SProgram95, __p, __stk))
; SProgram95 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram45 (__input, __end) (n :> (SProgram95, __p, __stk))
; SProgram95 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram53 (__input, __end) (n :> (SProgram95, __p, __stk))
; SProgram95 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram57 (__input, __end) (n :> (SProgram95, __p, __stk))
; SProgram96 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram41 (__input, __end) (() :> (SProgram96, __p, __stk))
; SProgram96 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram45 (__input, __end) (n :> (SProgram96, __p, __stk))
; SProgram96 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram53 (__input, __end) (n :> (SProgram96, __p, __stk))
; SProgram96 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram57 (__input, __end) (n :> (SProgram96, __p, __stk))
; SProgram97 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram41 (__input, __end) (() :> (SProgram97, __p, __stk))
; SProgram97 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram45 (__input, __end) (n :> (SProgram97, __p, __stk))
; SProgram97 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram53 (__input, __end) (n :> (SProgram97, __p, __stk))
; SProgram97 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram57 (__input, __end) (n :> (SProgram97, __p, __stk))
; SProgram106 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram150 (__input, __end) (() :> (SProgram106, __p, __stk))
; SProgram107 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram151 (__input, __end) (() :> (SProgram107, __p, __stk))
; SProgram116 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram94 (__input, __end) (() :> (SProgram116, __p, __stk))
; SProgram116 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram98 (__input, __end) (n :> (SProgram116, __p, __stk))
; SProgram116 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram108 (__input, __end) (n :> (SProgram116, __p, __stk))
; SProgram116 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram112 (__input, __end) (n :> (SProgram116, __p, __stk))
; SProgram117 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram95 (__input, __end) (() :> (SProgram117, __p, __stk))
; SProgram117 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram99 (__input, __end) (n :> (SProgram117, __p, __stk))
; SProgram117 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram109 (__input, __end) (n :> (SProgram117, __p, __stk))
; SProgram117 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram113 (__input, __end) (n :> (SProgram117, __p, __stk))
; SProgram118 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram96 (__input, __end) (() :> (SProgram118, __p, __stk))
; SProgram118 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram100 (__input, __end) (n :> (SProgram118, __p, __stk))
; SProgram118 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram110 (__input, __end) (n :> (SProgram118, __p, __stk))
; SProgram118 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram114 (__input, __end) (n :> (SProgram118, __p, __stk))
; SProgram119 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram97 (__input, __end) (() :> (SProgram119, __p, __stk))
; SProgram119 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram101 (__input, __end) (n :> (SProgram119, __p, __stk))
; SProgram119 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram111 (__input, __end) (n :> (SProgram119, __p, __stk))
; SProgram119 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram115 (__input, __end) (n :> (SProgram119, __p, __stk))
; SProgram120 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram128 (__input, __end) (() :> (SProgram120, __p, __stk))
; SProgram121 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram129 (__input, __end) (() :> (SProgram121, __p, __stk))
; SProgram122 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram130 (__input, __end) (() :> (SProgram122, __p, __stk))
; SProgram123 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram131 (__input, __end) (() :> (SProgram123, __p, __stk))
; SProgram132 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram137 (__input, __end) (() :> (SProgram132, __p, __stk))
; SProgram132 ((__p,  "=") : __input, __end) __stk ->
    __runProgram SProgram162 (__input, __end) (() :> (SProgram132, __p, __stk))
; SProgram133 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram138 (__input, __end) (() :> (SProgram133, __p, __stk))
; SProgram133 ((__p,  "=") : __input, __end) __stk ->
    __runProgram SProgram163 (__input, __end) (() :> (SProgram133, __p, __stk))
; SProgram134 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram136 (__input, __end) (() :> (SProgram134, __p, __stk))
; SProgram135 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram139 (__input, __end) (() :> (SProgram135, __p, __stk))
; SProgram136 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram94 (__input, __end) (() :> (SProgram136, __p, __stk))
; SProgram136 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram98 (__input, __end) (n :> (SProgram136, __p, __stk))
; SProgram136 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram108 (__input, __end) (n :> (SProgram136, __p, __stk))
; SProgram136 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram112 (__input, __end) (n :> (SProgram136, __p, __stk))
; SProgram137 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram95 (__input, __end) (() :> (SProgram137, __p, __stk))
; SProgram137 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram99 (__input, __end) (n :> (SProgram137, __p, __stk))
; SProgram137 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram109 (__input, __end) (n :> (SProgram137, __p, __stk))
; SProgram137 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram113 (__input, __end) (n :> (SProgram137, __p, __stk))
; SProgram138 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram96 (__input, __end) (() :> (SProgram138, __p, __stk))
; SProgram138 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram100 (__input, __end) (n :> (SProgram138, __p, __stk))
; SProgram138 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram110 (__input, __end) (n :> (SProgram138, __p, __stk))
; SProgram138 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram114 (__input, __end) (n :> (SProgram138, __p, __stk))
; SProgram139 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram97 (__input, __end) (() :> (SProgram139, __p, __stk))
; SProgram139 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram101 (__input, __end) (n :> (SProgram139, __p, __stk))
; SProgram139 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram111 (__input, __end) (n :> (SProgram139, __p, __stk))
; SProgram139 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram115 (__input, __end) (n :> (SProgram139, __p, __stk))
; SProgram140 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram148 (__input, __end) (n :> (SProgram140, __p, __stk))
; SProgram141 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram148 (__input, __end) (n :> (SProgram141, __p, __stk))
; SProgram144 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram148 (__input, __end) (n :> (SProgram144, __p, __stk))
; SProgram145 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram149 (__input, __end) (n :> (SProgram145, __p, __stk))
; SProgram148 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram150 (__input, __end) (() :> (SProgram148, __p, __stk))
; SProgram149 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram151 (__input, __end) (() :> (SProgram149, __p, __stk))
; SProgram150 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram40 (__input, __end) (() :> (SProgram150, __p, __stk))
; SProgram150 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram158 (__input, __end) (() :> (SProgram150, __p, __stk))
; SProgram150 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram44 (__input, __end) (n :> (SProgram150, __p, __stk))
; SProgram150 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram52 (__input, __end) (n :> (SProgram150, __p, __stk))
; SProgram150 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram56 (__input, __end) (n :> (SProgram150, __p, __stk))
; SProgram151 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram40 (__input, __end) (() :> (SProgram151, __p, __stk))
; SProgram151 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram159 (__input, __end) (() :> (SProgram151, __p, __stk))
; SProgram151 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram44 (__input, __end) (n :> (SProgram151, __p, __stk))
; SProgram151 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram52 (__input, __end) (n :> (SProgram151, __p, __stk))
; SProgram151 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram56 (__input, __end) (n :> (SProgram151, __p, __stk))
; SProgram160 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram164 (__input, __end) (() :> (SProgram160, __p, __stk))
; SProgram161 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram165 (__input, __end) (() :> (SProgram161, __p, __stk))
; SProgram162 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram94 (__input, __end) (() :> (SProgram162, __p, __stk))
; SProgram162 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram98 (__input, __end) (n :> (SProgram162, __p, __stk))
; SProgram162 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram108 (__input, __end) (n :> (SProgram162, __p, __stk))
; SProgram162 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram112 (__input, __end) (n :> (SProgram162, __p, __stk))
; SProgram163 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram97 (__input, __end) (() :> (SProgram163, __p, __stk))
; SProgram163 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram101 (__input, __end) (n :> (SProgram163, __p, __stk))
; SProgram163 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram111 (__input, __end) (n :> (SProgram163, __p, __stk))
; SProgram163 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram115 (__input, __end) (n :> (SProgram163, __p, __stk))
; SProgram166 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram167 (__input, __end) (() :> (SProgram166, __p, __stk))
; SProgram167 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram40 (__input, __end) (() :> (SProgram167, __p, __stk))
; SProgram167 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram169 (__input, __end) (() :> (SProgram167, __p, __stk))
; SProgram167 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram44 (__input, __end) (n :> (SProgram167, __p, __stk))
; SProgram167 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram52 (__input, __end) (n :> (SProgram167, __p, __stk))
; SProgram167 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram56 (__input, __end) (n :> (SProgram167, __p, __stk))
; SProgram170 ((__p,  ")") : __input, __end) __stk ->
    __runProgram SProgram171 (__input, __end) (() :> (SProgram170, __p, __stk))
; SProgram172 ((__p,  ",") : __input, __end) __stk ->
    __runProgram SProgram175 (__input, __end) (() :> (SProgram172, __p, __stk))
; SProgram173 ((__p,  ",") : __input, __end) __stk ->
    __runProgram SProgram176 (__input, __end) (() :> (SProgram173, __p, __stk))
; SProgram174 ((__p,  ",") : __input, __end) __stk ->
    __runProgram SProgram177 (__input, __end) (() :> (SProgram174, __p, __stk))
; SProgram175 ((__p,  "+") : __input, __end) __stk ->
    __runProgram SProgram140 (__input, __end) (() :> (SProgram175, __p, __stk))
; SProgram175 ((__p,  "-") : __input, __end) __stk ->
    __runProgram SProgram141 (__input, __end) (() :> (SProgram175, __p, __stk))
; SProgram176 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram95 (__input, __end) (() :> (SProgram176, __p, __stk))
; SProgram176 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram99 (__input, __end) (n :> (SProgram176, __p, __stk))
; SProgram176 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram106 (__input, __end) (n :> (SProgram176, __p, __stk))
; SProgram176 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram113 (__input, __end) (n :> (SProgram176, __p, __stk))
; SProgram176 ((__p,  "~") : __input, __end) __stk ->
    __runProgram SProgram144 (__input, __end) (() :> (SProgram176, __p, __stk))
; SProgram177 ((__p,  "(") : __input, __end) __stk ->
    __runProgram SProgram96 (__input, __end) (() :> (SProgram177, __p, __stk))
; SProgram177 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram SProgram100 (__input, __end) (n :> (SProgram177, __p, __stk))
; SProgram177 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram SProgram107 (__input, __end) (n :> (SProgram177, __p, __stk))
; SProgram177 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram SProgram114 (__input, __end) (n :> (SProgram177, __p, __stk))
; SProgram177 ((__p,  "~") : __input, __end) __stk ->
    __runProgram SProgram145 (__input, __end) (() :> (SProgram177, __p, __stk))
-- lookahead Nothing, entity Program
; SProgram1 ([], __end) ((res :> __stk@(_, __pos, _))) -> pure res
-- lookahead Nothing, entity Program
; SProgram2 ([], __end) ((stmts :> __stk@(_, __pos, _))) ->
    __gotoProgramForProgram ([], __end) (action10 __pos stmts) __stk
-- lookahead Nothing, entity Stmts
; SProgram3 ([], __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoStmtsForProgram ([], __end) (action14 __pos c) __stk
-- lookahead Nothing, entity Stmt
; SProgram4 ([], __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoStmtForProgram ([], __end) (action17 __pos c) __stk
-- lookahead Just <name>, entity Stmt
; SProgram4 ((__p, LowercaseName tok) : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoStmtForProgram ((__p, LowercaseName tok) : __input, __end) (action17 __pos c) __stk
-- lookahead Nothing, entity Stmt
; SProgram5 ([], __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoStmtForProgram ([], __end) (action18 __pos e) __stk
-- lookahead Just <name>, entity Stmt
; SProgram5 ((__p, LowercaseName tok) : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoStmtForProgram ((__p, LowercaseName tok) : __input, __end) (action18 __pos e) __stk
-- lookahead Nothing, entity Stmts
; SProgram7 ([], __end) ((cs :> (_, _, c :> __stk@(_, __pos, _)))) ->
    __gotoStmtsForProgram ([], __end) (action13 __pos c cs) __stk
-- lookahead Nothing, entity Clause
; SProgram10 ([], __end) ((_ :> (_, _, c :> __stk@(_, __pos, _)))) ->
    __gotoClauseForProgram ([], __end) (action26 __pos c) __stk
-- lookahead Just <name>, entity Clause
; SProgram10 ((__p, LowercaseName tok) : __input, __end) ((_ :> (_, _, c :> __stk@(_, __pos, _)))) ->
    __gotoClauseForProgram ((__p, LowercaseName tok) : __input, __end) (action26 __pos c) __stk
-- lookahead Nothing, entity Effect
; SProgram15 ([], __end) ((_ :> (_, _, ds :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))) ->
    __gotoEffectForProgram ([], __end) (action21 __pos c ds) __stk
-- lookahead Just <name>, entity Effect
; SProgram15 ((__p, LowercaseName tok) : __input, __end) ((_ :> (_, _, ds :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))) ->
    __gotoEffectForProgram ((__p, LowercaseName tok) : __input, __end) (action21 __pos c
                                                                                       ds) __stk
-- lookahead Nothing, entity Effect
; SProgram17 ([], __end) ((_ :> (_, _, cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))) ->
    __gotoEffectForProgram ([], __end) (action23 __pos c cs) __stk
-- lookahead Just <name>, entity Effect
; SProgram17 ((__p, LowercaseName tok) : __input, __end) ((_ :> (_, _, cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))) ->
    __gotoEffectForProgram ((__p, LowercaseName tok) : __input, __end) (action23 __pos c
                                                                                       cs) __stk
-- lookahead Nothing, entity Clause
; SProgram18 ([], __end) ((_ :> (_, _, cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))) ->
    __gotoClauseForProgram ([], __end) (action27 __pos c cs) __stk
-- lookahead Just <name>, entity Clause
; SProgram18 ((__p, LowercaseName tok) : __input, __end) ((_ :> (_, _, cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))) ->
    __gotoClauseForProgram ((__p, LowercaseName tok) : __input, __end) (action27 __pos c
                                                                                       cs) __stk
-- lookahead Nothing, entity Effect
; SProgram20 ([], __end) ((_ :> (_, _, ds :> (_, _, _ :> (_, _, cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))))) ->
    __gotoEffectForProgram ([], __end) (action22 __pos c cs ds) __stk
-- lookahead Just <name>, entity Effect
; SProgram20 ((__p, LowercaseName tok) : __input, __end) ((_ :> (_, _, ds :> (_, _, _ :> (_, _, cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _)))))))) ->
    __gotoEffectForProgram ((__p, LowercaseName tok) : __input, __end) (action22 __pos c
                                                                                       cs ds) __stk
-- lookahead Just ), entity Exprs1
; SProgram21 ((__p,  ")") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoExprs1ForProgram ((__p,  ")") : __input, __end) (action55 __pos e) __stk
-- lookahead Just ), entity Expr
; SProgram22 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  ")") : __input, __end) (action59 __pos a) __stk
-- lookahead Just ,, entity Expr
; SProgram22 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  ",") : __input, __end) (action59 __pos a) __stk
-- lookahead Just ), entity Expr
; SProgram23 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  ")") : __input, __end) (action59 __pos a) __stk
-- lookahead Just ), entity Expr
; SProgram24 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  ")") : __input, __end) (action58 __pos a
                                                                        b) __stk
-- lookahead Just ), entity Expr
; SProgram25 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  ")") : __input, __end) (action58 __pos a
                                                                        b) __stk
-- lookahead Just ,, entity Expr
; SProgram25 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  ",") : __input, __end) (action58 __pos a
                                                                        b) __stk
-- lookahead Just ), entity Add
; SProgram26 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead Just +, entity Add
; SProgram26 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead Just ), entity Add
; SProgram27 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead Just +, entity Add
; SProgram27 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead Just ,, entity Add
; SProgram27 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead Just ), entity Add
; SProgram28 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead Just +, entity Add
; SProgram28 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead Just ,, entity Add
; SProgram28 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead Just =, entity Add
; SProgram28 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action63 __pos a) __stk
-- lookahead Just ), entity Add
; SProgram29 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead Just +, entity Add
; SProgram29 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead Just =, entity Add
; SProgram29 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action63 __pos a) __stk
-- lookahead Just ), entity Add
; SProgram30 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just +, entity Add
; SProgram30 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just ), entity Add
; SProgram31 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just +, entity Add
; SProgram31 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just ,, entity Add
; SProgram31 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just ), entity Add
; SProgram32 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just +, entity Add
; SProgram32 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just ,, entity Add
; SProgram32 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just =, entity Add
; SProgram32 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just ), entity Add
; SProgram33 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ")") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just +, entity Add
; SProgram33 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just =, entity Add
; SProgram33 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just ), entity Mult
; SProgram34 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead Just *, entity Mult
; SProgram34 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead Just +, entity Mult
; SProgram34 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead Just ), entity Mult
; SProgram35 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead Just *, entity Mult
; SProgram35 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead Just +, entity Mult
; SProgram35 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead Just ,, entity Mult
; SProgram35 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead Just ), entity Mult
; SProgram36 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead Just *, entity Mult
; SProgram36 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead Just +, entity Mult
; SProgram36 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead Just ,, entity Mult
; SProgram36 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead Just =, entity Mult
; SProgram36 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action67 __pos a) __stk
-- lookahead Just ), entity Mult
; SProgram37 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead Just *, entity Mult
; SProgram37 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead Just +, entity Mult
; SProgram37 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead Just =, entity Mult
; SProgram37 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action67 __pos a) __stk
-- lookahead Just ), entity Term
; SProgram42 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead Just *, entity Term
; SProgram42 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead Just +, entity Term
; SProgram42 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead Just ), entity Term
; SProgram43 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead Just *, entity Term
; SProgram43 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead Just +, entity Term
; SProgram43 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead Just ,, entity Term
; SProgram43 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead Just ), entity Term
; SProgram44 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead Just *, entity Term
; SProgram44 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead Just +, entity Term
; SProgram44 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead Just ,, entity Term
; SProgram44 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead Just =, entity Term
; SProgram44 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action71 __pos n) __stk
-- lookahead Just ), entity Term
; SProgram45 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead Just *, entity Term
; SProgram45 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead Just +, entity Term
; SProgram45 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead Just =, entity Term
; SProgram45 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action71 __pos n) __stk
-- lookahead Just ), entity Term
; SProgram46 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead Just *, entity Term
; SProgram46 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead Just +, entity Term
; SProgram46 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead Just ), entity Term
; SProgram47 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead Just *, entity Term
; SProgram47 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead Just +, entity Term
; SProgram47 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead Just ,, entity Term
; SProgram47 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead Just ), entity Term
; SProgram48 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead Just *, entity Term
; SProgram48 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead Just +, entity Term
; SProgram48 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead Just ,, entity Term
; SProgram48 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead Just =, entity Term
; SProgram48 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action72 __pos n) __stk
-- lookahead Just ), entity Term
; SProgram49 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead Just *, entity Term
; SProgram49 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead Just +, entity Term
; SProgram49 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead Just =, entity Term
; SProgram49 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action72 __pos n) __stk
-- lookahead Just ), entity Const
; SProgram50 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead Just *, entity Const
; SProgram50 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead Just +, entity Const
; SProgram50 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ), entity Const
; SProgram51 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead Just *, entity Const
; SProgram51 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead Just +, entity Const
; SProgram51 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ,, entity Const
; SProgram51 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ), entity Const
; SProgram52 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead Just *, entity Const
; SProgram52 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead Just +, entity Const
; SProgram52 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ,, entity Const
; SProgram52 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead Just =, entity Const
; SProgram52 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ), entity Const
; SProgram53 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead Just *, entity Const
; SProgram53 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead Just +, entity Const
; SProgram53 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead Just =, entity Const
; SProgram53 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ), entity Const
; SProgram54 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead Just *, entity Const
; SProgram54 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead Just +, entity Const
; SProgram54 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead Just ), entity Const
; SProgram55 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead Just *, entity Const
; SProgram55 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead Just +, entity Const
; SProgram55 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead Just ,, entity Const
; SProgram55 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead Just ), entity Const
; SProgram56 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead Just *, entity Const
; SProgram56 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead Just +, entity Const
; SProgram56 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead Just ,, entity Const
; SProgram56 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead Just =, entity Const
; SProgram56 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action76 __pos n) __stk
-- lookahead Just ), entity Const
; SProgram57 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead Just *, entity Const
; SProgram57 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead Just +, entity Const
; SProgram57 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead Just =, entity Const
; SProgram57 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action76 __pos n) __stk
-- lookahead Just ), entity Exprs1
; SProgram73 ((__p,  ")") : __input, __end) ((es :> (_, _, _ :> (_, _, e :> __stk@(_, __pos, _))))) ->
    __gotoExprs1ForProgram ((__p,  ")") : __input, __end) (action54 __pos e
                                                                          es) __stk
-- lookahead Just ), entity Mult
; SProgram74 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just *, entity Mult
; SProgram74 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just +, entity Mult
; SProgram74 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just ), entity Mult
; SProgram75 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just *, entity Mult
; SProgram75 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just +, entity Mult
; SProgram75 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just ,, entity Mult
; SProgram75 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just ), entity Mult
; SProgram76 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just *, entity Mult
; SProgram76 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just +, entity Mult
; SProgram76 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just ,, entity Mult
; SProgram76 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just =, entity Mult
; SProgram76 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just ), entity Mult
; SProgram77 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ")") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just *, entity Mult
; SProgram77 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just +, entity Mult
; SProgram77 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just =, entity Mult
; SProgram77 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just ), entity Term
; SProgram78 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead Just *, entity Term
; SProgram78 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead Just +, entity Term
; SProgram78 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead Just ), entity Term
; SProgram79 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead Just *, entity Term
; SProgram79 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead Just +, entity Term
; SProgram79 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead Just ,, entity Term
; SProgram79 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead Just ), entity Term
; SProgram80 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead Just *, entity Term
; SProgram80 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead Just +, entity Term
; SProgram80 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead Just ,, entity Term
; SProgram80 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead Just =, entity Term
; SProgram80 ((__p,  "=") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action70 __pos e) __stk
-- lookahead Just ), entity Term
; SProgram81 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead Just *, entity Term
; SProgram81 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead Just +, entity Term
; SProgram81 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead Just =, entity Term
; SProgram81 ((__p,  "=") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action70 __pos e) __stk
-- lookahead Just +, entity Add
; SProgram82 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead Just ,, entity Add
; SProgram82 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead Just ., entity Add
; SProgram82 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action63 __pos a) __stk
-- lookahead Just +, entity Add
; SProgram83 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead Just ,, entity Add
; SProgram83 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead Just ., entity Add
; SProgram83 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action63 __pos a) __stk
-- lookahead Just =, entity Add
; SProgram83 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action63 __pos a) __stk
-- lookahead Just +, entity Add
; SProgram84 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead Just ,, entity Add
; SProgram84 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead Just ., entity Add
; SProgram84 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action63 __pos a) __stk
-- lookahead Just =, entity Add
; SProgram84 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action63 __pos a) __stk
-- lookahead Just =>, entity Add
; SProgram84 ((__p,  "=>") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "=>") : __input, __end) (action63 __pos a) __stk
-- lookahead Just +, entity Add
; SProgram85 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead Just ,, entity Add
; SProgram85 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead Just ., entity Add
; SProgram85 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action63 __pos a) __stk
-- lookahead Just =>, entity Add
; SProgram85 ((__p,  "=>") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForProgram ((__p,  "=>") : __input, __end) (action63 __pos a) __stk
-- lookahead Just +, entity Add
; SProgram86 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just ,, entity Add
; SProgram86 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just ., entity Add
; SProgram86 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just +, entity Add
; SProgram87 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just ,, entity Add
; SProgram87 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just ., entity Add
; SProgram87 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just =, entity Add
; SProgram87 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just +, entity Add
; SProgram88 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just ,, entity Add
; SProgram88 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just ., entity Add
; SProgram88 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just =, entity Add
; SProgram88 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "=") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just =>, entity Add
; SProgram88 ((__p,  "=>") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "=>") : __input, __end) (action62 __pos a
                                                                        b) __stk
-- lookahead Just +, entity Add
; SProgram89 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "+") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just ,, entity Add
; SProgram89 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ",") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just ., entity Add
; SProgram89 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  ".") : __input, __end) (action62 __pos a
                                                                       b) __stk
-- lookahead Just =>, entity Add
; SProgram89 ((__p,  "=>") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForProgram ((__p,  "=>") : __input, __end) (action62 __pos a
                                                                        b) __stk
-- lookahead Just *, entity Mult
; SProgram90 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead Just +, entity Mult
; SProgram90 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead Just ,, entity Mult
; SProgram90 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead Just ., entity Mult
; SProgram90 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action67 __pos a) __stk
-- lookahead Just *, entity Mult
; SProgram91 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead Just +, entity Mult
; SProgram91 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead Just ,, entity Mult
; SProgram91 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead Just ., entity Mult
; SProgram91 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action67 __pos a) __stk
-- lookahead Just =, entity Mult
; SProgram91 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action67 __pos a) __stk
-- lookahead Just *, entity Mult
; SProgram92 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead Just +, entity Mult
; SProgram92 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead Just ,, entity Mult
; SProgram92 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead Just ., entity Mult
; SProgram92 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action67 __pos a) __stk
-- lookahead Just =, entity Mult
; SProgram92 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action67 __pos a) __stk
-- lookahead Just =>, entity Mult
; SProgram92 ((__p,  "=>") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "=>") : __input, __end) (action67 __pos a) __stk
-- lookahead Just *, entity Mult
; SProgram93 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead Just +, entity Mult
; SProgram93 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead Just ,, entity Mult
; SProgram93 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead Just ., entity Mult
; SProgram93 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action67 __pos a) __stk
-- lookahead Just =>, entity Mult
; SProgram93 ((__p,  "=>") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForProgram ((__p,  "=>") : __input, __end) (action67 __pos a) __stk
-- lookahead Just *, entity Term
; SProgram98 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead Just +, entity Term
; SProgram98 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead Just ,, entity Term
; SProgram98 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead Just ., entity Term
; SProgram98 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action71 __pos n) __stk
-- lookahead Just *, entity Term
; SProgram99 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead Just +, entity Term
; SProgram99 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead Just ,, entity Term
; SProgram99 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead Just ., entity Term
; SProgram99 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action71 __pos n) __stk
-- lookahead Just =, entity Term
; SProgram99 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action71 __pos n) __stk
-- lookahead Just *, entity Term
; SProgram100 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead Just +, entity Term
; SProgram100 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead Just ,, entity Term
; SProgram100 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead Just ., entity Term
; SProgram100 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action71 __pos n) __stk
-- lookahead Just =, entity Term
; SProgram100 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action71 __pos n) __stk
-- lookahead Just =>, entity Term
; SProgram100 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=>") : __input, __end) (action71 __pos n) __stk
-- lookahead Just *, entity Term
; SProgram101 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead Just +, entity Term
; SProgram101 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead Just ,, entity Term
; SProgram101 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead Just ., entity Term
; SProgram101 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action71 __pos n) __stk
-- lookahead Just =>, entity Term
; SProgram101 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=>") : __input, __end) (action71 __pos n) __stk
-- lookahead Just *, entity Term
; SProgram102 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead Just +, entity Term
; SProgram102 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead Just ,, entity Term
; SProgram102 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead Just ., entity Term
; SProgram102 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action72 __pos n) __stk
-- lookahead Just *, entity Term
; SProgram103 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead Just +, entity Term
; SProgram103 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead Just ,, entity Term
; SProgram103 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead Just ., entity Term
; SProgram103 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action72 __pos n) __stk
-- lookahead Just =, entity Term
; SProgram103 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action72 __pos n) __stk
-- lookahead Just *, entity Term
; SProgram104 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead Just +, entity Term
; SProgram104 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead Just ,, entity Term
; SProgram104 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead Just ., entity Term
; SProgram104 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action72 __pos n) __stk
-- lookahead Just =, entity Term
; SProgram104 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action72 __pos n) __stk
-- lookahead Just =>, entity Term
; SProgram104 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=>") : __input, __end) (action72 __pos n) __stk
-- lookahead Just *, entity Term
; SProgram105 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead Just +, entity Term
; SProgram105 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead Just ,, entity Term
; SProgram105 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead Just ., entity Term
; SProgram105 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action72 __pos n) __stk
-- lookahead Just =>, entity Term
; SProgram105 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForProgram ((__p,  "=>") : __input, __end) (action72 __pos n) __stk
-- lookahead Just *, entity Const
; SProgram106 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead Just +, entity Const
; SProgram106 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ,, entity Const
; SProgram106 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ., entity Const
; SProgram106 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action75 __pos n) __stk
-- lookahead Just =, entity Const
; SProgram106 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead Just *, entity Const
; SProgram107 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead Just +, entity Const
; SProgram107 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ,, entity Const
; SProgram107 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ., entity Const
; SProgram107 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action75 __pos n) __stk
-- lookahead Just =, entity Const
; SProgram107 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead Just =>, entity Const
; SProgram107 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=>") : __input, __end) (action75 __pos n) __stk
-- lookahead Just *, entity Const
; SProgram108 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead Just +, entity Const
; SProgram108 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ,, entity Const
; SProgram108 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ., entity Const
; SProgram108 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action75 __pos n) __stk
-- lookahead Just *, entity Const
; SProgram109 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead Just +, entity Const
; SProgram109 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ,, entity Const
; SProgram109 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ., entity Const
; SProgram109 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action75 __pos n) __stk
-- lookahead Just =, entity Const
; SProgram109 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead Just *, entity Const
; SProgram110 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead Just +, entity Const
; SProgram110 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ,, entity Const
; SProgram110 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ., entity Const
; SProgram110 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action75 __pos n) __stk
-- lookahead Just =, entity Const
; SProgram110 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead Just =>, entity Const
; SProgram110 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=>") : __input, __end) (action75 __pos n) __stk
-- lookahead Just *, entity Const
; SProgram111 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead Just +, entity Const
; SProgram111 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ,, entity Const
; SProgram111 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ., entity Const
; SProgram111 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action75 __pos n) __stk
-- lookahead Just =>, entity Const
; SProgram111 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=>") : __input, __end) (action75 __pos n) __stk
-- lookahead Just *, entity Const
; SProgram112 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead Just +, entity Const
; SProgram112 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead Just ,, entity Const
; SProgram112 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead Just ., entity Const
; SProgram112 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action76 __pos n) __stk
-- lookahead Just *, entity Const
; SProgram113 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead Just +, entity Const
; SProgram113 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead Just ,, entity Const
; SProgram113 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead Just ., entity Const
; SProgram113 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action76 __pos n) __stk
-- lookahead Just =, entity Const
; SProgram113 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action76 __pos n) __stk
-- lookahead Just *, entity Const
; SProgram114 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead Just +, entity Const
; SProgram114 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead Just ,, entity Const
; SProgram114 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead Just ., entity Const
; SProgram114 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action76 __pos n) __stk
-- lookahead Just =, entity Const
; SProgram114 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=") : __input, __end) (action76 __pos n) __stk
-- lookahead Just =>, entity Const
; SProgram114 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=>") : __input, __end) (action76 __pos n) __stk
-- lookahead Just *, entity Const
; SProgram115 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead Just +, entity Const
; SProgram115 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead Just ,, entity Const
; SProgram115 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead Just ., entity Const
; SProgram115 ((__p,  ".") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  ".") : __input, __end) (action76 __pos n) __stk
-- lookahead Just =>, entity Const
; SProgram115 ((__p,  "=>") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForProgram ((__p,  "=>") : __input, __end) (action76 __pos n) __stk
-- lookahead Just *, entity Mult
; SProgram124 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just +, entity Mult
; SProgram124 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just ,, entity Mult
; SProgram124 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just ., entity Mult
; SProgram124 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just *, entity Mult
; SProgram125 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just +, entity Mult
; SProgram125 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just ,, entity Mult
; SProgram125 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just ., entity Mult
; SProgram125 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just =, entity Mult
; SProgram125 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just *, entity Mult
; SProgram126 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just +, entity Mult
; SProgram126 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just ,, entity Mult
; SProgram126 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just ., entity Mult
; SProgram126 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just =, entity Mult
; SProgram126 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "=") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just =>, entity Mult
; SProgram126 ((__p,  "=>") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "=>") : __input, __end) (action66 __pos a
                                                                         b) __stk
-- lookahead Just *, entity Mult
; SProgram127 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "*") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just +, entity Mult
; SProgram127 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "+") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just ,, entity Mult
; SProgram127 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ",") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just ., entity Mult
; SProgram127 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  ".") : __input, __end) (action66 __pos a
                                                                        b) __stk
-- lookahead Just =>, entity Mult
; SProgram127 ((__p,  "=>") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForProgram ((__p,  "=>") : __input, __end) (action66 __pos a
                                                                         b) __stk
-- lookahead Just *, entity Term
; SProgram128 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead Just +, entity Term
; SProgram128 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead Just ,, entity Term
; SProgram128 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead Just ., entity Term
; SProgram128 ((__p,  ".") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action70 __pos e) __stk
-- lookahead Just *, entity Term
; SProgram129 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead Just +, entity Term
; SProgram129 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead Just ,, entity Term
; SProgram129 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead Just ., entity Term
; SProgram129 ((__p,  ".") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action70 __pos e) __stk
-- lookahead Just =, entity Term
; SProgram129 ((__p,  "=") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action70 __pos e) __stk
-- lookahead Just *, entity Term
; SProgram130 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead Just +, entity Term
; SProgram130 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead Just ,, entity Term
; SProgram130 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead Just ., entity Term
; SProgram130 ((__p,  ".") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action70 __pos e) __stk
-- lookahead Just =, entity Term
; SProgram130 ((__p,  "=") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "=") : __input, __end) (action70 __pos e) __stk
-- lookahead Just =>, entity Term
; SProgram130 ((__p,  "=>") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "=>") : __input, __end) (action70 __pos e) __stk
-- lookahead Just *, entity Term
; SProgram131 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead Just +, entity Term
; SProgram131 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead Just ,, entity Term
; SProgram131 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead Just ., entity Term
; SProgram131 ((__p,  ".") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  ".") : __input, __end) (action70 __pos e) __stk
-- lookahead Just =>, entity Term
; SProgram131 ((__p,  "=>") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForProgram ((__p,  "=>") : __input, __end) (action70 __pos e) __stk
-- lookahead Just ,, entity Expr
; SProgram132 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  ",") : __input, __end) (action59 __pos a) __stk
-- lookahead Just ., entity Expr
; SProgram132 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  ".") : __input, __end) (action59 __pos a) __stk
-- lookahead Just ,, entity Expr
; SProgram133 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  ",") : __input, __end) (action59 __pos a) __stk
-- lookahead Just ., entity Expr
; SProgram133 ((__p,  ".") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  ".") : __input, __end) (action59 __pos a) __stk
-- lookahead Just =>, entity Expr
; SProgram133 ((__p,  "=>") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForProgram ((__p,  "=>") : __input, __end) (action59 __pos a) __stk
-- lookahead Just ,, entity Expr
; SProgram134 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  ",") : __input, __end) (action58 __pos a
                                                                        b) __stk
-- lookahead Just ., entity Expr
; SProgram134 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  ".") : __input, __end) (action58 __pos a
                                                                        b) __stk
-- lookahead Just ,, entity Expr
; SProgram135 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  ",") : __input, __end) (action58 __pos a
                                                                        b) __stk
-- lookahead Just ., entity Expr
; SProgram135 ((__p,  ".") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  ".") : __input, __end) (action58 __pos a
                                                                        b) __stk
-- lookahead Just =>, entity Expr
; SProgram135 ((__p,  "=>") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForProgram ((__p,  "=>") : __input, __end) (action58 __pos a
                                                                         b) __stk
-- lookahead Just ,, entity Cond
; SProgram142 ((__p,  ",") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ",") : __input, __end) (action42 __pos c) __stk
-- lookahead Just ., entity Cond
; SProgram142 ((__p,  ".") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ".") : __input, __end) (action42 __pos c) __stk
-- lookahead Just ,, entity Cond
; SProgram143 ((__p,  ",") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ",") : __input, __end) (action42 __pos c) __stk
-- lookahead Just ., entity Cond
; SProgram143 ((__p,  ".") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ".") : __input, __end) (action42 __pos c) __stk
-- lookahead Just =>, entity Cond
; SProgram143 ((__p,  "=>") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  "=>") : __input, __end) (action42 __pos c) __stk
-- lookahead Just ,, entity Cond
; SProgram146 ((__p,  ",") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ",") : __input, __end) (action44 __pos e) __stk
-- lookahead Just ., entity Cond
; SProgram146 ((__p,  ".") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ".") : __input, __end) (action44 __pos e) __stk
-- lookahead Just ,, entity Cond
; SProgram147 ((__p,  ",") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ",") : __input, __end) (action44 __pos e) __stk
-- lookahead Just ., entity Cond
; SProgram147 ((__p,  ".") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  ".") : __input, __end) (action44 __pos e) __stk
-- lookahead Just =>, entity Cond
; SProgram147 ((__p,  "=>") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoCondForProgram ((__p,  "=>") : __input, __end) (action44 __pos e) __stk
-- lookahead Just ,, entity Change
; SProgram152 ((__p,  ",") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoChangeForProgram ((__p,  ",") : __input, __end) (action34 __pos c) __stk
-- lookahead Just ., entity Change
; SProgram152 ((__p,  ".") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoChangeForProgram ((__p,  ".") : __input, __end) (action34 __pos c) __stk
-- lookahead Just ,, entity Change
; SProgram153 ((__p,  ",") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoChangeForProgram ((__p,  ",") : __input, __end) (action35 __pos c) __stk
-- lookahead Just ., entity Change
; SProgram153 ((__p,  ".") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoChangeForProgram ((__p,  ".") : __input, __end) (action35 __pos c) __stk
-- lookahead Just ,, entity Cond
; SProgram154 ((__p,  ",") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoCondForProgram ((__p,  ",") : __input, __end) (action43 __pos c) __stk
-- lookahead Just ., entity Cond
; SProgram154 ((__p,  ".") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoCondForProgram ((__p,  ".") : __input, __end) (action43 __pos c) __stk
-- lookahead Just ,, entity Cond
; SProgram155 ((__p,  ",") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoCondForProgram ((__p,  ",") : __input, __end) (action43 __pos c) __stk
-- lookahead Just ., entity Cond
; SProgram155 ((__p,  ".") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoCondForProgram ((__p,  ".") : __input, __end) (action43 __pos c) __stk
-- lookahead Just =>, entity Cond
; SProgram155 ((__p,  "=>") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoCondForProgram ((__p,  "=>") : __input, __end) (action43 __pos c) __stk
-- lookahead Just ,, entity Call
; SProgram156 ((__p,  ",") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  ",") : __input, __end) (action47 __pos pre
                                                                        t) __stk
-- lookahead Just ., entity Call
; SProgram156 ((__p,  ".") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  ".") : __input, __end) (action47 __pos pre
                                                                        t) __stk
-- lookahead Just ,, entity Call
; SProgram157 ((__p,  ",") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  ",") : __input, __end) (action47 __pos pre
                                                                        t) __stk
-- lookahead Just ., entity Call
; SProgram157 ((__p,  ".") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  ".") : __input, __end) (action47 __pos pre
                                                                        t) __stk
-- lookahead Just =>, entity Call
; SProgram157 ((__p,  "=>") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  "=>") : __input, __end) (action47 __pos pre
                                                                         t) __stk
-- lookahead Just ,, entity Tuple
; SProgram158 ((__p,  ",") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  ",") : __input, __end) (action50 __pos ) __stk
-- lookahead Just ., entity Tuple
; SProgram158 ((__p,  ".") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  ".") : __input, __end) (action50 __pos ) __stk
-- lookahead Just ,, entity Tuple
; SProgram159 ((__p,  ",") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  ",") : __input, __end) (action50 __pos ) __stk
-- lookahead Just ., entity Tuple
; SProgram159 ((__p,  ".") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  ".") : __input, __end) (action50 __pos ) __stk
-- lookahead Just =>, entity Tuple
; SProgram159 ((__p,  "=>") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  "=>") : __input, __end) (action50 __pos ) __stk
-- lookahead Just ,, entity Tuple
; SProgram164 ((__p,  ",") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  ",") : __input, __end) (action51 __pos es) __stk
-- lookahead Just ., entity Tuple
; SProgram164 ((__p,  ".") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  ".") : __input, __end) (action51 __pos es) __stk
-- lookahead Just ,, entity Tuple
; SProgram165 ((__p,  ",") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  ",") : __input, __end) (action51 __pos es) __stk
-- lookahead Just ., entity Tuple
; SProgram165 ((__p,  ".") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  ".") : __input, __end) (action51 __pos es) __stk
-- lookahead Just =>, entity Tuple
; SProgram165 ((__p,  "=>") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  "=>") : __input, __end) (action51 __pos es) __stk
-- lookahead Just ->, entity Call
; SProgram168 ((__p,  "->") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  "->") : __input, __end) (action47 __pos pre
                                                                         t) __stk
-- lookahead Just ., entity Call
; SProgram168 ((__p,  ".") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  ".") : __input, __end) (action47 __pos pre
                                                                        t) __stk
-- lookahead Just <-, entity Call
; SProgram168 ((__p,  "<-") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  "<-") : __input, __end) (action47 __pos pre
                                                                         t) __stk
-- lookahead Just =>, entity Call
; SProgram168 ((__p,  "=>") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForProgram ((__p,  "=>") : __input, __end) (action47 __pos pre
                                                                         t) __stk
-- lookahead Just ->, entity Tuple
; SProgram169 ((__p,  "->") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  "->") : __input, __end) (action50 __pos ) __stk
-- lookahead Just ., entity Tuple
; SProgram169 ((__p,  ".") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  ".") : __input, __end) (action50 __pos ) __stk
-- lookahead Just <-, entity Tuple
; SProgram169 ((__p,  "<-") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  "<-") : __input, __end) (action50 __pos ) __stk
-- lookahead Just =>, entity Tuple
; SProgram169 ((__p,  "=>") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForProgram ((__p,  "=>") : __input, __end) (action50 __pos ) __stk
-- lookahead Just ->, entity Tuple
; SProgram171 ((__p,  "->") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  "->") : __input, __end) (action51 __pos es) __stk
-- lookahead Just ., entity Tuple
; SProgram171 ((__p,  ".") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  ".") : __input, __end) (action51 __pos es) __stk
-- lookahead Just <-, entity Tuple
; SProgram171 ((__p,  "<-") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  "<-") : __input, __end) (action51 __pos es) __stk
-- lookahead Just =>, entity Tuple
; SProgram171 ((__p,  "=>") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForProgram ((__p,  "=>") : __input, __end) (action51 __pos es) __stk
-- lookahead Just ., entity Changes
; SProgram172 ((__p,  ".") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoChangesForProgram ((__p,  ".") : __input, __end) (action31 __pos c) __stk
-- lookahead Just ., entity Conds
; SProgram173 ((__p,  ".") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondsForProgram ((__p,  ".") : __input, __end) (action39 __pos c) __stk
-- lookahead Just ., entity Conds
; SProgram174 ((__p,  ".") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondsForProgram ((__p,  ".") : __input, __end) (action39 __pos c) __stk
-- lookahead Just =>, entity Conds
; SProgram174 ((__p,  "=>") : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoCondsForProgram ((__p,  "=>") : __input, __end) (action39 __pos c) __stk
-- lookahead Just ., entity Changes
; SProgram178 ((__p,  ".") : __input, __end) ((cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _))))) ->
    __gotoChangesForProgram ((__p,  ".") : __input, __end) (action30 __pos c
                                                                           cs) __stk
-- lookahead Just ., entity Conds
; SProgram179 ((__p,  ".") : __input, __end) ((cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _))))) ->
    __gotoCondsForProgram ((__p,  ".") : __input, __end) (action38 __pos c
                                                                         cs) __stk
-- lookahead Just ., entity Conds
; SProgram180 ((__p,  ".") : __input, __end) ((cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _))))) ->
    __gotoCondsForProgram ((__p,  ".") : __input, __end) (action38 __pos c
                                                                         cs) __stk
-- lookahead Just =>, entity Conds
; SProgram180 ((__p,  "=>") : __input, __end) ((cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _))))) ->
    __gotoCondsForProgram ((__p,  "=>") : __input, __end) (action38 __pos c
                                                                          cs) __stk
; SProgram0 __input _ -> Left  (currentPos __input, ["<name>"])
; SProgram1 __input _ -> Left  (currentPos __input, ["<eof>"])
; SProgram2 __input _ -> Left  (currentPos __input, ["<eof>"])
; SProgram3 __input _ ->
    Left  (currentPos __input, ["<eof>", "<name>"])
; SProgram4 __input _ ->
    Left  (currentPos __input, ["<eof>", "<name>"])
; SProgram5 __input _ ->
    Left  (currentPos __input, ["<eof>", "<name>"])
; SProgram6 __input _ ->
    Left  (currentPos __input, ["->", ".", "<-", "=>"])
; SProgram7 __input _ -> Left  (currentPos __input, ["<eof>"])
; SProgram8 __input _ -> Left  (currentPos __input, ["+", "-"])
; SProgram9 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>", "~"])
; SProgram10 __input _ ->
    Left  (currentPos __input, ["<eof>", "<name>"])
; SProgram11 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>", "~"])
; SProgram12 __input _ -> Left  (currentPos __input, ["."])
; SProgram13 __input _ -> Left  (currentPos __input, [".", "=>"])
; SProgram14 __input _ -> Left  (currentPos __input, ["."])
; SProgram15 __input _ ->
    Left  (currentPos __input, ["<eof>", "<name>"])
; SProgram16 __input _ -> Left  (currentPos __input, ["+", "-"])
; SProgram17 __input _ ->
    Left  (currentPos __input, ["<eof>", "<name>"])
; SProgram18 __input _ ->
    Left  (currentPos __input, ["<eof>", "<name>"])
; SProgram19 __input _ -> Left  (currentPos __input, ["."])
; SProgram20 __input _ ->
    Left  (currentPos __input, ["<eof>", "<name>"])
; SProgram21 __input _ -> Left  (currentPos __input, [")", ","])
; SProgram22 __input _ ->
    Left  (currentPos __input, [")", "+", ",", "="])
; SProgram23 __input _ ->
    Left  (currentPos __input, [")", "+", "="])
; SProgram24 __input _ -> Left  (currentPos __input, [")", "+"])
; SProgram25 __input _ ->
    Left  (currentPos __input, [")", "+", ","])
; SProgram26 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram27 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram28 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram29 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram30 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram31 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram32 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram33 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram34 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram35 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram36 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram37 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram38 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram39 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram40 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram41 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram42 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram43 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram44 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram45 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram46 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram47 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram48 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram49 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram50 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram51 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram52 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram53 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram54 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram55 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram56 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram57 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram58 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram59 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram60 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram61 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram62 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram63 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram64 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram65 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram66 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram67 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram68 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram69 __input _ -> Left  (currentPos __input, [")"])
; SProgram70 __input _ -> Left  (currentPos __input, [")"])
; SProgram71 __input _ -> Left  (currentPos __input, [")"])
; SProgram72 __input _ -> Left  (currentPos __input, [")"])
; SProgram73 __input _ -> Left  (currentPos __input, [")"])
; SProgram74 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram75 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram76 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram77 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; SProgram78 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; SProgram79 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; SProgram80 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; SProgram81 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
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
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram95 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram96 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram97 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram98 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram99 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram100 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram101 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram102 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram103 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram104 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram105 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram106 __input _ ->
    Left  (currentPos __input, ["(", "*", "+", ",", ".", "="])
; SProgram107 __input _ ->
    Left  (currentPos __input, ["(", "*", "+", ",", ".", "=", "=>"])
; SProgram108 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram109 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram110 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram111 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram112 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram113 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram114 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram115 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram116 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram117 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram118 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram119 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram120 __input _ -> Left  (currentPos __input, [")"])
; SProgram121 __input _ -> Left  (currentPos __input, [")"])
; SProgram122 __input _ -> Left  (currentPos __input, [")"])
; SProgram123 __input _ -> Left  (currentPos __input, [")"])
; SProgram124 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram125 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram126 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram127 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram128 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; SProgram129 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; SProgram130 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; SProgram131 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; SProgram132 __input _ ->
    Left  (currentPos __input, ["+", ",", ".", "="])
; SProgram133 __input _ ->
    Left  (currentPos __input, ["+", ",", ".", "=", "=>"])
; SProgram134 __input _ ->
    Left  (currentPos __input, ["+", ",", "."])
; SProgram135 __input _ ->
    Left  (currentPos __input, ["+", ",", ".", "=>"])
; SProgram136 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram137 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram138 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram139 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram140 __input _ -> Left  (currentPos __input, ["<name>"])
; SProgram141 __input _ -> Left  (currentPos __input, ["<name>"])
; SProgram142 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram143 __input _ ->
    Left  (currentPos __input, [",", ".", "=>"])
; SProgram144 __input _ -> Left  (currentPos __input, ["<name>"])
; SProgram145 __input _ -> Left  (currentPos __input, ["<name>"])
; SProgram146 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram147 __input _ ->
    Left  (currentPos __input, [",", ".", "=>"])
; SProgram148 __input _ -> Left  (currentPos __input, ["("])
; SProgram149 __input _ -> Left  (currentPos __input, ["("])
; SProgram150 __input _ ->
    Left  (currentPos __input, ["(", ")", "<Name>", "<name>", "<num>"])
; SProgram151 __input _ ->
    Left  (currentPos __input, ["(", ")", "<Name>", "<name>", "<num>"])
; SProgram152 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram153 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram154 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram155 __input _ ->
    Left  (currentPos __input, [",", ".", "=>"])
; SProgram156 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram157 __input _ ->
    Left  (currentPos __input, [",", ".", "=>"])
; SProgram158 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram159 __input _ ->
    Left  (currentPos __input, [",", ".", "=>"])
; SProgram160 __input _ -> Left  (currentPos __input, [")"])
; SProgram161 __input _ -> Left  (currentPos __input, [")"])
; SProgram162 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram163 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; SProgram164 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram165 __input _ ->
    Left  (currentPos __input, [",", ".", "=>"])
; SProgram166 __input _ -> Left  (currentPos __input, ["("])
; SProgram167 __input _ ->
    Left  (currentPos __input, ["(", ")", "<Name>", "<name>", "<num>"])
; SProgram168 __input _ ->
    Left  (currentPos __input, ["->", ".", "<-", "=>"])
; SProgram169 __input _ ->
    Left  (currentPos __input, ["->", ".", "<-", "=>"])
; SProgram170 __input _ -> Left  (currentPos __input, [")"])
; SProgram171 __input _ ->
    Left  (currentPos __input, ["->", ".", "<-", "=>"])
; SProgram172 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram173 __input _ -> Left  (currentPos __input, [",", "."])
; SProgram174 __input _ ->
    Left  (currentPos __input, [",", ".", "=>"])
; SProgram175 __input _ -> Left  (currentPos __input, ["+", "-"])
; SProgram176 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>", "~"])
; SProgram177 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>", "~"])
; SProgram178 __input _ -> Left  (currentPos __input, ["."])
; SProgram179 __input _ -> Left  (currentPos __input, ["."])
; SProgram180 __input _ -> Left  (currentPos __input, [".", "=>"])
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
    Right input -> pure (Right (__runProgram SProgram0 input Nil))
data StTestSuite :: [Kind.Type] -> Kind.Type where
  STestSuite0 :: StTestSuite (a)
  STestSuite1 :: StTestSuite (TestSuite : a)
  STestSuite2 :: StTestSuite (Text : a)
  STestSuite3 :: StTestSuite (() : a)
  STestSuite4 :: StTestSuite (Expr : a)
  STestSuite5 :: StTestSuite (Expr : () : Expr : a)
  STestSuite6 :: StTestSuite (Expr : a)
  STestSuite7 :: StTestSuite (Expr : a)
  STestSuite8 :: StTestSuite (Expr : () : Expr : a)
  STestSuite9 :: StTestSuite (Expr : () : Expr : a)
  STestSuite10 :: StTestSuite (Expr : a)
  STestSuite11 :: StTestSuite (Expr : a)
  STestSuite12 :: StTestSuite (() : a)
  STestSuite13 :: StTestSuite (() : a)
  STestSuite14 :: StTestSuite (Text : a)
  STestSuite15 :: StTestSuite (Text : a)
  STestSuite16 :: StTestSuite (Const : a)
  STestSuite17 :: StTestSuite (Const : a)
  STestSuite18 :: StTestSuite (Text : a)
  STestSuite19 :: StTestSuite (Text : a)
  STestSuite20 :: StTestSuite (Integer : a)
  STestSuite21 :: StTestSuite (Integer : a)
  STestSuite22 :: StTestSuite (() : a)
  STestSuite23 :: StTestSuite (() : a)
  STestSuite24 :: StTestSuite (() : a)
  STestSuite25 :: StTestSuite (() : a)
  STestSuite26 :: StTestSuite (Test : a)
  STestSuite27 :: StTestSuite ([Expr] : Text : a)
  STestSuite28 :: StTestSuite (() : () : a)
  STestSuite29 :: StTestSuite ([Expr] : () : a)
  STestSuite30 :: StTestSuite (() : Expr : a)
  STestSuite31 :: StTestSuite (() : Expr : a)
  STestSuite32 :: StTestSuite (() : Expr : a)
  STestSuite33 :: StTestSuite (() : Expr : a)
  STestSuite34 :: StTestSuite (() : Expr : a)
  STestSuite35 :: StTestSuite (Expr : () : a)
  STestSuite36 :: StTestSuite (Expr : () : a)
  STestSuite37 :: StTestSuite (Call : () : a)
  STestSuite38 :: StTestSuite (Call : () : a)
  STestSuite39 :: StTestSuite (Expr : () : a)
  STestSuite40 :: StTestSuite ([Test] : () : a)
  STestSuite41 :: StTestSuite ([Test] : Test : a)
  STestSuite42 :: StTestSuite (() : [Expr] : () : a)
  STestSuite43 :: StTestSuite (Expr : () : Expr : a)
  STestSuite44 :: StTestSuite (Expr : () : Expr : a)
  STestSuite45 :: StTestSuite (() : Expr : () : a)
  STestSuite46 :: StTestSuite (() : Expr : () : a)
  STestSuite47 :: StTestSuite (Expr : a)
  STestSuite48 :: StTestSuite (Expr : a)
  STestSuite49 :: StTestSuite (Expr : a)
  STestSuite50 :: StTestSuite (Expr : () : Expr : a)
  STestSuite51 :: StTestSuite (Expr : () : Expr : a)
  STestSuite52 :: StTestSuite (Expr : a)
  STestSuite53 :: StTestSuite (Expr : a)
  STestSuite54 :: StTestSuite (Expr : a)
  STestSuite55 :: StTestSuite (Expr : a)
  STestSuite56 :: StTestSuite (Expr : () : Expr : a)
  STestSuite57 :: StTestSuite (Expr : () : Expr : a)
  STestSuite58 :: StTestSuite (Expr : () : Expr : a)
  STestSuite59 :: StTestSuite (Expr : () : Expr : a)
  STestSuite60 :: StTestSuite (Expr : a)
  STestSuite61 :: StTestSuite (Expr : a)
  STestSuite62 :: StTestSuite (Expr : a)
  STestSuite63 :: StTestSuite (Expr : a)
  STestSuite64 :: StTestSuite (() : a)
  STestSuite65 :: StTestSuite (() : a)
  STestSuite66 :: StTestSuite (() : a)
  STestSuite67 :: StTestSuite (() : a)
  STestSuite68 :: StTestSuite (Text : a)
  STestSuite69 :: StTestSuite (Text : a)
  STestSuite70 :: StTestSuite (Text : a)
  STestSuite71 :: StTestSuite (Text : a)
  STestSuite72 :: StTestSuite (Const : a)
  STestSuite73 :: StTestSuite (Const : a)
  STestSuite74 :: StTestSuite (Const : a)
  STestSuite75 :: StTestSuite (Const : a)
  STestSuite76 :: StTestSuite (Text : a)
  STestSuite77 :: StTestSuite (Text : a)
  STestSuite78 :: StTestSuite (Text : a)
  STestSuite79 :: StTestSuite (Text : a)
  STestSuite80 :: StTestSuite (Integer : a)
  STestSuite81 :: StTestSuite (Integer : a)
  STestSuite82 :: StTestSuite (Integer : a)
  STestSuite83 :: StTestSuite (Integer : a)
  STestSuite84 :: StTestSuite (() : Expr : a)
  STestSuite85 :: StTestSuite (() : Expr : a)
  STestSuite86 :: StTestSuite (() : Expr : a)
  STestSuite87 :: StTestSuite (() : Expr : a)
  STestSuite88 :: StTestSuite (() : Expr : a)
  STestSuite89 :: StTestSuite (() : Expr : a)
  STestSuite90 :: StTestSuite (() : Expr : a)
  STestSuite91 :: StTestSuite (() : Expr : a)
  STestSuite92 :: StTestSuite (() : Expr : a)
  STestSuite93 :: StTestSuite (() : Expr : a)
  STestSuite94 :: StTestSuite (() : Expr : a)
  STestSuite95 :: StTestSuite (Expr : () : a)
  STestSuite96 :: StTestSuite (Expr : () : a)
  STestSuite97 :: StTestSuite (Expr : () : a)
  STestSuite98 :: StTestSuite (Expr : () : a)
  STestSuite99 :: StTestSuite ([Expr] : () : Expr : a)
  STestSuite100 :: StTestSuite (Expr : () : Expr : a)
  STestSuite101 :: StTestSuite (Expr : () : Expr : a)
  STestSuite102 :: StTestSuite (Expr : () : Expr : a)
  STestSuite103 :: StTestSuite (Expr : () : Expr : a)
  STestSuite104 :: StTestSuite (() : Expr : () : a)
  STestSuite105 :: StTestSuite (() : Expr : () : a)
  STestSuite106 :: StTestSuite (() : Expr : () : a)
  STestSuite107 :: StTestSuite (() : Expr : () : a)
  
__gotoAddForTestSuite :: ([Lexeme], Pos) -> Expr -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoAddForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite3 -> __runTestSuite STestSuite48 toks (term :> stk)
  STestSuite12 -> __runTestSuite STestSuite49 toks (term :> stk)
  STestSuite13 -> __runTestSuite STestSuite49 toks (term :> stk)
  STestSuite24 -> __runTestSuite STestSuite4 toks (term :> stk)
  STestSuite30 -> __runTestSuite STestSuite5 toks (term :> stk)
  STestSuite64 -> __runTestSuite STestSuite49 toks (term :> stk)
  STestSuite65 -> __runTestSuite STestSuite49 toks (term :> stk)
  STestSuite66 -> __runTestSuite STestSuite49 toks (term :> stk)
  STestSuite67 -> __runTestSuite STestSuite49 toks (term :> stk)
  STestSuite84 -> __runTestSuite STestSuite48 toks (term :> stk)
  STestSuite85 -> __runTestSuite STestSuite50 toks (term :> stk)
  STestSuite86 -> __runTestSuite STestSuite51 toks (term :> stk)
  _ -> error ""

__gotoCallForTestSuite :: ([Lexeme], Pos) -> Call -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoCallForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite22 -> __runTestSuite STestSuite37 toks (term :> stk)
  STestSuite23 -> __runTestSuite STestSuite38 toks (term :> stk)
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
  STestSuite3 -> __runTestSuite STestSuite74 toks (term :> stk)
  STestSuite12 -> __runTestSuite STestSuite75 toks (term :> stk)
  STestSuite13 -> __runTestSuite STestSuite75 toks (term :> stk)
  STestSuite24 -> __runTestSuite STestSuite16 toks (term :> stk)
  STestSuite30 -> __runTestSuite STestSuite17 toks (term :> stk)
  STestSuite31 -> __runTestSuite STestSuite16 toks (term :> stk)
  STestSuite32 -> __runTestSuite STestSuite17 toks (term :> stk)
  STestSuite33 -> __runTestSuite STestSuite16 toks (term :> stk)
  STestSuite34 -> __runTestSuite STestSuite17 toks (term :> stk)
  STestSuite64 -> __runTestSuite STestSuite75 toks (term :> stk)
  STestSuite65 -> __runTestSuite STestSuite75 toks (term :> stk)
  STestSuite66 -> __runTestSuite STestSuite75 toks (term :> stk)
  STestSuite67 -> __runTestSuite STestSuite75 toks (term :> stk)
  STestSuite84 -> __runTestSuite STestSuite74 toks (term :> stk)
  STestSuite85 -> __runTestSuite STestSuite72 toks (term :> stk)
  STestSuite86 -> __runTestSuite STestSuite73 toks (term :> stk)
  STestSuite87 -> __runTestSuite STestSuite72 toks (term :> stk)
  STestSuite88 -> __runTestSuite STestSuite73 toks (term :> stk)
  STestSuite89 -> __runTestSuite STestSuite74 toks (term :> stk)
  STestSuite90 -> __runTestSuite STestSuite75 toks (term :> stk)
  STestSuite91 -> __runTestSuite STestSuite72 toks (term :> stk)
  STestSuite92 -> __runTestSuite STestSuite73 toks (term :> stk)
  STestSuite93 -> __runTestSuite STestSuite74 toks (term :> stk)
  STestSuite94 -> __runTestSuite STestSuite75 toks (term :> stk)
  _ -> error ""

__gotoEffectForTestSuite :: ([Lexeme], Pos) -> Effect -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoEffectForTestSuite toks term stk@(state, _, _) = case state of
  _ -> error ""

__gotoExprForTestSuite :: ([Lexeme], Pos) -> Expr -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoExprForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite3 -> __runTestSuite STestSuite47 toks (term :> stk)
  STestSuite12 -> __runTestSuite STestSuite35 toks (term :> stk)
  STestSuite13 -> __runTestSuite STestSuite36 toks (term :> stk)
  STestSuite24 -> __runTestSuite STestSuite39 toks (term :> stk)
  STestSuite64 -> __runTestSuite STestSuite95 toks (term :> stk)
  STestSuite65 -> __runTestSuite STestSuite96 toks (term :> stk)
  STestSuite66 -> __runTestSuite STestSuite97 toks (term :> stk)
  STestSuite67 -> __runTestSuite STestSuite98 toks (term :> stk)
  STestSuite84 -> __runTestSuite STestSuite47 toks (term :> stk)
  _ -> error ""

__gotoExprs1ForTestSuite :: ([Lexeme], Pos) -> [Expr] -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoExprs1ForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite3 -> __runTestSuite STestSuite29 toks (term :> stk)
  STestSuite84 -> __runTestSuite STestSuite99 toks (term :> stk)
  _ -> error ""

__gotoMultForTestSuite :: ([Lexeme], Pos) -> Expr -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoMultForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite3 -> __runTestSuite STestSuite54 toks (term :> stk)
  STestSuite12 -> __runTestSuite STestSuite55 toks (term :> stk)
  STestSuite13 -> __runTestSuite STestSuite55 toks (term :> stk)
  STestSuite24 -> __runTestSuite STestSuite6 toks (term :> stk)
  STestSuite30 -> __runTestSuite STestSuite7 toks (term :> stk)
  STestSuite31 -> __runTestSuite STestSuite8 toks (term :> stk)
  STestSuite32 -> __runTestSuite STestSuite9 toks (term :> stk)
  STestSuite64 -> __runTestSuite STestSuite55 toks (term :> stk)
  STestSuite65 -> __runTestSuite STestSuite55 toks (term :> stk)
  STestSuite66 -> __runTestSuite STestSuite55 toks (term :> stk)
  STestSuite67 -> __runTestSuite STestSuite55 toks (term :> stk)
  STestSuite84 -> __runTestSuite STestSuite54 toks (term :> stk)
  STestSuite85 -> __runTestSuite STestSuite52 toks (term :> stk)
  STestSuite86 -> __runTestSuite STestSuite53 toks (term :> stk)
  STestSuite87 -> __runTestSuite STestSuite56 toks (term :> stk)
  STestSuite88 -> __runTestSuite STestSuite57 toks (term :> stk)
  STestSuite89 -> __runTestSuite STestSuite58 toks (term :> stk)
  STestSuite90 -> __runTestSuite STestSuite59 toks (term :> stk)
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
  STestSuite3 -> __runTestSuite STestSuite62 toks (term :> stk)
  STestSuite12 -> __runTestSuite STestSuite63 toks (term :> stk)
  STestSuite13 -> __runTestSuite STestSuite63 toks (term :> stk)
  STestSuite24 -> __runTestSuite STestSuite10 toks (term :> stk)
  STestSuite30 -> __runTestSuite STestSuite11 toks (term :> stk)
  STestSuite31 -> __runTestSuite STestSuite10 toks (term :> stk)
  STestSuite32 -> __runTestSuite STestSuite11 toks (term :> stk)
  STestSuite33 -> __runTestSuite STestSuite43 toks (term :> stk)
  STestSuite34 -> __runTestSuite STestSuite44 toks (term :> stk)
  STestSuite64 -> __runTestSuite STestSuite63 toks (term :> stk)
  STestSuite65 -> __runTestSuite STestSuite63 toks (term :> stk)
  STestSuite66 -> __runTestSuite STestSuite63 toks (term :> stk)
  STestSuite67 -> __runTestSuite STestSuite63 toks (term :> stk)
  STestSuite84 -> __runTestSuite STestSuite62 toks (term :> stk)
  STestSuite85 -> __runTestSuite STestSuite60 toks (term :> stk)
  STestSuite86 -> __runTestSuite STestSuite61 toks (term :> stk)
  STestSuite87 -> __runTestSuite STestSuite60 toks (term :> stk)
  STestSuite88 -> __runTestSuite STestSuite61 toks (term :> stk)
  STestSuite89 -> __runTestSuite STestSuite62 toks (term :> stk)
  STestSuite90 -> __runTestSuite STestSuite63 toks (term :> stk)
  STestSuite91 -> __runTestSuite STestSuite100 toks (term :> stk)
  STestSuite92 -> __runTestSuite STestSuite101 toks (term :> stk)
  STestSuite93 -> __runTestSuite STestSuite102 toks (term :> stk)
  STestSuite94 -> __runTestSuite STestSuite103 toks (term :> stk)
  _ -> error ""

__gotoTestForTestSuite :: ([Lexeme], Pos) -> Test -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoTestForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite25 -> __runTestSuite STestSuite26 toks (term :> stk)
  STestSuite26 -> __runTestSuite STestSuite26 toks (term :> stk)
  _ -> error ""

__gotoTestSuiteForTestSuite :: ([Lexeme], Pos) -> TestSuite -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoTestSuiteForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite0 -> __runTestSuite STestSuite1 toks (term :> stk)
  _ -> error ""

__gotoTestsForTestSuite :: ([Lexeme], Pos) -> [Test] -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoTestsForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite25 -> __runTestSuite STestSuite40 toks (term :> stk)
  STestSuite26 -> __runTestSuite STestSuite41 toks (term :> stk)
  _ -> error ""

__gotoTupleForTestSuite :: ([Lexeme], Pos) -> [Expr] -> Stack StTestSuite a -> Either (Pos, [String]) TestSuite
__gotoTupleForTestSuite toks term stk@(state, _, _) = case state of
  STestSuite2 -> __runTestSuite STestSuite27 toks (term :> stk)
  _ -> error ""
  
__runTestSuite :: StTestSuite a -> ([Lexeme], Pos) -> Stack' StTestSuite a -> Either (Pos, [String]) TestSuite
__runTestSuite = \cases {
; STestSuite0 ((__p,  "test") : __input, __end) __stk ->
    __runTestSuite STestSuite25 (__input, __end) (() :> (STestSuite0, __p, __stk))
; STestSuite2 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite3 (__input, __end) (() :> (STestSuite2, __p, __stk))
; STestSuite3 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite66 (__input, __end) (() :> (STestSuite3, __p, __stk))
; STestSuite3 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite28 (__input, __end) (() :> (STestSuite3, __p, __stk))
; STestSuite3 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite70 (__input, __end) (n :> (STestSuite3, __p, __stk))
; STestSuite3 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite78 (__input, __end) (n :> (STestSuite3, __p, __stk))
; STestSuite3 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite82 (__input, __end) (n :> (STestSuite3, __p, __stk))
; STestSuite4 ((__p,  "+") : __input, __end) __stk ->
    __runTestSuite STestSuite31 (__input, __end) (() :> (STestSuite4, __p, __stk))
; STestSuite4 ((__p,  "=") : __input, __end) __stk ->
    __runTestSuite STestSuite30 (__input, __end) (() :> (STestSuite4, __p, __stk))
; STestSuite5 ((__p,  "+") : __input, __end) __stk ->
    __runTestSuite STestSuite32 (__input, __end) (() :> (STestSuite5, __p, __stk))
; STestSuite6 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite33 (__input, __end) (() :> (STestSuite6, __p, __stk))
; STestSuite7 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite34 (__input, __end) (() :> (STestSuite7, __p, __stk))
; STestSuite8 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite33 (__input, __end) (() :> (STestSuite8, __p, __stk))
; STestSuite9 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite34 (__input, __end) (() :> (STestSuite9, __p, __stk))
; STestSuite12 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite67 (__input, __end) (() :> (STestSuite12, __p, __stk))
; STestSuite12 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite71 (__input, __end) (n :> (STestSuite12, __p, __stk))
; STestSuite12 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite79 (__input, __end) (n :> (STestSuite12, __p, __stk))
; STestSuite12 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite83 (__input, __end) (n :> (STestSuite12, __p, __stk))
; STestSuite13 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite67 (__input, __end) (() :> (STestSuite13, __p, __stk))
; STestSuite13 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite71 (__input, __end) (n :> (STestSuite13, __p, __stk))
; STestSuite13 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite79 (__input, __end) (n :> (STestSuite13, __p, __stk))
; STestSuite13 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite83 (__input, __end) (n :> (STestSuite13, __p, __stk))
; STestSuite22 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite2 (__input, __end) (n :> (STestSuite22, __p, __stk))
; STestSuite23 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite2 (__input, __end) (n :> (STestSuite23, __p, __stk))
; STestSuite24 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite12 (__input, __end) (() :> (STestSuite24, __p, __stk))
; STestSuite24 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite14 (__input, __end) (n :> (STestSuite24, __p, __stk))
; STestSuite24 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite18 (__input, __end) (n :> (STestSuite24, __p, __stk))
; STestSuite24 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite20 (__input, __end) (n :> (STestSuite24, __p, __stk))
; STestSuite25 ((__p,  "expect") : __input, __end) __stk ->
    __runTestSuite STestSuite22 (__input, __end) (() :> (STestSuite25, __p, __stk))
; STestSuite25 ((__p,  "guard") : __input, __end) __stk ->
    __runTestSuite STestSuite24 (__input, __end) (() :> (STestSuite25, __p, __stk))
; STestSuite25 ((__p,  "notify") : __input, __end) __stk ->
    __runTestSuite STestSuite23 (__input, __end) (() :> (STestSuite25, __p, __stk))
; STestSuite26 ((__p,  "expect") : __input, __end) __stk ->
    __runTestSuite STestSuite22 (__input, __end) (() :> (STestSuite26, __p, __stk))
; STestSuite26 ((__p,  "guard") : __input, __end) __stk ->
    __runTestSuite STestSuite24 (__input, __end) (() :> (STestSuite26, __p, __stk))
; STestSuite26 ((__p,  "notify") : __input, __end) __stk ->
    __runTestSuite STestSuite23 (__input, __end) (() :> (STestSuite26, __p, __stk))
; STestSuite29 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite42 (__input, __end) (() :> (STestSuite29, __p, __stk))
; STestSuite30 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite13 (__input, __end) (() :> (STestSuite30, __p, __stk))
; STestSuite30 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite15 (__input, __end) (n :> (STestSuite30, __p, __stk))
; STestSuite30 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite19 (__input, __end) (n :> (STestSuite30, __p, __stk))
; STestSuite30 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite21 (__input, __end) (n :> (STestSuite30, __p, __stk))
; STestSuite31 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite12 (__input, __end) (() :> (STestSuite31, __p, __stk))
; STestSuite31 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite14 (__input, __end) (n :> (STestSuite31, __p, __stk))
; STestSuite31 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite18 (__input, __end) (n :> (STestSuite31, __p, __stk))
; STestSuite31 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite20 (__input, __end) (n :> (STestSuite31, __p, __stk))
; STestSuite32 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite13 (__input, __end) (() :> (STestSuite32, __p, __stk))
; STestSuite32 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite15 (__input, __end) (n :> (STestSuite32, __p, __stk))
; STestSuite32 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite19 (__input, __end) (n :> (STestSuite32, __p, __stk))
; STestSuite32 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite21 (__input, __end) (n :> (STestSuite32, __p, __stk))
; STestSuite33 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite12 (__input, __end) (() :> (STestSuite33, __p, __stk))
; STestSuite33 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite14 (__input, __end) (n :> (STestSuite33, __p, __stk))
; STestSuite33 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite18 (__input, __end) (n :> (STestSuite33, __p, __stk))
; STestSuite33 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite20 (__input, __end) (n :> (STestSuite33, __p, __stk))
; STestSuite34 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite13 (__input, __end) (() :> (STestSuite34, __p, __stk))
; STestSuite34 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite15 (__input, __end) (n :> (STestSuite34, __p, __stk))
; STestSuite34 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite19 (__input, __end) (n :> (STestSuite34, __p, __stk))
; STestSuite34 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite21 (__input, __end) (n :> (STestSuite34, __p, __stk))
; STestSuite35 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite45 (__input, __end) (() :> (STestSuite35, __p, __stk))
; STestSuite36 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite46 (__input, __end) (() :> (STestSuite36, __p, __stk))
; STestSuite47 ((__p,  ",") : __input, __end) __stk ->
    __runTestSuite STestSuite84 (__input, __end) (() :> (STestSuite47, __p, __stk))
; STestSuite48 ((__p,  "+") : __input, __end) __stk ->
    __runTestSuite STestSuite89 (__input, __end) (() :> (STestSuite48, __p, __stk))
; STestSuite48 ((__p,  "=") : __input, __end) __stk ->
    __runTestSuite STestSuite86 (__input, __end) (() :> (STestSuite48, __p, __stk))
; STestSuite49 ((__p,  "+") : __input, __end) __stk ->
    __runTestSuite STestSuite90 (__input, __end) (() :> (STestSuite49, __p, __stk))
; STestSuite49 ((__p,  "=") : __input, __end) __stk ->
    __runTestSuite STestSuite85 (__input, __end) (() :> (STestSuite49, __p, __stk))
; STestSuite50 ((__p,  "+") : __input, __end) __stk ->
    __runTestSuite STestSuite87 (__input, __end) (() :> (STestSuite50, __p, __stk))
; STestSuite51 ((__p,  "+") : __input, __end) __stk ->
    __runTestSuite STestSuite88 (__input, __end) (() :> (STestSuite51, __p, __stk))
; STestSuite52 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite91 (__input, __end) (() :> (STestSuite52, __p, __stk))
; STestSuite53 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite92 (__input, __end) (() :> (STestSuite53, __p, __stk))
; STestSuite54 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite93 (__input, __end) (() :> (STestSuite54, __p, __stk))
; STestSuite55 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite94 (__input, __end) (() :> (STestSuite55, __p, __stk))
; STestSuite56 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite91 (__input, __end) (() :> (STestSuite56, __p, __stk))
; STestSuite57 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite92 (__input, __end) (() :> (STestSuite57, __p, __stk))
; STestSuite58 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite93 (__input, __end) (() :> (STestSuite58, __p, __stk))
; STestSuite59 ((__p,  "*") : __input, __end) __stk ->
    __runTestSuite STestSuite94 (__input, __end) (() :> (STestSuite59, __p, __stk))
; STestSuite64 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite67 (__input, __end) (() :> (STestSuite64, __p, __stk))
; STestSuite64 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite71 (__input, __end) (n :> (STestSuite64, __p, __stk))
; STestSuite64 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite79 (__input, __end) (n :> (STestSuite64, __p, __stk))
; STestSuite64 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite83 (__input, __end) (n :> (STestSuite64, __p, __stk))
; STestSuite65 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite67 (__input, __end) (() :> (STestSuite65, __p, __stk))
; STestSuite65 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite71 (__input, __end) (n :> (STestSuite65, __p, __stk))
; STestSuite65 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite79 (__input, __end) (n :> (STestSuite65, __p, __stk))
; STestSuite65 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite83 (__input, __end) (n :> (STestSuite65, __p, __stk))
; STestSuite66 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite67 (__input, __end) (() :> (STestSuite66, __p, __stk))
; STestSuite66 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite71 (__input, __end) (n :> (STestSuite66, __p, __stk))
; STestSuite66 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite79 (__input, __end) (n :> (STestSuite66, __p, __stk))
; STestSuite66 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite83 (__input, __end) (n :> (STestSuite66, __p, __stk))
; STestSuite67 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite67 (__input, __end) (() :> (STestSuite67, __p, __stk))
; STestSuite67 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite71 (__input, __end) (n :> (STestSuite67, __p, __stk))
; STestSuite67 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite79 (__input, __end) (n :> (STestSuite67, __p, __stk))
; STestSuite67 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite83 (__input, __end) (n :> (STestSuite67, __p, __stk))
; STestSuite84 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite66 (__input, __end) (() :> (STestSuite84, __p, __stk))
; STestSuite84 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite70 (__input, __end) (n :> (STestSuite84, __p, __stk))
; STestSuite84 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite78 (__input, __end) (n :> (STestSuite84, __p, __stk))
; STestSuite84 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite82 (__input, __end) (n :> (STestSuite84, __p, __stk))
; STestSuite85 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite64 (__input, __end) (() :> (STestSuite85, __p, __stk))
; STestSuite85 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite68 (__input, __end) (n :> (STestSuite85, __p, __stk))
; STestSuite85 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite76 (__input, __end) (n :> (STestSuite85, __p, __stk))
; STestSuite85 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite80 (__input, __end) (n :> (STestSuite85, __p, __stk))
; STestSuite86 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite65 (__input, __end) (() :> (STestSuite86, __p, __stk))
; STestSuite86 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite69 (__input, __end) (n :> (STestSuite86, __p, __stk))
; STestSuite86 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite77 (__input, __end) (n :> (STestSuite86, __p, __stk))
; STestSuite86 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite81 (__input, __end) (n :> (STestSuite86, __p, __stk))
; STestSuite87 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite64 (__input, __end) (() :> (STestSuite87, __p, __stk))
; STestSuite87 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite68 (__input, __end) (n :> (STestSuite87, __p, __stk))
; STestSuite87 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite76 (__input, __end) (n :> (STestSuite87, __p, __stk))
; STestSuite87 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite80 (__input, __end) (n :> (STestSuite87, __p, __stk))
; STestSuite88 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite65 (__input, __end) (() :> (STestSuite88, __p, __stk))
; STestSuite88 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite69 (__input, __end) (n :> (STestSuite88, __p, __stk))
; STestSuite88 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite77 (__input, __end) (n :> (STestSuite88, __p, __stk))
; STestSuite88 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite81 (__input, __end) (n :> (STestSuite88, __p, __stk))
; STestSuite89 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite66 (__input, __end) (() :> (STestSuite89, __p, __stk))
; STestSuite89 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite70 (__input, __end) (n :> (STestSuite89, __p, __stk))
; STestSuite89 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite78 (__input, __end) (n :> (STestSuite89, __p, __stk))
; STestSuite89 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite82 (__input, __end) (n :> (STestSuite89, __p, __stk))
; STestSuite90 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite67 (__input, __end) (() :> (STestSuite90, __p, __stk))
; STestSuite90 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite71 (__input, __end) (n :> (STestSuite90, __p, __stk))
; STestSuite90 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite79 (__input, __end) (n :> (STestSuite90, __p, __stk))
; STestSuite90 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite83 (__input, __end) (n :> (STestSuite90, __p, __stk))
; STestSuite91 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite64 (__input, __end) (() :> (STestSuite91, __p, __stk))
; STestSuite91 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite68 (__input, __end) (n :> (STestSuite91, __p, __stk))
; STestSuite91 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite76 (__input, __end) (n :> (STestSuite91, __p, __stk))
; STestSuite91 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite80 (__input, __end) (n :> (STestSuite91, __p, __stk))
; STestSuite92 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite65 (__input, __end) (() :> (STestSuite92, __p, __stk))
; STestSuite92 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite69 (__input, __end) (n :> (STestSuite92, __p, __stk))
; STestSuite92 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite77 (__input, __end) (n :> (STestSuite92, __p, __stk))
; STestSuite92 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite81 (__input, __end) (n :> (STestSuite92, __p, __stk))
; STestSuite93 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite66 (__input, __end) (() :> (STestSuite93, __p, __stk))
; STestSuite93 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite70 (__input, __end) (n :> (STestSuite93, __p, __stk))
; STestSuite93 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite78 (__input, __end) (n :> (STestSuite93, __p, __stk))
; STestSuite93 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite82 (__input, __end) (n :> (STestSuite93, __p, __stk))
; STestSuite94 ((__p,  "(") : __input, __end) __stk ->
    __runTestSuite STestSuite67 (__input, __end) (() :> (STestSuite94, __p, __stk))
; STestSuite94 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite71 (__input, __end) (n :> (STestSuite94, __p, __stk))
; STestSuite94 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runTestSuite STestSuite79 (__input, __end) (n :> (STestSuite94, __p, __stk))
; STestSuite94 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runTestSuite STestSuite83 (__input, __end) (n :> (STestSuite94, __p, __stk))
; STestSuite95 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite104 (__input, __end) (() :> (STestSuite95, __p, __stk))
; STestSuite96 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite105 (__input, __end) (() :> (STestSuite96, __p, __stk))
; STestSuite97 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite106 (__input, __end) (() :> (STestSuite97, __p, __stk))
; STestSuite98 ((__p,  ")") : __input, __end) __stk ->
    __runTestSuite STestSuite107 (__input, __end) (() :> (STestSuite98, __p, __stk))
-- lookahead Nothing, entity TestSuite
; STestSuite1 ([], __end) ((res :> __stk@(_, __pos, _))) ->
    pure res
-- lookahead Nothing, entity Expr
; STestSuite4 ([], __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForTestSuite ([], __end) (action59 __pos a) __stk
-- lookahead Just expect, entity Expr
; STestSuite4 ((__p,  "expect") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForTestSuite ((__p,  "expect") : __input, __end) (action59 __pos a) __stk
-- lookahead Just guard, entity Expr
; STestSuite4 ((__p,  "guard") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForTestSuite ((__p,  "guard") : __input, __end) (action59 __pos a) __stk
-- lookahead Just notify, entity Expr
; STestSuite4 ((__p,  "notify") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForTestSuite ((__p,  "notify") : __input, __end) (action59 __pos a) __stk
-- lookahead Nothing, entity Expr
; STestSuite5 ([], __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForTestSuite ([], __end) (action58 __pos a b) __stk
-- lookahead Just expect, entity Expr
; STestSuite5 ((__p,  "expect") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForTestSuite ((__p,  "expect") : __input, __end) (action58 __pos a
                                                                               b) __stk
-- lookahead Just guard, entity Expr
; STestSuite5 ((__p,  "guard") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForTestSuite ((__p,  "guard") : __input, __end) (action58 __pos a
                                                                              b) __stk
-- lookahead Just notify, entity Expr
; STestSuite5 ((__p,  "notify") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForTestSuite ((__p,  "notify") : __input, __end) (action58 __pos a
                                                                               b) __stk
-- lookahead Nothing, entity Add
; STestSuite6 ([], __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ([], __end) (action63 __pos a) __stk
-- lookahead Just +, entity Add
; STestSuite6 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead Just =, entity Add
; STestSuite6 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "=") : __input, __end) (action63 __pos a) __stk
-- lookahead Just expect, entity Add
; STestSuite6 ((__p,  "expect") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "expect") : __input, __end) (action63 __pos a) __stk
-- lookahead Just guard, entity Add
; STestSuite6 ((__p,  "guard") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "guard") : __input, __end) (action63 __pos a) __stk
-- lookahead Just notify, entity Add
; STestSuite6 ((__p,  "notify") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "notify") : __input, __end) (action63 __pos a) __stk
-- lookahead Nothing, entity Add
; STestSuite7 ([], __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ([], __end) (action63 __pos a) __stk
-- lookahead Just +, entity Add
; STestSuite7 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead Just expect, entity Add
; STestSuite7 ((__p,  "expect") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "expect") : __input, __end) (action63 __pos a) __stk
-- lookahead Just guard, entity Add
; STestSuite7 ((__p,  "guard") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "guard") : __input, __end) (action63 __pos a) __stk
-- lookahead Just notify, entity Add
; STestSuite7 ((__p,  "notify") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "notify") : __input, __end) (action63 __pos a) __stk
-- lookahead Nothing, entity Add
; STestSuite8 ([], __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ([], __end) (action62 __pos a b) __stk
-- lookahead Just +, entity Add
; STestSuite8 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead Just =, entity Add
; STestSuite8 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "=") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead Just expect, entity Add
; STestSuite8 ((__p,  "expect") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "expect") : __input, __end) (action62 __pos a
                                                                              b) __stk
-- lookahead Just guard, entity Add
; STestSuite8 ((__p,  "guard") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "guard") : __input, __end) (action62 __pos a
                                                                             b) __stk
-- lookahead Just notify, entity Add
; STestSuite8 ((__p,  "notify") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "notify") : __input, __end) (action62 __pos a
                                                                              b) __stk
-- lookahead Nothing, entity Add
; STestSuite9 ([], __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ([], __end) (action62 __pos a b) __stk
-- lookahead Just +, entity Add
; STestSuite9 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead Just expect, entity Add
; STestSuite9 ((__p,  "expect") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "expect") : __input, __end) (action62 __pos a
                                                                              b) __stk
-- lookahead Just guard, entity Add
; STestSuite9 ((__p,  "guard") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "guard") : __input, __end) (action62 __pos a
                                                                             b) __stk
-- lookahead Just notify, entity Add
; STestSuite9 ((__p,  "notify") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "notify") : __input, __end) (action62 __pos a
                                                                              b) __stk
-- lookahead Nothing, entity Mult
; STestSuite10 ([], __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ([], __end) (action67 __pos a) __stk
-- lookahead Just *, entity Mult
; STestSuite10 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead Just +, entity Mult
; STestSuite10 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead Just =, entity Mult
; STestSuite10 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "=") : __input, __end) (action67 __pos a) __stk
-- lookahead Just expect, entity Mult
; STestSuite10 ((__p,  "expect") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "expect") : __input, __end) (action67 __pos a) __stk
-- lookahead Just guard, entity Mult
; STestSuite10 ((__p,  "guard") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "guard") : __input, __end) (action67 __pos a) __stk
-- lookahead Just notify, entity Mult
; STestSuite10 ((__p,  "notify") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "notify") : __input, __end) (action67 __pos a) __stk
-- lookahead Nothing, entity Mult
; STestSuite11 ([], __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ([], __end) (action67 __pos a) __stk
-- lookahead Just *, entity Mult
; STestSuite11 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead Just +, entity Mult
; STestSuite11 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead Just expect, entity Mult
; STestSuite11 ((__p,  "expect") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "expect") : __input, __end) (action67 __pos a) __stk
-- lookahead Just guard, entity Mult
; STestSuite11 ((__p,  "guard") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "guard") : __input, __end) (action67 __pos a) __stk
-- lookahead Just notify, entity Mult
; STestSuite11 ((__p,  "notify") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "notify") : __input, __end) (action67 __pos a) __stk
-- lookahead Nothing, entity Term
; STestSuite14 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ([], __end) (action71 __pos n) __stk
-- lookahead Just *, entity Term
; STestSuite14 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead Just +, entity Term
; STestSuite14 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead Just =, entity Term
; STestSuite14 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action71 __pos n) __stk
-- lookahead Just expect, entity Term
; STestSuite14 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "expect") : __input, __end) (action71 __pos n) __stk
-- lookahead Just guard, entity Term
; STestSuite14 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "guard") : __input, __end) (action71 __pos n) __stk
-- lookahead Just notify, entity Term
; STestSuite14 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "notify") : __input, __end) (action71 __pos n) __stk
-- lookahead Nothing, entity Term
; STestSuite15 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ([], __end) (action71 __pos n) __stk
-- lookahead Just *, entity Term
; STestSuite15 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead Just +, entity Term
; STestSuite15 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead Just expect, entity Term
; STestSuite15 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "expect") : __input, __end) (action71 __pos n) __stk
-- lookahead Just guard, entity Term
; STestSuite15 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "guard") : __input, __end) (action71 __pos n) __stk
-- lookahead Just notify, entity Term
; STestSuite15 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "notify") : __input, __end) (action71 __pos n) __stk
-- lookahead Nothing, entity Term
; STestSuite16 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ([], __end) (action72 __pos n) __stk
-- lookahead Just *, entity Term
; STestSuite16 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead Just +, entity Term
; STestSuite16 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead Just =, entity Term
; STestSuite16 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action72 __pos n) __stk
-- lookahead Just expect, entity Term
; STestSuite16 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "expect") : __input, __end) (action72 __pos n) __stk
-- lookahead Just guard, entity Term
; STestSuite16 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "guard") : __input, __end) (action72 __pos n) __stk
-- lookahead Just notify, entity Term
; STestSuite16 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "notify") : __input, __end) (action72 __pos n) __stk
-- lookahead Nothing, entity Term
; STestSuite17 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ([], __end) (action72 __pos n) __stk
-- lookahead Just *, entity Term
; STestSuite17 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead Just +, entity Term
; STestSuite17 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead Just expect, entity Term
; STestSuite17 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "expect") : __input, __end) (action72 __pos n) __stk
-- lookahead Just guard, entity Term
; STestSuite17 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "guard") : __input, __end) (action72 __pos n) __stk
-- lookahead Just notify, entity Term
; STestSuite17 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "notify") : __input, __end) (action72 __pos n) __stk
-- lookahead Nothing, entity Const
; STestSuite18 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ([], __end) (action75 __pos n) __stk
-- lookahead Just *, entity Const
; STestSuite18 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead Just +, entity Const
; STestSuite18 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead Just =, entity Const
; STestSuite18 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead Just expect, entity Const
; STestSuite18 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "expect") : __input, __end) (action75 __pos n) __stk
-- lookahead Just guard, entity Const
; STestSuite18 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "guard") : __input, __end) (action75 __pos n) __stk
-- lookahead Just notify, entity Const
; STestSuite18 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "notify") : __input, __end) (action75 __pos n) __stk
-- lookahead Nothing, entity Const
; STestSuite19 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ([], __end) (action75 __pos n) __stk
-- lookahead Just *, entity Const
; STestSuite19 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead Just +, entity Const
; STestSuite19 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead Just expect, entity Const
; STestSuite19 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "expect") : __input, __end) (action75 __pos n) __stk
-- lookahead Just guard, entity Const
; STestSuite19 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "guard") : __input, __end) (action75 __pos n) __stk
-- lookahead Just notify, entity Const
; STestSuite19 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "notify") : __input, __end) (action75 __pos n) __stk
-- lookahead Nothing, entity Const
; STestSuite20 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ([], __end) (action76 __pos n) __stk
-- lookahead Just *, entity Const
; STestSuite20 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead Just +, entity Const
; STestSuite20 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead Just =, entity Const
; STestSuite20 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "=") : __input, __end) (action76 __pos n) __stk
-- lookahead Just expect, entity Const
; STestSuite20 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "expect") : __input, __end) (action76 __pos n) __stk
-- lookahead Just guard, entity Const
; STestSuite20 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "guard") : __input, __end) (action76 __pos n) __stk
-- lookahead Just notify, entity Const
; STestSuite20 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "notify") : __input, __end) (action76 __pos n) __stk
-- lookahead Nothing, entity Const
; STestSuite21 ([], __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ([], __end) (action76 __pos n) __stk
-- lookahead Just *, entity Const
; STestSuite21 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead Just +, entity Const
; STestSuite21 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead Just expect, entity Const
; STestSuite21 ((__p,  "expect") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "expect") : __input, __end) (action76 __pos n) __stk
-- lookahead Just guard, entity Const
; STestSuite21 ((__p,  "guard") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "guard") : __input, __end) (action76 __pos n) __stk
-- lookahead Just notify, entity Const
; STestSuite21 ((__p,  "notify") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "notify") : __input, __end) (action76 __pos n) __stk
-- lookahead Nothing, entity Tests
; STestSuite26 ([], __end) ((t :> __stk@(_, __pos, _))) ->
    __gotoTestsForTestSuite ([], __end) (action87 __pos t) __stk
-- lookahead Nothing, entity Call
; STestSuite27 ([], __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForTestSuite ([], __end) (action47 __pos pre t) __stk
-- lookahead Just expect, entity Call
; STestSuite27 ((__p,  "expect") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForTestSuite ((__p,  "expect") : __input, __end) (action47 __pos pre
                                                                               t) __stk
-- lookahead Just guard, entity Call
; STestSuite27 ((__p,  "guard") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForTestSuite ((__p,  "guard") : __input, __end) (action47 __pos pre
                                                                              t) __stk
-- lookahead Just notify, entity Call
; STestSuite27 ((__p,  "notify") : __input, __end) ((t :> (_, _, pre :> __stk@(_, __pos, _)))) ->
    __gotoCallForTestSuite ((__p,  "notify") : __input, __end) (action47 __pos pre
                                                                               t) __stk
-- lookahead Nothing, entity Tuple
; STestSuite28 ([], __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForTestSuite ([], __end) (action50 __pos ) __stk
-- lookahead Just expect, entity Tuple
; STestSuite28 ((__p,  "expect") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForTestSuite ((__p,  "expect") : __input, __end) (action50 __pos ) __stk
-- lookahead Just guard, entity Tuple
; STestSuite28 ((__p,  "guard") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForTestSuite ((__p,  "guard") : __input, __end) (action50 __pos ) __stk
-- lookahead Just notify, entity Tuple
; STestSuite28 ((__p,  "notify") : __input, __end) ((_ :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTupleForTestSuite ((__p,  "notify") : __input, __end) (action50 __pos ) __stk
-- lookahead Nothing, entity Test
; STestSuite37 ([], __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ([], __end) (action79 __pos c) __stk
-- lookahead Just expect, entity Test
; STestSuite37 ((__p,  "expect") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "expect") : __input, __end) (action79 __pos c) __stk
-- lookahead Just guard, entity Test
; STestSuite37 ((__p,  "guard") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "guard") : __input, __end) (action79 __pos c) __stk
-- lookahead Just notify, entity Test
; STestSuite37 ((__p,  "notify") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "notify") : __input, __end) (action79 __pos c) __stk
-- lookahead Nothing, entity Test
; STestSuite38 ([], __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ([], __end) (action80 __pos c) __stk
-- lookahead Just expect, entity Test
; STestSuite38 ((__p,  "expect") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "expect") : __input, __end) (action80 __pos c) __stk
-- lookahead Just guard, entity Test
; STestSuite38 ((__p,  "guard") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "guard") : __input, __end) (action80 __pos c) __stk
-- lookahead Just notify, entity Test
; STestSuite38 ((__p,  "notify") : __input, __end) ((c :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "notify") : __input, __end) (action80 __pos c) __stk
-- lookahead Nothing, entity Test
; STestSuite39 ([], __end) ((e :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ([], __end) (action81 __pos e) __stk
-- lookahead Just expect, entity Test
; STestSuite39 ((__p,  "expect") : __input, __end) ((e :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "expect") : __input, __end) (action81 __pos e) __stk
-- lookahead Just guard, entity Test
; STestSuite39 ((__p,  "guard") : __input, __end) ((e :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "guard") : __input, __end) (action81 __pos e) __stk
-- lookahead Just notify, entity Test
; STestSuite39 ((__p,  "notify") : __input, __end) ((e :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestForTestSuite ((__p,  "notify") : __input, __end) (action81 __pos e) __stk
-- lookahead Nothing, entity TestSuite
; STestSuite40 ([], __end) ((tests :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoTestSuiteForTestSuite ([], __end) (action84 __pos tests) __stk
-- lookahead Nothing, entity Tests
; STestSuite41 ([], __end) ((ts :> (_, _, t :> __stk@(_, __pos, _)))) ->
    __gotoTestsForTestSuite ([], __end) (action88 __pos t ts) __stk
-- lookahead Nothing, entity Tuple
; STestSuite42 ([], __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForTestSuite ([], __end) (action51 __pos es) __stk
-- lookahead Just expect, entity Tuple
; STestSuite42 ((__p,  "expect") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForTestSuite ((__p,  "expect") : __input, __end) (action51 __pos es) __stk
-- lookahead Just guard, entity Tuple
; STestSuite42 ((__p,  "guard") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForTestSuite ((__p,  "guard") : __input, __end) (action51 __pos es) __stk
-- lookahead Just notify, entity Tuple
; STestSuite42 ((__p,  "notify") : __input, __end) ((_ :> (_, _, es :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTupleForTestSuite ((__p,  "notify") : __input, __end) (action51 __pos es) __stk
-- lookahead Nothing, entity Mult
; STestSuite43 ([], __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ([], __end) (action66 __pos a b) __stk
-- lookahead Just *, entity Mult
; STestSuite43 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead Just +, entity Mult
; STestSuite43 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead Just =, entity Mult
; STestSuite43 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "=") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead Just expect, entity Mult
; STestSuite43 ((__p,  "expect") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "expect") : __input, __end) (action66 __pos a
                                                                               b) __stk
-- lookahead Just guard, entity Mult
; STestSuite43 ((__p,  "guard") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "guard") : __input, __end) (action66 __pos a
                                                                              b) __stk
-- lookahead Just notify, entity Mult
; STestSuite43 ((__p,  "notify") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "notify") : __input, __end) (action66 __pos a
                                                                               b) __stk
-- lookahead Nothing, entity Mult
; STestSuite44 ([], __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ([], __end) (action66 __pos a b) __stk
-- lookahead Just *, entity Mult
; STestSuite44 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead Just +, entity Mult
; STestSuite44 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead Just expect, entity Mult
; STestSuite44 ((__p,  "expect") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "expect") : __input, __end) (action66 __pos a
                                                                               b) __stk
-- lookahead Just guard, entity Mult
; STestSuite44 ((__p,  "guard") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "guard") : __input, __end) (action66 __pos a
                                                                              b) __stk
-- lookahead Just notify, entity Mult
; STestSuite44 ((__p,  "notify") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "notify") : __input, __end) (action66 __pos a
                                                                               b) __stk
-- lookahead Nothing, entity Term
; STestSuite45 ([], __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ([], __end) (action70 __pos e) __stk
-- lookahead Just *, entity Term
; STestSuite45 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead Just +, entity Term
; STestSuite45 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead Just =, entity Term
; STestSuite45 ((__p,  "=") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action70 __pos e) __stk
-- lookahead Just expect, entity Term
; STestSuite45 ((__p,  "expect") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "expect") : __input, __end) (action70 __pos e) __stk
-- lookahead Just guard, entity Term
; STestSuite45 ((__p,  "guard") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "guard") : __input, __end) (action70 __pos e) __stk
-- lookahead Just notify, entity Term
; STestSuite45 ((__p,  "notify") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "notify") : __input, __end) (action70 __pos e) __stk
-- lookahead Nothing, entity Term
; STestSuite46 ([], __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ([], __end) (action70 __pos e) __stk
-- lookahead Just *, entity Term
; STestSuite46 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead Just +, entity Term
; STestSuite46 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead Just expect, entity Term
; STestSuite46 ((__p,  "expect") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "expect") : __input, __end) (action70 __pos e) __stk
-- lookahead Just guard, entity Term
; STestSuite46 ((__p,  "guard") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "guard") : __input, __end) (action70 __pos e) __stk
-- lookahead Just notify, entity Term
; STestSuite46 ((__p,  "notify") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "notify") : __input, __end) (action70 __pos e) __stk
-- lookahead Just ), entity Exprs1
; STestSuite47 ((__p,  ")") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoExprs1ForTestSuite ((__p,  ")") : __input, __end) (action55 __pos e) __stk
-- lookahead Just ), entity Expr
; STestSuite48 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForTestSuite ((__p,  ")") : __input, __end) (action59 __pos a) __stk
-- lookahead Just ,, entity Expr
; STestSuite48 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForTestSuite ((__p,  ",") : __input, __end) (action59 __pos a) __stk
-- lookahead Just ), entity Expr
; STestSuite49 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoExprForTestSuite ((__p,  ")") : __input, __end) (action59 __pos a) __stk
-- lookahead Just ), entity Expr
; STestSuite50 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForTestSuite ((__p,  ")") : __input, __end) (action58 __pos a
                                                                          b) __stk
-- lookahead Just ), entity Expr
; STestSuite51 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForTestSuite ((__p,  ")") : __input, __end) (action58 __pos a
                                                                          b) __stk
-- lookahead Just ,, entity Expr
; STestSuite51 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoExprForTestSuite ((__p,  ",") : __input, __end) (action58 __pos a
                                                                          b) __stk
-- lookahead Just ), entity Add
; STestSuite52 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead Just +, entity Add
; STestSuite52 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead Just ), entity Add
; STestSuite53 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead Just +, entity Add
; STestSuite53 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead Just ,, entity Add
; STestSuite53 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead Just ), entity Add
; STestSuite54 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead Just +, entity Add
; STestSuite54 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead Just ,, entity Add
; STestSuite54 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  ",") : __input, __end) (action63 __pos a) __stk
-- lookahead Just =, entity Add
; STestSuite54 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "=") : __input, __end) (action63 __pos a) __stk
-- lookahead Just ), entity Add
; STestSuite55 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action63 __pos a) __stk
-- lookahead Just +, entity Add
; STestSuite55 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action63 __pos a) __stk
-- lookahead Just =, entity Add
; STestSuite55 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoAddForTestSuite ((__p,  "=") : __input, __end) (action63 __pos a) __stk
-- lookahead Just ), entity Add
; STestSuite56 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead Just +, entity Add
; STestSuite56 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead Just ), entity Add
; STestSuite57 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead Just +, entity Add
; STestSuite57 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead Just ,, entity Add
; STestSuite57 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  ",") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead Just ), entity Add
; STestSuite58 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead Just +, entity Add
; STestSuite58 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead Just ,, entity Add
; STestSuite58 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  ",") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead Just =, entity Add
; STestSuite58 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "=") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead Just ), entity Add
; STestSuite59 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  ")") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead Just +, entity Add
; STestSuite59 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "+") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead Just =, entity Add
; STestSuite59 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoAddForTestSuite ((__p,  "=") : __input, __end) (action62 __pos a
                                                                         b) __stk
-- lookahead Just ), entity Mult
; STestSuite60 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead Just *, entity Mult
; STestSuite60 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead Just +, entity Mult
; STestSuite60 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead Just ), entity Mult
; STestSuite61 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead Just *, entity Mult
; STestSuite61 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead Just +, entity Mult
; STestSuite61 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead Just ,, entity Mult
; STestSuite61 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead Just ), entity Mult
; STestSuite62 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead Just *, entity Mult
; STestSuite62 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead Just +, entity Mult
; STestSuite62 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead Just ,, entity Mult
; STestSuite62 ((__p,  ",") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  ",") : __input, __end) (action67 __pos a) __stk
-- lookahead Just =, entity Mult
; STestSuite62 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "=") : __input, __end) (action67 __pos a) __stk
-- lookahead Just ), entity Mult
; STestSuite63 ((__p,  ")") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action67 __pos a) __stk
-- lookahead Just *, entity Mult
; STestSuite63 ((__p,  "*") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action67 __pos a) __stk
-- lookahead Just +, entity Mult
; STestSuite63 ((__p,  "+") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action67 __pos a) __stk
-- lookahead Just =, entity Mult
; STestSuite63 ((__p,  "=") : __input, __end) ((a :> __stk@(_, __pos, _))) ->
    __gotoMultForTestSuite ((__p,  "=") : __input, __end) (action67 __pos a) __stk
-- lookahead Just ), entity Term
; STestSuite68 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead Just *, entity Term
; STestSuite68 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead Just +, entity Term
; STestSuite68 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead Just ), entity Term
; STestSuite69 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead Just *, entity Term
; STestSuite69 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead Just +, entity Term
; STestSuite69 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead Just ,, entity Term
; STestSuite69 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead Just ), entity Term
; STestSuite70 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead Just *, entity Term
; STestSuite70 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead Just +, entity Term
; STestSuite70 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead Just ,, entity Term
; STestSuite70 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ",") : __input, __end) (action71 __pos n) __stk
-- lookahead Just =, entity Term
; STestSuite70 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action71 __pos n) __stk
-- lookahead Just ), entity Term
; STestSuite71 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action71 __pos n) __stk
-- lookahead Just *, entity Term
; STestSuite71 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action71 __pos n) __stk
-- lookahead Just +, entity Term
; STestSuite71 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action71 __pos n) __stk
-- lookahead Just =, entity Term
; STestSuite71 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action71 __pos n) __stk
-- lookahead Just ), entity Term
; STestSuite72 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead Just *, entity Term
; STestSuite72 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead Just +, entity Term
; STestSuite72 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead Just ), entity Term
; STestSuite73 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead Just *, entity Term
; STestSuite73 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead Just +, entity Term
; STestSuite73 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead Just ,, entity Term
; STestSuite73 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead Just ), entity Term
; STestSuite74 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead Just *, entity Term
; STestSuite74 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead Just +, entity Term
; STestSuite74 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead Just ,, entity Term
; STestSuite74 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ",") : __input, __end) (action72 __pos n) __stk
-- lookahead Just =, entity Term
; STestSuite74 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action72 __pos n) __stk
-- lookahead Just ), entity Term
; STestSuite75 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action72 __pos n) __stk
-- lookahead Just *, entity Term
; STestSuite75 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action72 __pos n) __stk
-- lookahead Just +, entity Term
; STestSuite75 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action72 __pos n) __stk
-- lookahead Just =, entity Term
; STestSuite75 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action72 __pos n) __stk
-- lookahead Just ), entity Const
; STestSuite76 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead Just *, entity Const
; STestSuite76 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead Just +, entity Const
; STestSuite76 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ), entity Const
; STestSuite77 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead Just *, entity Const
; STestSuite77 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead Just +, entity Const
; STestSuite77 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ,, entity Const
; STestSuite77 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ), entity Const
; STestSuite78 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead Just *, entity Const
; STestSuite78 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead Just +, entity Const
; STestSuite78 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ,, entity Const
; STestSuite78 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ",") : __input, __end) (action75 __pos n) __stk
-- lookahead Just =, entity Const
; STestSuite78 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ), entity Const
; STestSuite79 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action75 __pos n) __stk
-- lookahead Just *, entity Const
; STestSuite79 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action75 __pos n) __stk
-- lookahead Just +, entity Const
; STestSuite79 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action75 __pos n) __stk
-- lookahead Just =, entity Const
; STestSuite79 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "=") : __input, __end) (action75 __pos n) __stk
-- lookahead Just ), entity Const
; STestSuite80 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead Just *, entity Const
; STestSuite80 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead Just +, entity Const
; STestSuite80 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead Just ), entity Const
; STestSuite81 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead Just *, entity Const
; STestSuite81 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead Just +, entity Const
; STestSuite81 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead Just ,, entity Const
; STestSuite81 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead Just ), entity Const
; STestSuite82 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead Just *, entity Const
; STestSuite82 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead Just +, entity Const
; STestSuite82 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead Just ,, entity Const
; STestSuite82 ((__p,  ",") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ",") : __input, __end) (action76 __pos n) __stk
-- lookahead Just =, entity Const
; STestSuite82 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "=") : __input, __end) (action76 __pos n) __stk
-- lookahead Just ), entity Const
; STestSuite83 ((__p,  ")") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  ")") : __input, __end) (action76 __pos n) __stk
-- lookahead Just *, entity Const
; STestSuite83 ((__p,  "*") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "*") : __input, __end) (action76 __pos n) __stk
-- lookahead Just +, entity Const
; STestSuite83 ((__p,  "+") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "+") : __input, __end) (action76 __pos n) __stk
-- lookahead Just =, entity Const
; STestSuite83 ((__p,  "=") : __input, __end) ((n :> __stk@(_, __pos, _))) ->
    __gotoConstForTestSuite ((__p,  "=") : __input, __end) (action76 __pos n) __stk
-- lookahead Just ), entity Exprs1
; STestSuite99 ((__p,  ")") : __input, __end) ((es :> (_, _, _ :> (_, _, e :> __stk@(_, __pos, _))))) ->
    __gotoExprs1ForTestSuite ((__p,  ")") : __input, __end) (action54 __pos e
                                                                            es) __stk
-- lookahead Just ), entity Mult
; STestSuite100 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead Just *, entity Mult
; STestSuite100 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead Just +, entity Mult
; STestSuite100 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead Just ), entity Mult
; STestSuite101 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead Just *, entity Mult
; STestSuite101 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead Just +, entity Mult
; STestSuite101 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead Just ,, entity Mult
; STestSuite101 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  ",") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead Just ), entity Mult
; STestSuite102 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead Just *, entity Mult
; STestSuite102 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead Just +, entity Mult
; STestSuite102 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead Just ,, entity Mult
; STestSuite102 ((__p,  ",") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  ",") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead Just =, entity Mult
; STestSuite102 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "=") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead Just ), entity Mult
; STestSuite103 ((__p,  ")") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  ")") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead Just *, entity Mult
; STestSuite103 ((__p,  "*") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "*") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead Just +, entity Mult
; STestSuite103 ((__p,  "+") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "+") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead Just =, entity Mult
; STestSuite103 ((__p,  "=") : __input, __end) ((b :> (_, _, _ :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoMultForTestSuite ((__p,  "=") : __input, __end) (action66 __pos a
                                                                          b) __stk
-- lookahead Just ), entity Term
; STestSuite104 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead Just *, entity Term
; STestSuite104 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead Just +, entity Term
; STestSuite104 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead Just ), entity Term
; STestSuite105 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead Just *, entity Term
; STestSuite105 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead Just +, entity Term
; STestSuite105 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead Just ,, entity Term
; STestSuite105 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead Just ), entity Term
; STestSuite106 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead Just *, entity Term
; STestSuite106 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead Just +, entity Term
; STestSuite106 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead Just ,, entity Term
; STestSuite106 ((__p,  ",") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  ",") : __input, __end) (action70 __pos e) __stk
-- lookahead Just =, entity Term
; STestSuite106 ((__p,  "=") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action70 __pos e) __stk
-- lookahead Just ), entity Term
; STestSuite107 ((__p,  ")") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  ")") : __input, __end) (action70 __pos e) __stk
-- lookahead Just *, entity Term
; STestSuite107 ((__p,  "*") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "*") : __input, __end) (action70 __pos e) __stk
-- lookahead Just +, entity Term
; STestSuite107 ((__p,  "+") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "+") : __input, __end) (action70 __pos e) __stk
-- lookahead Just =, entity Term
; STestSuite107 ((__p,  "=") : __input, __end) ((_ :> (_, _, e :> (_, _, _ :> __stk@(_, __pos, _))))) ->
    __gotoTermForTestSuite ((__p,  "=") : __input, __end) (action70 __pos e) __stk
; STestSuite0 __input _ -> Left  (currentPos __input, ["test"])
; STestSuite1 __input _ -> Left  (currentPos __input, ["<eof>"])
; STestSuite2 __input _ -> Left  (currentPos __input, ["("])
; STestSuite3 __input _ ->
    Left  (currentPos __input, ["(", ")", "<Name>", "<name>", "<num>"])
; STestSuite4 __input _ ->
    Left  (currentPos __input, ["<eof>", "+", "=", "expect", "guard",
                                "notify"])
; STestSuite5 __input _ ->
    Left  (currentPos __input, ["<eof>", "+", "expect", "guard",
                                "notify"])
; STestSuite6 __input _ ->
    Left  (currentPos __input, ["<eof>", "*", "+", "=", "expect",
                                "guard", "notify"])
; STestSuite7 __input _ ->
    Left  (currentPos __input, ["<eof>", "*", "+", "expect", "guard",
                                "notify"])
; STestSuite8 __input _ ->
    Left  (currentPos __input, ["<eof>", "*", "+", "=", "expect",
                                "guard", "notify"])
; STestSuite9 __input _ ->
    Left  (currentPos __input, ["<eof>", "*", "+", "expect", "guard",
                                "notify"])
; STestSuite10 __input _ ->
    Left  (currentPos __input, ["<eof>", "*", "+", "=", "expect",
                                "guard", "notify"])
; STestSuite11 __input _ ->
    Left  (currentPos __input, ["<eof>", "*", "+", "expect", "guard",
                                "notify"])
; STestSuite12 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite13 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite14 __input _ ->
    Left  (currentPos __input, ["<eof>", "*", "+", "=", "expect",
                                "guard", "notify"])
; STestSuite15 __input _ ->
    Left  (currentPos __input, ["<eof>", "*", "+", "expect", "guard",
                                "notify"])
; STestSuite16 __input _ ->
    Left  (currentPos __input, ["<eof>", "*", "+", "=", "expect",
                                "guard", "notify"])
; STestSuite17 __input _ ->
    Left  (currentPos __input, ["<eof>", "*", "+", "expect", "guard",
                                "notify"])
; STestSuite18 __input _ ->
    Left  (currentPos __input, ["<eof>", "*", "+", "=", "expect",
                                "guard", "notify"])
; STestSuite19 __input _ ->
    Left  (currentPos __input, ["<eof>", "*", "+", "expect", "guard",
                                "notify"])
; STestSuite20 __input _ ->
    Left  (currentPos __input, ["<eof>", "*", "+", "=", "expect",
                                "guard", "notify"])
; STestSuite21 __input _ ->
    Left  (currentPos __input, ["<eof>", "*", "+", "expect", "guard",
                                "notify"])
; STestSuite22 __input _ -> Left  (currentPos __input, ["<name>"])
; STestSuite23 __input _ -> Left  (currentPos __input, ["<name>"])
; STestSuite24 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite25 __input _ ->
    Left  (currentPos __input, ["expect", "guard", "notify"])
; STestSuite26 __input _ ->
    Left  (currentPos __input, ["<eof>", "expect", "guard", "notify"])
; STestSuite27 __input _ ->
    Left  (currentPos __input, ["<eof>", "expect", "guard", "notify"])
; STestSuite28 __input _ ->
    Left  (currentPos __input, ["<eof>", "expect", "guard", "notify"])
; STestSuite29 __input _ -> Left  (currentPos __input, [")"])
; STestSuite30 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite31 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite32 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite33 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite34 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite35 __input _ -> Left  (currentPos __input, [")"])
; STestSuite36 __input _ -> Left  (currentPos __input, [")"])
; STestSuite37 __input _ ->
    Left  (currentPos __input, ["<eof>", "expect", "guard", "notify"])
; STestSuite38 __input _ ->
    Left  (currentPos __input, ["<eof>", "expect", "guard", "notify"])
; STestSuite39 __input _ ->
    Left  (currentPos __input, ["<eof>", "expect", "guard", "notify"])
; STestSuite40 __input _ -> Left  (currentPos __input, ["<eof>"])
; STestSuite41 __input _ -> Left  (currentPos __input, ["<eof>"])
; STestSuite42 __input _ ->
    Left  (currentPos __input, ["<eof>", "expect", "guard", "notify"])
; STestSuite43 __input _ ->
    Left  (currentPos __input, ["<eof>", "*", "+", "=", "expect",
                                "guard", "notify"])
; STestSuite44 __input _ ->
    Left  (currentPos __input, ["<eof>", "*", "+", "expect", "guard",
                                "notify"])
; STestSuite45 __input _ ->
    Left  (currentPos __input, ["<eof>", "*", "+", "=", "expect",
                                "guard", "notify"])
; STestSuite46 __input _ ->
    Left  (currentPos __input, ["<eof>", "*", "+", "expect", "guard",
                                "notify"])
; STestSuite47 __input _ -> Left  (currentPos __input, [")", ","])
; STestSuite48 __input _ ->
    Left  (currentPos __input, [")", "+", ",", "="])
; STestSuite49 __input _ ->
    Left  (currentPos __input, [")", "+", "="])
; STestSuite50 __input _ -> Left  (currentPos __input, [")", "+"])
; STestSuite51 __input _ ->
    Left  (currentPos __input, [")", "+", ","])
; STestSuite52 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite53 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite54 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite55 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
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
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite69 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite70 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite71 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite72 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite73 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite74 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite75 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite76 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite77 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite78 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite79 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite80 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite81 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite82 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite83 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite84 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite85 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite86 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite87 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite88 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite89 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite90 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite91 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite92 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite93 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite94 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; STestSuite95 __input _ -> Left  (currentPos __input, [")"])
; STestSuite96 __input _ -> Left  (currentPos __input, [")"])
; STestSuite97 __input _ -> Left  (currentPos __input, [")"])
; STestSuite98 __input _ -> Left  (currentPos __input, [")"])
; STestSuite99 __input _ -> Left  (currentPos __input, [")"])
; STestSuite100 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite101 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite102 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite103 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
; STestSuite104 __input _ ->
    Left  (currentPos __input, [")", "*", "+"])
; STestSuite105 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ","])
; STestSuite106 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; STestSuite107 __input _ ->
    Left  (currentPos __input, [")", "*", "+", "="])
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
    Right input -> pure (Right (__runTestSuite STestSuite0 input Nil))
  
currentPos :: ([Lexeme], Pos) -> Pos
currentPos = \case
  ([],           end) -> end
  ((pos, _) : _, _)   -> pos
  