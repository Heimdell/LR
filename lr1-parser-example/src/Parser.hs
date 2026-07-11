{-# language PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Parser (parseProgram) where
  
import Data.Text.IO.Utf8 qualified as Text
import Data.Kind qualified as Kind
import Text.Lexer.Default
import Data.Text.Position (Pos)
import Data.Lexeme
import AST
import Data.Text (Text)
  
data Stack' xs where
  Nil  ::      Stack' '[]
  (:>) :: x -> Stack xs -> Stack' (x : xs)
  
type Stack a = (St a, Pos, Stack' a)
  
pattern (:?) :: a -> Stack xs -> Stack (a : xs)
pattern a :? xs <- (_, _, a :> xs)
  
infixr 2 :>, :?
  
data St :: [Kind.Type] -> Kind.Type where
  S0 :: forall a. St (Expr : a)
  S1 :: forall a. St (Expr : a)
  S2 :: forall a. St (Expr : a)
  S3 :: forall a. St (Expr : () : Expr : a)
  S4 :: forall a. St (Expr : () : Expr : a)
  S5 :: forall a. St (Expr : a)
  S6 :: forall a. St (Expr : a)
  S7 :: forall a. St (Expr : a)
  S8 :: forall a. St (Expr : a)
  S9 :: forall a. St (Expr : () : Expr : a)
  S10 :: forall a. St (Expr : () : Expr : a)
  S11 :: forall a. St (Expr : () : Expr : a)
  S12 :: forall a. St (Expr : () : Expr : a)
  S13 :: forall a. St (Expr : a)
  S14 :: forall a. St (Expr : a)
  S15 :: forall a. St (Expr : a)
  S16 :: forall a. St (Expr : a)
  S17 :: forall a. St (() : a)
  S18 :: forall a. St (() : a)
  S19 :: forall a. St (() : a)
  S20 :: forall a. St (() : a)
  S21 :: forall a. St (Text : a)
  S22 :: forall a. St (Text : a)
  S23 :: forall a. St (Text : a)
  S24 :: forall a. St (Text : a)
  S25 :: forall a. St (Const : a)
  S26 :: forall a. St (Const : a)
  S27 :: forall a. St (Const : a)
  S28 :: forall a. St (Const : a)
  S29 :: forall a. St (Text : a)
  S30 :: forall a. St (Text : a)
  S31 :: forall a. St (Text : a)
  S32 :: forall a. St (Text : a)
  S33 :: forall a. St (Integer : a)
  S34 :: forall a. St (Integer : a)
  S35 :: forall a. St (Integer : a)
  S36 :: forall a. St (Integer : a)
  S37 :: forall a. St (() : Expr : a)
  S38 :: forall a. St (() : Expr : a)
  S39 :: forall a. St (() : Expr : a)
  S40 :: forall a. St (() : Expr : a)
  S41 :: forall a. St (() : Expr : a)
  S42 :: forall a. St (() : Expr : a)
  S43 :: forall a. St (() : Expr : a)
  S44 :: forall a. St (() : Expr : a)
  S45 :: forall a. St (() : Expr : a)
  S46 :: forall a. St (() : Expr : a)
  S47 :: forall a. St (() : Expr : a)
  S48 :: forall a. St (Expr : () : a)
  S49 :: forall a. St (Expr : () : a)
  S50 :: forall a. St (Expr : () : a)
  S51 :: forall a. St (Expr : () : a)
  S52 :: forall a. St ([Expr] : () : Expr : a)
  S53 :: forall a. St (Expr : () : Expr : a)
  S54 :: forall a. St (Expr : () : Expr : a)
  S55 :: forall a. St (Expr : () : Expr : a)
  S56 :: forall a. St (Expr : () : Expr : a)
  S57 :: forall a. St (() : Expr : () : a)
  S58 :: forall a. St (() : Expr : () : a)
  S59 :: forall a. St (() : Expr : () : a)
  S60 :: forall a. St (() : Expr : () : a)
  S61 :: forall a. St (Expr : a)
  S62 :: forall a. St (Expr : a)
  S63 :: forall a. St (Expr : a)
  S64 :: forall a. St (Expr : a)
  S65 :: forall a. St (Expr : () : Expr : a)
  S66 :: forall a. St (Expr : () : Expr : a)
  S67 :: forall a. St (Expr : () : Expr : a)
  S68 :: forall a. St (Expr : () : Expr : a)
  S69 :: forall a. St (Expr : a)
  S70 :: forall a. St (Expr : a)
  S71 :: forall a. St (Expr : a)
  S72 :: forall a. St (Expr : a)
  S73 :: forall a. St (() : a)
  S74 :: forall a. St (() : a)
  S75 :: forall a. St (() : a)
  S76 :: forall a. St (() : a)
  S77 :: forall a. St (Text : a)
  S78 :: forall a. St (Text : a)
  S79 :: forall a. St (Text : a)
  S80 :: forall a. St (Text : a)
  S81 :: forall a. St (Const : a)
  S82 :: forall a. St (Const : a)
  S83 :: forall a. St (Const : a)
  S84 :: forall a. St (Const : a)
  S85 :: forall a. St (Text : a)
  S86 :: forall a. St (Text : a)
  S87 :: forall a. St (Text : a)
  S88 :: forall a. St (Text : a)
  S89 :: forall a. St (Text : a)
  S90 :: forall a. St (Text : a)
  S91 :: forall a. St (Integer : a)
  S92 :: forall a. St (Integer : a)
  S93 :: forall a. St (Integer : a)
  S94 :: forall a. St (Integer : a)
  S95 :: forall a. St (() : Expr : a)
  S96 :: forall a. St (() : Expr : a)
  S97 :: forall a. St (() : Expr : a)
  S98 :: forall a. St (() : Expr : a)
  S99 :: forall a. St (Expr : () : a)
  S100 :: forall a. St (Expr : () : a)
  S101 :: forall a. St (Expr : () : a)
  S102 :: forall a. St (Expr : () : a)
  S103 :: forall a. St (Expr : () : Expr : a)
  S104 :: forall a. St (Expr : () : Expr : a)
  S105 :: forall a. St (Expr : () : Expr : a)
  S106 :: forall a. St (Expr : () : Expr : a)
  S107 :: forall a. St (() : Expr : () : a)
  S108 :: forall a. St (() : Expr : () : a)
  S109 :: forall a. St (() : Expr : () : a)
  S110 :: forall a. St (() : Expr : () : a)
  S111 :: forall a. St (Expr : a)
  S112 :: forall a. St (Expr : a)
  S113 :: forall a. St (Expr : () : Expr : a)
  S114 :: forall a. St (Expr : () : Expr : a)
  S115 :: forall a. St (() : Expr : a)
  S116 :: forall a. St (() : Expr : a)
  S117 :: forall a. St (() : Expr : a)
  S118 :: forall a. St (() : Expr : a)
  S119 :: forall a. St (() : a)
  S120 :: forall a. St (() : a)
  S121 :: forall a. St (Call : a)
  S122 :: forall a. St (Call : a)
  S123 :: forall a. St (() : a)
  S124 :: forall a. St (() : a)
  S125 :: forall a. St (Expr : a)
  S126 :: forall a. St (Expr : a)
  S127 :: forall a. St (Text : a)
  S128 :: forall a. St (Text : a)
  S129 :: forall a. St (() : a)
  S130 :: forall a. St (() : a)
  S131 :: forall a. St (Call : () : a)
  S132 :: forall a. St (Call : () : a)
  S133 :: forall a. St (Call : () : a)
  S134 :: forall a. St (Call : () : a)
  S135 :: forall a. St ([Expr] : Text : a)
  S136 :: forall a. St ([Expr] : Text : a)
  S137 :: forall a. St (() : () : a)
  S138 :: forall a. St (() : () : a)
  S139 :: forall a. St ([Expr] : () : a)
  S140 :: forall a. St ([Expr] : () : a)
  S141 :: forall a. St (() : Expr : a)
  S142 :: forall a. St (() : Expr : a)
  S143 :: forall a. St (() : [Expr] : () : a)
  S144 :: forall a. St (() : [Expr] : () : a)
  S145 :: forall a. St (Text : a)
  S146 :: forall a. St (() : a)
  S147 :: forall a. St ([Expr] : Text : a)
  S148 :: forall a. St (() : () : a)
  S149 :: forall a. St ([Expr] : () : a)
  S150 :: forall a. St (() : [Expr] : () : a)
  S151 :: forall a. St (Change : a)
  S152 :: forall a. St (Cond : a)
  S153 :: forall a. St (Cond : a)
  S154 :: forall a. St (() : Change : a)
  S155 :: forall a. St (() : Cond : a)
  S156 :: forall a. St (() : Cond : a)
  S157 :: forall a. St ([Change] : () : Change : a)
  S158 :: forall a. St ([Cond] : () : Cond : a)
  S159 :: forall a. St ([Cond] : () : Cond : a)
  S160 :: forall a. St (a)
  S161 :: forall a. St (Program : a)
  S162 :: forall a. St ([Stmt] : a)
  S163 :: forall a. St (Stmt : a)
  S164 :: forall a. St (Clause : a)
  S165 :: forall a. St (Effect : a)
  S166 :: forall a. St (Call : a)
  S167 :: forall a. St ([Stmt] : Stmt : a)
  S168 :: forall a. St (() : Call : a)
  S169 :: forall a. St (() : Call : a)
  S170 :: forall a. St (() : Call : a)
  S171 :: forall a. St (() : Call : a)
  S172 :: forall a. St ([Change] : () : Call : a)
  S173 :: forall a. St ([Cond] : () : Call : a)
  S174 :: forall a. St ([Cond] : () : Call : a)
  S175 :: forall a. St (() : [Change] : () : Call : a)
  S176 :: forall a. St (() : [Cond] : () : Call : a)
  S177 :: forall a. St (() : [Cond] : () : Call : a)
  S178 :: forall a. St (() : [Cond] : () : Call : a)
  S179 :: forall a. St ([Change] : () : [Cond] : () : Call : a)
  S180 :: forall a. St (() : [Change] : () : [Cond] : () : Call : a)
  
__gotoAdd :: ([Lexeme], Pos) -> Expr -> Stack a -> Either (Pos, [String]) Program
__gotoAdd toks term stk@(state, _, _) = case state of
  S17 -> __runProgram S2 toks (term :> stk)
  S18 -> __runProgram S2 toks (term :> stk)
  S19 -> __runProgram S2 toks (term :> stk)
  S20 -> __runProgram S2 toks (term :> stk)
  S37 -> __runProgram S1 toks (term :> stk)
  S38 -> __runProgram S3 toks (term :> stk)
  S39 -> __runProgram S4 toks (term :> stk)
  S73 -> __runProgram S2 toks (term :> stk)
  S74 -> __runProgram S2 toks (term :> stk)
  S75 -> __runProgram S2 toks (term :> stk)
  S76 -> __runProgram S2 toks (term :> stk)
  S129 -> __runProgram S1 toks (term :> stk)
  S130 -> __runProgram S1 toks (term :> stk)
  S141 -> __runProgram S113 toks (term :> stk)
  S142 -> __runProgram S114 toks (term :> stk)
  S146 -> __runProgram S1 toks (term :> stk)
  S155 -> __runProgram S111 toks (term :> stk)
  S156 -> __runProgram S112 toks (term :> stk)
  S169 -> __runProgram S112 toks (term :> stk)
  S171 -> __runProgram S111 toks (term :> stk)
  _ -> error ""

__gotoCall :: ([Lexeme], Pos) -> Call -> Stack a -> Either (Pos, [String]) Program
__gotoCall toks term stk@(state, _, _) = case state of
  S119 -> __runProgram S131 toks (term :> stk)
  S120 -> __runProgram S132 toks (term :> stk)
  S123 -> __runProgram S133 toks (term :> stk)
  S124 -> __runProgram S134 toks (term :> stk)
  S155 -> __runProgram S121 toks (term :> stk)
  S156 -> __runProgram S122 toks (term :> stk)
  S160 -> __runProgram S166 toks (term :> stk)
  S163 -> __runProgram S166 toks (term :> stk)
  S169 -> __runProgram S122 toks (term :> stk)
  S171 -> __runProgram S121 toks (term :> stk)
  _ -> error ""

__gotoChange :: ([Lexeme], Pos) -> Change -> Stack a -> Either (Pos, [String]) Program
__gotoChange toks term stk@(state, _, _) = case state of
  S154 -> __runProgram S151 toks (term :> stk)
  S168 -> __runProgram S151 toks (term :> stk)
  S176 -> __runProgram S151 toks (term :> stk)
  _ -> error ""

__gotoChanges :: ([Lexeme], Pos) -> [Change] -> Stack a -> Either (Pos, [String]) Program
__gotoChanges toks term stk@(state, _, _) = case state of
  S154 -> __runProgram S157 toks (term :> stk)
  S168 -> __runProgram S172 toks (term :> stk)
  S176 -> __runProgram S179 toks (term :> stk)
  _ -> error ""

__gotoClause :: ([Lexeme], Pos) -> Clause -> Stack a -> Either (Pos, [String]) Program
__gotoClause toks term stk@(state, _, _) = case state of
  S160 -> __runProgram S164 toks (term :> stk)
  S163 -> __runProgram S164 toks (term :> stk)
  _ -> error ""

__gotoCond :: ([Lexeme], Pos) -> Cond -> Stack a -> Either (Pos, [String]) Program
__gotoCond toks term stk@(state, _, _) = case state of
  S155 -> __runProgram S152 toks (term :> stk)
  S156 -> __runProgram S153 toks (term :> stk)
  S169 -> __runProgram S153 toks (term :> stk)
  S171 -> __runProgram S152 toks (term :> stk)
  _ -> error ""

__gotoConds :: ([Lexeme], Pos) -> [Cond] -> Stack a -> Either (Pos, [String]) Program
__gotoConds toks term stk@(state, _, _) = case state of
  S155 -> __runProgram S158 toks (term :> stk)
  S156 -> __runProgram S159 toks (term :> stk)
  S169 -> __runProgram S173 toks (term :> stk)
  S171 -> __runProgram S174 toks (term :> stk)
  _ -> error ""

__gotoConst :: ([Lexeme], Pos) -> Const -> Stack a -> Either (Pos, [String]) Program
__gotoConst toks term stk@(state, _, _) = case state of
  S17 -> __runProgram S28 toks (term :> stk)
  S18 -> __runProgram S28 toks (term :> stk)
  S19 -> __runProgram S28 toks (term :> stk)
  S20 -> __runProgram S28 toks (term :> stk)
  S37 -> __runProgram S27 toks (term :> stk)
  S38 -> __runProgram S25 toks (term :> stk)
  S39 -> __runProgram S26 toks (term :> stk)
  S40 -> __runProgram S25 toks (term :> stk)
  S41 -> __runProgram S26 toks (term :> stk)
  S42 -> __runProgram S27 toks (term :> stk)
  S43 -> __runProgram S28 toks (term :> stk)
  S44 -> __runProgram S25 toks (term :> stk)
  S45 -> __runProgram S26 toks (term :> stk)
  S46 -> __runProgram S27 toks (term :> stk)
  S47 -> __runProgram S28 toks (term :> stk)
  S73 -> __runProgram S28 toks (term :> stk)
  S74 -> __runProgram S28 toks (term :> stk)
  S75 -> __runProgram S28 toks (term :> stk)
  S76 -> __runProgram S28 toks (term :> stk)
  S95 -> __runProgram S81 toks (term :> stk)
  S96 -> __runProgram S82 toks (term :> stk)
  S97 -> __runProgram S83 toks (term :> stk)
  S98 -> __runProgram S84 toks (term :> stk)
  S115 -> __runProgram S81 toks (term :> stk)
  S116 -> __runProgram S82 toks (term :> stk)
  S117 -> __runProgram S83 toks (term :> stk)
  S118 -> __runProgram S84 toks (term :> stk)
  S129 -> __runProgram S27 toks (term :> stk)
  S130 -> __runProgram S27 toks (term :> stk)
  S141 -> __runProgram S81 toks (term :> stk)
  S142 -> __runProgram S84 toks (term :> stk)
  S146 -> __runProgram S27 toks (term :> stk)
  S155 -> __runProgram S82 toks (term :> stk)
  S156 -> __runProgram S83 toks (term :> stk)
  S169 -> __runProgram S83 toks (term :> stk)
  S171 -> __runProgram S82 toks (term :> stk)
  _ -> error ""

__gotoEffect :: ([Lexeme], Pos) -> Effect -> Stack a -> Either (Pos, [String]) Program
__gotoEffect toks term stk@(state, _, _) = case state of
  S160 -> __runProgram S165 toks (term :> stk)
  S163 -> __runProgram S165 toks (term :> stk)
  _ -> error ""

__gotoExpr :: ([Lexeme], Pos) -> Expr -> Stack a -> Either (Pos, [String]) Program
__gotoExpr toks term stk@(state, _, _) = case state of
  S17 -> __runProgram S48 toks (term :> stk)
  S18 -> __runProgram S49 toks (term :> stk)
  S19 -> __runProgram S50 toks (term :> stk)
  S20 -> __runProgram S51 toks (term :> stk)
  S37 -> __runProgram S0 toks (term :> stk)
  S73 -> __runProgram S99 toks (term :> stk)
  S74 -> __runProgram S100 toks (term :> stk)
  S75 -> __runProgram S101 toks (term :> stk)
  S76 -> __runProgram S102 toks (term :> stk)
  S129 -> __runProgram S0 toks (term :> stk)
  S130 -> __runProgram S0 toks (term :> stk)
  S146 -> __runProgram S0 toks (term :> stk)
  S155 -> __runProgram S125 toks (term :> stk)
  S156 -> __runProgram S126 toks (term :> stk)
  S169 -> __runProgram S126 toks (term :> stk)
  S171 -> __runProgram S125 toks (term :> stk)
  _ -> error ""

__gotoExprs1 :: ([Lexeme], Pos) -> [Expr] -> Stack a -> Either (Pos, [String]) Program
__gotoExprs1 toks term stk@(state, _, _) = case state of
  S37 -> __runProgram S52 toks (term :> stk)
  S129 -> __runProgram S139 toks (term :> stk)
  S130 -> __runProgram S140 toks (term :> stk)
  S146 -> __runProgram S149 toks (term :> stk)
  _ -> error ""

__gotoMult :: ([Lexeme], Pos) -> Expr -> Stack a -> Either (Pos, [String]) Program
__gotoMult toks term stk@(state, _, _) = case state of
  S17 -> __runProgram S8 toks (term :> stk)
  S18 -> __runProgram S8 toks (term :> stk)
  S19 -> __runProgram S8 toks (term :> stk)
  S20 -> __runProgram S8 toks (term :> stk)
  S37 -> __runProgram S7 toks (term :> stk)
  S38 -> __runProgram S5 toks (term :> stk)
  S39 -> __runProgram S6 toks (term :> stk)
  S40 -> __runProgram S9 toks (term :> stk)
  S41 -> __runProgram S10 toks (term :> stk)
  S42 -> __runProgram S11 toks (term :> stk)
  S43 -> __runProgram S12 toks (term :> stk)
  S73 -> __runProgram S8 toks (term :> stk)
  S74 -> __runProgram S8 toks (term :> stk)
  S75 -> __runProgram S8 toks (term :> stk)
  S76 -> __runProgram S8 toks (term :> stk)
  S115 -> __runProgram S65 toks (term :> stk)
  S116 -> __runProgram S66 toks (term :> stk)
  S117 -> __runProgram S67 toks (term :> stk)
  S118 -> __runProgram S68 toks (term :> stk)
  S129 -> __runProgram S7 toks (term :> stk)
  S130 -> __runProgram S7 toks (term :> stk)
  S141 -> __runProgram S61 toks (term :> stk)
  S142 -> __runProgram S64 toks (term :> stk)
  S146 -> __runProgram S7 toks (term :> stk)
  S155 -> __runProgram S62 toks (term :> stk)
  S156 -> __runProgram S63 toks (term :> stk)
  S169 -> __runProgram S63 toks (term :> stk)
  S171 -> __runProgram S62 toks (term :> stk)
  _ -> error ""

__gotoProgram :: ([Lexeme], Pos) -> Program -> Stack a -> Either (Pos, [String]) Program
__gotoProgram toks term stk@(state, _, _) = case state of
  S160 -> __runProgram S161 toks (term :> stk)
  _ -> error ""

__gotoStmt :: ([Lexeme], Pos) -> Stmt -> Stack a -> Either (Pos, [String]) Program
__gotoStmt toks term stk@(state, _, _) = case state of
  S160 -> __runProgram S163 toks (term :> stk)
  S163 -> __runProgram S163 toks (term :> stk)
  _ -> error ""

__gotoStmts :: ([Lexeme], Pos) -> [Stmt] -> Stack a -> Either (Pos, [String]) Program
__gotoStmts toks term stk@(state, _, _) = case state of
  S160 -> __runProgram S162 toks (term :> stk)
  S163 -> __runProgram S167 toks (term :> stk)
  _ -> error ""

__gotoTerm :: ([Lexeme], Pos) -> Expr -> Stack a -> Either (Pos, [String]) Program
__gotoTerm toks term stk@(state, _, _) = case state of
  S17 -> __runProgram S16 toks (term :> stk)
  S18 -> __runProgram S16 toks (term :> stk)
  S19 -> __runProgram S16 toks (term :> stk)
  S20 -> __runProgram S16 toks (term :> stk)
  S37 -> __runProgram S15 toks (term :> stk)
  S38 -> __runProgram S13 toks (term :> stk)
  S39 -> __runProgram S14 toks (term :> stk)
  S40 -> __runProgram S13 toks (term :> stk)
  S41 -> __runProgram S14 toks (term :> stk)
  S42 -> __runProgram S15 toks (term :> stk)
  S43 -> __runProgram S16 toks (term :> stk)
  S44 -> __runProgram S53 toks (term :> stk)
  S45 -> __runProgram S54 toks (term :> stk)
  S46 -> __runProgram S55 toks (term :> stk)
  S47 -> __runProgram S56 toks (term :> stk)
  S73 -> __runProgram S16 toks (term :> stk)
  S74 -> __runProgram S16 toks (term :> stk)
  S75 -> __runProgram S16 toks (term :> stk)
  S76 -> __runProgram S16 toks (term :> stk)
  S95 -> __runProgram S103 toks (term :> stk)
  S96 -> __runProgram S104 toks (term :> stk)
  S97 -> __runProgram S105 toks (term :> stk)
  S98 -> __runProgram S106 toks (term :> stk)
  S115 -> __runProgram S69 toks (term :> stk)
  S116 -> __runProgram S70 toks (term :> stk)
  S117 -> __runProgram S71 toks (term :> stk)
  S118 -> __runProgram S72 toks (term :> stk)
  S129 -> __runProgram S15 toks (term :> stk)
  S130 -> __runProgram S15 toks (term :> stk)
  S141 -> __runProgram S69 toks (term :> stk)
  S142 -> __runProgram S72 toks (term :> stk)
  S146 -> __runProgram S15 toks (term :> stk)
  S155 -> __runProgram S70 toks (term :> stk)
  S156 -> __runProgram S71 toks (term :> stk)
  S169 -> __runProgram S71 toks (term :> stk)
  S171 -> __runProgram S70 toks (term :> stk)
  _ -> error ""

__gotoTuple :: ([Lexeme], Pos) -> [Expr] -> Stack a -> Either (Pos, [String]) Program
__gotoTuple toks term stk@(state, _, _) = case state of
  S85 -> __runProgram S135 toks (term :> stk)
  S86 -> __runProgram S136 toks (term :> stk)
  S127 -> __runProgram S135 toks (term :> stk)
  S128 -> __runProgram S136 toks (term :> stk)
  S145 -> __runProgram S147 toks (term :> stk)
  _ -> error ""
  
__runProgram :: St a -> ([Lexeme], Pos) -> Stack' a -> Either (Pos, [String]) Program
__runProgram = \cases {
; S0 ((__p,  ",") : __input, __end) __stk ->
    __runProgram S37 (__input, __end) (() :> (S0, __p, __stk))
; S1 ((__p,  "+") : __input, __end) __stk ->
    __runProgram S42 (__input, __end) (() :> (S1, __p, __stk))
; S1 ((__p,  "=") : __input, __end) __stk ->
    __runProgram S39 (__input, __end) (() :> (S1, __p, __stk))
; S2 ((__p,  "+") : __input, __end) __stk ->
    __runProgram S43 (__input, __end) (() :> (S2, __p, __stk))
; S2 ((__p,  "=") : __input, __end) __stk ->
    __runProgram S38 (__input, __end) (() :> (S2, __p, __stk))
; S3 ((__p,  "+") : __input, __end) __stk ->
    __runProgram S40 (__input, __end) (() :> (S3, __p, __stk))
; S4 ((__p,  "+") : __input, __end) __stk ->
    __runProgram S41 (__input, __end) (() :> (S4, __p, __stk))
; S5 ((__p,  "*") : __input, __end) __stk ->
    __runProgram S44 (__input, __end) (() :> (S5, __p, __stk))
; S6 ((__p,  "*") : __input, __end) __stk ->
    __runProgram S45 (__input, __end) (() :> (S6, __p, __stk))
; S7 ((__p,  "*") : __input, __end) __stk ->
    __runProgram S46 (__input, __end) (() :> (S7, __p, __stk))
; S8 ((__p,  "*") : __input, __end) __stk ->
    __runProgram S47 (__input, __end) (() :> (S8, __p, __stk))
; S9 ((__p,  "*") : __input, __end) __stk ->
    __runProgram S44 (__input, __end) (() :> (S9, __p, __stk))
; S10 ((__p,  "*") : __input, __end) __stk ->
    __runProgram S45 (__input, __end) (() :> (S10, __p, __stk))
; S11 ((__p,  "*") : __input, __end) __stk ->
    __runProgram S46 (__input, __end) (() :> (S11, __p, __stk))
; S12 ((__p,  "*") : __input, __end) __stk ->
    __runProgram S47 (__input, __end) (() :> (S12, __p, __stk))
; S17 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S20 (__input, __end) (() :> (S17, __p, __stk))
; S17 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S24 (__input, __end) (n :> (S17, __p, __stk))
; S17 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S32 (__input, __end) (n :> (S17, __p, __stk))
; S17 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S36 (__input, __end) (n :> (S17, __p, __stk))
; S18 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S20 (__input, __end) (() :> (S18, __p, __stk))
; S18 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S24 (__input, __end) (n :> (S18, __p, __stk))
; S18 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S32 (__input, __end) (n :> (S18, __p, __stk))
; S18 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S36 (__input, __end) (n :> (S18, __p, __stk))
; S19 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S20 (__input, __end) (() :> (S19, __p, __stk))
; S19 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S24 (__input, __end) (n :> (S19, __p, __stk))
; S19 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S32 (__input, __end) (n :> (S19, __p, __stk))
; S19 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S36 (__input, __end) (n :> (S19, __p, __stk))
; S20 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S20 (__input, __end) (() :> (S20, __p, __stk))
; S20 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S24 (__input, __end) (n :> (S20, __p, __stk))
; S20 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S32 (__input, __end) (n :> (S20, __p, __stk))
; S20 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S36 (__input, __end) (n :> (S20, __p, __stk))
; S37 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S19 (__input, __end) (() :> (S37, __p, __stk))
; S37 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S23 (__input, __end) (n :> (S37, __p, __stk))
; S37 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S31 (__input, __end) (n :> (S37, __p, __stk))
; S37 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S35 (__input, __end) (n :> (S37, __p, __stk))
; S38 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S17 (__input, __end) (() :> (S38, __p, __stk))
; S38 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S21 (__input, __end) (n :> (S38, __p, __stk))
; S38 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S29 (__input, __end) (n :> (S38, __p, __stk))
; S38 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S33 (__input, __end) (n :> (S38, __p, __stk))
; S39 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S18 (__input, __end) (() :> (S39, __p, __stk))
; S39 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S22 (__input, __end) (n :> (S39, __p, __stk))
; S39 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S30 (__input, __end) (n :> (S39, __p, __stk))
; S39 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S34 (__input, __end) (n :> (S39, __p, __stk))
; S40 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S17 (__input, __end) (() :> (S40, __p, __stk))
; S40 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S21 (__input, __end) (n :> (S40, __p, __stk))
; S40 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S29 (__input, __end) (n :> (S40, __p, __stk))
; S40 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S33 (__input, __end) (n :> (S40, __p, __stk))
; S41 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S18 (__input, __end) (() :> (S41, __p, __stk))
; S41 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S22 (__input, __end) (n :> (S41, __p, __stk))
; S41 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S30 (__input, __end) (n :> (S41, __p, __stk))
; S41 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S34 (__input, __end) (n :> (S41, __p, __stk))
; S42 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S19 (__input, __end) (() :> (S42, __p, __stk))
; S42 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S23 (__input, __end) (n :> (S42, __p, __stk))
; S42 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S31 (__input, __end) (n :> (S42, __p, __stk))
; S42 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S35 (__input, __end) (n :> (S42, __p, __stk))
; S43 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S20 (__input, __end) (() :> (S43, __p, __stk))
; S43 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S24 (__input, __end) (n :> (S43, __p, __stk))
; S43 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S32 (__input, __end) (n :> (S43, __p, __stk))
; S43 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S36 (__input, __end) (n :> (S43, __p, __stk))
; S44 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S17 (__input, __end) (() :> (S44, __p, __stk))
; S44 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S21 (__input, __end) (n :> (S44, __p, __stk))
; S44 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S29 (__input, __end) (n :> (S44, __p, __stk))
; S44 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S33 (__input, __end) (n :> (S44, __p, __stk))
; S45 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S18 (__input, __end) (() :> (S45, __p, __stk))
; S45 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S22 (__input, __end) (n :> (S45, __p, __stk))
; S45 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S30 (__input, __end) (n :> (S45, __p, __stk))
; S45 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S34 (__input, __end) (n :> (S45, __p, __stk))
; S46 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S19 (__input, __end) (() :> (S46, __p, __stk))
; S46 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S23 (__input, __end) (n :> (S46, __p, __stk))
; S46 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S31 (__input, __end) (n :> (S46, __p, __stk))
; S46 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S35 (__input, __end) (n :> (S46, __p, __stk))
; S47 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S20 (__input, __end) (() :> (S47, __p, __stk))
; S47 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S24 (__input, __end) (n :> (S47, __p, __stk))
; S47 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S32 (__input, __end) (n :> (S47, __p, __stk))
; S47 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S36 (__input, __end) (n :> (S47, __p, __stk))
; S48 ((__p,  ")") : __input, __end) __stk ->
    __runProgram S57 (__input, __end) (() :> (S48, __p, __stk))
; S49 ((__p,  ")") : __input, __end) __stk ->
    __runProgram S58 (__input, __end) (() :> (S49, __p, __stk))
; S50 ((__p,  ")") : __input, __end) __stk ->
    __runProgram S59 (__input, __end) (() :> (S50, __p, __stk))
; S51 ((__p,  ")") : __input, __end) __stk ->
    __runProgram S60 (__input, __end) (() :> (S51, __p, __stk))
; S61 ((__p,  "*") : __input, __end) __stk ->
    __runProgram S95 (__input, __end) (() :> (S61, __p, __stk))
; S62 ((__p,  "*") : __input, __end) __stk ->
    __runProgram S96 (__input, __end) (() :> (S62, __p, __stk))
; S63 ((__p,  "*") : __input, __end) __stk ->
    __runProgram S97 (__input, __end) (() :> (S63, __p, __stk))
; S64 ((__p,  "*") : __input, __end) __stk ->
    __runProgram S98 (__input, __end) (() :> (S64, __p, __stk))
; S65 ((__p,  "*") : __input, __end) __stk ->
    __runProgram S95 (__input, __end) (() :> (S65, __p, __stk))
; S66 ((__p,  "*") : __input, __end) __stk ->
    __runProgram S96 (__input, __end) (() :> (S66, __p, __stk))
; S67 ((__p,  "*") : __input, __end) __stk ->
    __runProgram S97 (__input, __end) (() :> (S67, __p, __stk))
; S68 ((__p,  "*") : __input, __end) __stk ->
    __runProgram S98 (__input, __end) (() :> (S68, __p, __stk))
; S73 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S20 (__input, __end) (() :> (S73, __p, __stk))
; S73 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S24 (__input, __end) (n :> (S73, __p, __stk))
; S73 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S32 (__input, __end) (n :> (S73, __p, __stk))
; S73 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S36 (__input, __end) (n :> (S73, __p, __stk))
; S74 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S20 (__input, __end) (() :> (S74, __p, __stk))
; S74 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S24 (__input, __end) (n :> (S74, __p, __stk))
; S74 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S32 (__input, __end) (n :> (S74, __p, __stk))
; S74 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S36 (__input, __end) (n :> (S74, __p, __stk))
; S75 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S20 (__input, __end) (() :> (S75, __p, __stk))
; S75 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S24 (__input, __end) (n :> (S75, __p, __stk))
; S75 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S32 (__input, __end) (n :> (S75, __p, __stk))
; S75 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S36 (__input, __end) (n :> (S75, __p, __stk))
; S76 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S20 (__input, __end) (() :> (S76, __p, __stk))
; S76 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S24 (__input, __end) (n :> (S76, __p, __stk))
; S76 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S32 (__input, __end) (n :> (S76, __p, __stk))
; S76 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S36 (__input, __end) (n :> (S76, __p, __stk))
; S85 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S129 (__input, __end) (() :> (S85, __p, __stk))
; S86 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S130 (__input, __end) (() :> (S86, __p, __stk))
; S95 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S73 (__input, __end) (() :> (S95, __p, __stk))
; S95 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S77 (__input, __end) (n :> (S95, __p, __stk))
; S95 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S87 (__input, __end) (n :> (S95, __p, __stk))
; S95 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S91 (__input, __end) (n :> (S95, __p, __stk))
; S96 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S74 (__input, __end) (() :> (S96, __p, __stk))
; S96 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S78 (__input, __end) (n :> (S96, __p, __stk))
; S96 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S88 (__input, __end) (n :> (S96, __p, __stk))
; S96 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S92 (__input, __end) (n :> (S96, __p, __stk))
; S97 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S75 (__input, __end) (() :> (S97, __p, __stk))
; S97 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S79 (__input, __end) (n :> (S97, __p, __stk))
; S97 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S89 (__input, __end) (n :> (S97, __p, __stk))
; S97 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S93 (__input, __end) (n :> (S97, __p, __stk))
; S98 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S76 (__input, __end) (() :> (S98, __p, __stk))
; S98 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S80 (__input, __end) (n :> (S98, __p, __stk))
; S98 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S90 (__input, __end) (n :> (S98, __p, __stk))
; S98 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S94 (__input, __end) (n :> (S98, __p, __stk))
; S99 ((__p,  ")") : __input, __end) __stk ->
    __runProgram S107 (__input, __end) (() :> (S99, __p, __stk))
; S100 ((__p,  ")") : __input, __end) __stk ->
    __runProgram S108 (__input, __end) (() :> (S100, __p, __stk))
; S101 ((__p,  ")") : __input, __end) __stk ->
    __runProgram S109 (__input, __end) (() :> (S101, __p, __stk))
; S102 ((__p,  ")") : __input, __end) __stk ->
    __runProgram S110 (__input, __end) (() :> (S102, __p, __stk))
; S111 ((__p,  "+") : __input, __end) __stk ->
    __runProgram S116 (__input, __end) (() :> (S111, __p, __stk))
; S111 ((__p,  "=") : __input, __end) __stk ->
    __runProgram S141 (__input, __end) (() :> (S111, __p, __stk))
; S112 ((__p,  "+") : __input, __end) __stk ->
    __runProgram S117 (__input, __end) (() :> (S112, __p, __stk))
; S112 ((__p,  "=") : __input, __end) __stk ->
    __runProgram S142 (__input, __end) (() :> (S112, __p, __stk))
; S113 ((__p,  "+") : __input, __end) __stk ->
    __runProgram S115 (__input, __end) (() :> (S113, __p, __stk))
; S114 ((__p,  "+") : __input, __end) __stk ->
    __runProgram S118 (__input, __end) (() :> (S114, __p, __stk))
; S115 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S73 (__input, __end) (() :> (S115, __p, __stk))
; S115 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S77 (__input, __end) (n :> (S115, __p, __stk))
; S115 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S87 (__input, __end) (n :> (S115, __p, __stk))
; S115 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S91 (__input, __end) (n :> (S115, __p, __stk))
; S116 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S74 (__input, __end) (() :> (S116, __p, __stk))
; S116 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S78 (__input, __end) (n :> (S116, __p, __stk))
; S116 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S88 (__input, __end) (n :> (S116, __p, __stk))
; S116 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S92 (__input, __end) (n :> (S116, __p, __stk))
; S117 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S75 (__input, __end) (() :> (S117, __p, __stk))
; S117 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S79 (__input, __end) (n :> (S117, __p, __stk))
; S117 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S89 (__input, __end) (n :> (S117, __p, __stk))
; S117 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S93 (__input, __end) (n :> (S117, __p, __stk))
; S118 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S76 (__input, __end) (() :> (S118, __p, __stk))
; S118 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S80 (__input, __end) (n :> (S118, __p, __stk))
; S118 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S90 (__input, __end) (n :> (S118, __p, __stk))
; S118 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S94 (__input, __end) (n :> (S118, __p, __stk))
; S119 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S127 (__input, __end) (n :> (S119, __p, __stk))
; S120 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S127 (__input, __end) (n :> (S120, __p, __stk))
; S123 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S127 (__input, __end) (n :> (S123, __p, __stk))
; S124 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S128 (__input, __end) (n :> (S124, __p, __stk))
; S127 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S129 (__input, __end) (() :> (S127, __p, __stk))
; S128 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S130 (__input, __end) (() :> (S128, __p, __stk))
; S129 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S19 (__input, __end) (() :> (S129, __p, __stk))
; S129 ((__p,  ")") : __input, __end) __stk ->
    __runProgram S137 (__input, __end) (() :> (S129, __p, __stk))
; S129 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S23 (__input, __end) (n :> (S129, __p, __stk))
; S129 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S31 (__input, __end) (n :> (S129, __p, __stk))
; S129 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S35 (__input, __end) (n :> (S129, __p, __stk))
; S130 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S19 (__input, __end) (() :> (S130, __p, __stk))
; S130 ((__p,  ")") : __input, __end) __stk ->
    __runProgram S138 (__input, __end) (() :> (S130, __p, __stk))
; S130 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S23 (__input, __end) (n :> (S130, __p, __stk))
; S130 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S31 (__input, __end) (n :> (S130, __p, __stk))
; S130 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S35 (__input, __end) (n :> (S130, __p, __stk))
; S139 ((__p,  ")") : __input, __end) __stk ->
    __runProgram S143 (__input, __end) (() :> (S139, __p, __stk))
; S140 ((__p,  ")") : __input, __end) __stk ->
    __runProgram S144 (__input, __end) (() :> (S140, __p, __stk))
; S141 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S73 (__input, __end) (() :> (S141, __p, __stk))
; S141 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S77 (__input, __end) (n :> (S141, __p, __stk))
; S141 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S87 (__input, __end) (n :> (S141, __p, __stk))
; S141 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S91 (__input, __end) (n :> (S141, __p, __stk))
; S142 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S76 (__input, __end) (() :> (S142, __p, __stk))
; S142 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S80 (__input, __end) (n :> (S142, __p, __stk))
; S142 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S90 (__input, __end) (n :> (S142, __p, __stk))
; S142 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S94 (__input, __end) (n :> (S142, __p, __stk))
; S145 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S146 (__input, __end) (() :> (S145, __p, __stk))
; S146 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S19 (__input, __end) (() :> (S146, __p, __stk))
; S146 ((__p,  ")") : __input, __end) __stk ->
    __runProgram S148 (__input, __end) (() :> (S146, __p, __stk))
; S146 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S23 (__input, __end) (n :> (S146, __p, __stk))
; S146 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S31 (__input, __end) (n :> (S146, __p, __stk))
; S146 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S35 (__input, __end) (n :> (S146, __p, __stk))
; S149 ((__p,  ")") : __input, __end) __stk ->
    __runProgram S150 (__input, __end) (() :> (S149, __p, __stk))
; S151 ((__p,  ",") : __input, __end) __stk ->
    __runProgram S154 (__input, __end) (() :> (S151, __p, __stk))
; S152 ((__p,  ",") : __input, __end) __stk ->
    __runProgram S155 (__input, __end) (() :> (S152, __p, __stk))
; S153 ((__p,  ",") : __input, __end) __stk ->
    __runProgram S156 (__input, __end) (() :> (S153, __p, __stk))
; S154 ((__p,  "+") : __input, __end) __stk ->
    __runProgram S119 (__input, __end) (() :> (S154, __p, __stk))
; S154 ((__p,  "-") : __input, __end) __stk ->
    __runProgram S120 (__input, __end) (() :> (S154, __p, __stk))
; S155 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S74 (__input, __end) (() :> (S155, __p, __stk))
; S155 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S78 (__input, __end) (n :> (S155, __p, __stk))
; S155 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S85 (__input, __end) (n :> (S155, __p, __stk))
; S155 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S92 (__input, __end) (n :> (S155, __p, __stk))
; S155 ((__p,  "~") : __input, __end) __stk ->
    __runProgram S123 (__input, __end) (() :> (S155, __p, __stk))
; S156 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S75 (__input, __end) (() :> (S156, __p, __stk))
; S156 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S79 (__input, __end) (n :> (S156, __p, __stk))
; S156 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S86 (__input, __end) (n :> (S156, __p, __stk))
; S156 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S93 (__input, __end) (n :> (S156, __p, __stk))
; S156 ((__p,  "~") : __input, __end) __stk ->
    __runProgram S124 (__input, __end) (() :> (S156, __p, __stk))
; S160 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S145 (__input, __end) (n :> (S160, __p, __stk))
; S163 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S145 (__input, __end) (n :> (S163, __p, __stk))
; S166 ((__p,  "->") : __input, __end) __stk ->
    __runProgram S169 (__input, __end) (() :> (S166, __p, __stk))
; S166 ((__p,  ".") : __input, __end) __stk ->
    __runProgram S170 (__input, __end) (() :> (S166, __p, __stk))
; S166 ((__p,  "<-") : __input, __end) __stk ->
    __runProgram S171 (__input, __end) (() :> (S166, __p, __stk))
; S166 ((__p,  "=>") : __input, __end) __stk ->
    __runProgram S168 (__input, __end) (() :> (S166, __p, __stk))
; S168 ((__p,  "+") : __input, __end) __stk ->
    __runProgram S119 (__input, __end) (() :> (S168, __p, __stk))
; S168 ((__p,  "-") : __input, __end) __stk ->
    __runProgram S120 (__input, __end) (() :> (S168, __p, __stk))
; S169 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S75 (__input, __end) (() :> (S169, __p, __stk))
; S169 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S79 (__input, __end) (n :> (S169, __p, __stk))
; S169 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S86 (__input, __end) (n :> (S169, __p, __stk))
; S169 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S93 (__input, __end) (n :> (S169, __p, __stk))
; S169 ((__p,  "~") : __input, __end) __stk ->
    __runProgram S124 (__input, __end) (() :> (S169, __p, __stk))
; S171 ((__p,  "(") : __input, __end) __stk ->
    __runProgram S74 (__input, __end) (() :> (S171, __p, __stk))
; S171 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runProgram S78 (__input, __end) (n :> (S171, __p, __stk))
; S171 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runProgram S85 (__input, __end) (n :> (S171, __p, __stk))
; S171 ((__p, NumberLiteral n) : __input, __end) __stk ->
    __runProgram S92 (__input, __end) (n :> (S171, __p, __stk))
; S171 ((__p,  "~") : __input, __end) __stk ->
    __runProgram S123 (__input, __end) (() :> (S171, __p, __stk))
; S172 ((__p,  ".") : __input, __end) __stk ->
    __runProgram S175 (__input, __end) (() :> (S172, __p, __stk))
; S173 ((__p,  ".") : __input, __end) __stk ->
    __runProgram S177 (__input, __end) (() :> (S173, __p, __stk))
; S173 ((__p,  "=>") : __input, __end) __stk ->
    __runProgram S176 (__input, __end) (() :> (S173, __p, __stk))
; S174 ((__p,  ".") : __input, __end) __stk ->
    __runProgram S178 (__input, __end) (() :> (S174, __p, __stk))
; S176 ((__p,  "+") : __input, __end) __stk ->
    __runProgram S119 (__input, __end) (() :> (S176, __p, __stk))
; S176 ((__p,  "-") : __input, __end) __stk ->
    __runProgram S120 (__input, __end) (() :> (S176, __p, __stk))
; S179 ((__p,  ".") : __input, __end) __stk ->
    __runProgram S180 (__input, __end) (() :> (S179, __p, __stk))
; S0 ((__p,  ")") : __input, __end) (e :> __stk@(_, __pos, _)) ->
    __gotoExprs1 ((__p,  ")") : __input, __end) (action55 __pos e) __stk
; S1 ((__p,  ")") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoExpr ((__p,  ")") : __input, __end) (action59 __pos a) __stk
; S1 ((__p,  ",") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoExpr ((__p,  ",") : __input, __end) (action59 __pos a) __stk
; S2 ((__p,  ")") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoExpr ((__p,  ")") : __input, __end) (action59 __pos a) __stk
; S3 ((__p,  ")") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoExpr ((__p,  ")") : __input, __end) (action58 __pos a
                                                              b) __stk
; S4 ((__p,  ")") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoExpr ((__p,  ")") : __input, __end) (action58 __pos a
                                                              b) __stk
; S4 ((__p,  ",") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoExpr ((__p,  ",") : __input, __end) (action58 __pos a
                                                              b) __stk
; S5 ((__p,  ")") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ")") : __input, __end) (action63 __pos a) __stk
; S5 ((__p,  "+") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "+") : __input, __end) (action63 __pos a) __stk
; S6 ((__p,  ")") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ")") : __input, __end) (action63 __pos a) __stk
; S6 ((__p,  "+") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "+") : __input, __end) (action63 __pos a) __stk
; S6 ((__p,  ",") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ",") : __input, __end) (action63 __pos a) __stk
; S7 ((__p,  ")") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ")") : __input, __end) (action63 __pos a) __stk
; S7 ((__p,  "+") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "+") : __input, __end) (action63 __pos a) __stk
; S7 ((__p,  ",") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ",") : __input, __end) (action63 __pos a) __stk
; S7 ((__p,  "=") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "=") : __input, __end) (action63 __pos a) __stk
; S8 ((__p,  ")") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ")") : __input, __end) (action63 __pos a) __stk
; S8 ((__p,  "+") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "+") : __input, __end) (action63 __pos a) __stk
; S8 ((__p,  "=") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "=") : __input, __end) (action63 __pos a) __stk
; S9 ((__p,  ")") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ")") : __input, __end) (action62 __pos a b) __stk
; S9 ((__p,  "+") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "+") : __input, __end) (action62 __pos a b) __stk
; S10 ((__p,  ")") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ")") : __input, __end) (action62 __pos a b) __stk
; S10 ((__p,  "+") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "+") : __input, __end) (action62 __pos a b) __stk
; S10 ((__p,  ",") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ",") : __input, __end) (action62 __pos a b) __stk
; S11 ((__p,  ")") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ")") : __input, __end) (action62 __pos a b) __stk
; S11 ((__p,  "+") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "+") : __input, __end) (action62 __pos a b) __stk
; S11 ((__p,  ",") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ",") : __input, __end) (action62 __pos a b) __stk
; S11 ((__p,  "=") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "=") : __input, __end) (action62 __pos a b) __stk
; S12 ((__p,  ")") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ")") : __input, __end) (action62 __pos a b) __stk
; S12 ((__p,  "+") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "+") : __input, __end) (action62 __pos a b) __stk
; S12 ((__p,  "=") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "=") : __input, __end) (action62 __pos a b) __stk
; S13 ((__p,  ")") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ")") : __input, __end) (action67 __pos a) __stk
; S13 ((__p,  "*") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "*") : __input, __end) (action67 __pos a) __stk
; S13 ((__p,  "+") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "+") : __input, __end) (action67 __pos a) __stk
; S14 ((__p,  ")") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ")") : __input, __end) (action67 __pos a) __stk
; S14 ((__p,  "*") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "*") : __input, __end) (action67 __pos a) __stk
; S14 ((__p,  "+") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "+") : __input, __end) (action67 __pos a) __stk
; S14 ((__p,  ",") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ",") : __input, __end) (action67 __pos a) __stk
; S15 ((__p,  ")") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ")") : __input, __end) (action67 __pos a) __stk
; S15 ((__p,  "*") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "*") : __input, __end) (action67 __pos a) __stk
; S15 ((__p,  "+") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "+") : __input, __end) (action67 __pos a) __stk
; S15 ((__p,  ",") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ",") : __input, __end) (action67 __pos a) __stk
; S15 ((__p,  "=") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "=") : __input, __end) (action67 __pos a) __stk
; S16 ((__p,  ")") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ")") : __input, __end) (action67 __pos a) __stk
; S16 ((__p,  "*") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "*") : __input, __end) (action67 __pos a) __stk
; S16 ((__p,  "+") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "+") : __input, __end) (action67 __pos a) __stk
; S16 ((__p,  "=") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "=") : __input, __end) (action67 __pos a) __stk
; S21 ((__p,  ")") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ")") : __input, __end) (action71 __pos n) __stk
; S21 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action71 __pos n) __stk
; S21 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action71 __pos n) __stk
; S22 ((__p,  ")") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ")") : __input, __end) (action71 __pos n) __stk
; S22 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action71 __pos n) __stk
; S22 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action71 __pos n) __stk
; S22 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ",") : __input, __end) (action71 __pos n) __stk
; S23 ((__p,  ")") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ")") : __input, __end) (action71 __pos n) __stk
; S23 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action71 __pos n) __stk
; S23 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action71 __pos n) __stk
; S23 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ",") : __input, __end) (action71 __pos n) __stk
; S23 ((__p,  "=") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "=") : __input, __end) (action71 __pos n) __stk
; S24 ((__p,  ")") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ")") : __input, __end) (action71 __pos n) __stk
; S24 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action71 __pos n) __stk
; S24 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action71 __pos n) __stk
; S24 ((__p,  "=") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "=") : __input, __end) (action71 __pos n) __stk
; S25 ((__p,  ")") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ")") : __input, __end) (action72 __pos n) __stk
; S25 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action72 __pos n) __stk
; S25 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action72 __pos n) __stk
; S26 ((__p,  ")") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ")") : __input, __end) (action72 __pos n) __stk
; S26 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action72 __pos n) __stk
; S26 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action72 __pos n) __stk
; S26 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ",") : __input, __end) (action72 __pos n) __stk
; S27 ((__p,  ")") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ")") : __input, __end) (action72 __pos n) __stk
; S27 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action72 __pos n) __stk
; S27 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action72 __pos n) __stk
; S27 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ",") : __input, __end) (action72 __pos n) __stk
; S27 ((__p,  "=") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "=") : __input, __end) (action72 __pos n) __stk
; S28 ((__p,  ")") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ")") : __input, __end) (action72 __pos n) __stk
; S28 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action72 __pos n) __stk
; S28 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action72 __pos n) __stk
; S28 ((__p,  "=") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "=") : __input, __end) (action72 __pos n) __stk
; S29 ((__p,  ")") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ")") : __input, __end) (action75 __pos n) __stk
; S29 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "*") : __input, __end) (action75 __pos n) __stk
; S29 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "+") : __input, __end) (action75 __pos n) __stk
; S30 ((__p,  ")") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ")") : __input, __end) (action75 __pos n) __stk
; S30 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "*") : __input, __end) (action75 __pos n) __stk
; S30 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "+") : __input, __end) (action75 __pos n) __stk
; S30 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ",") : __input, __end) (action75 __pos n) __stk
; S31 ((__p,  ")") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ")") : __input, __end) (action75 __pos n) __stk
; S31 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "*") : __input, __end) (action75 __pos n) __stk
; S31 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "+") : __input, __end) (action75 __pos n) __stk
; S31 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ",") : __input, __end) (action75 __pos n) __stk
; S31 ((__p,  "=") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "=") : __input, __end) (action75 __pos n) __stk
; S32 ((__p,  ")") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ")") : __input, __end) (action75 __pos n) __stk
; S32 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "*") : __input, __end) (action75 __pos n) __stk
; S32 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "+") : __input, __end) (action75 __pos n) __stk
; S32 ((__p,  "=") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "=") : __input, __end) (action75 __pos n) __stk
; S33 ((__p,  ")") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ")") : __input, __end) (action76 __pos n) __stk
; S33 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "*") : __input, __end) (action76 __pos n) __stk
; S33 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "+") : __input, __end) (action76 __pos n) __stk
; S34 ((__p,  ")") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ")") : __input, __end) (action76 __pos n) __stk
; S34 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "*") : __input, __end) (action76 __pos n) __stk
; S34 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "+") : __input, __end) (action76 __pos n) __stk
; S34 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ",") : __input, __end) (action76 __pos n) __stk
; S35 ((__p,  ")") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ")") : __input, __end) (action76 __pos n) __stk
; S35 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "*") : __input, __end) (action76 __pos n) __stk
; S35 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "+") : __input, __end) (action76 __pos n) __stk
; S35 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ",") : __input, __end) (action76 __pos n) __stk
; S35 ((__p,  "=") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "=") : __input, __end) (action76 __pos n) __stk
; S36 ((__p,  ")") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ")") : __input, __end) (action76 __pos n) __stk
; S36 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "*") : __input, __end) (action76 __pos n) __stk
; S36 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "+") : __input, __end) (action76 __pos n) __stk
; S36 ((__p,  "=") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "=") : __input, __end) (action76 __pos n) __stk
; S52 ((__p,  ")") : __input, __end) (es :> _ :? e :? __stk@(_, __pos, _)) ->
    __gotoExprs1 ((__p,  ")") : __input, __end) (action54 __pos e
                                                                es) __stk
; S53 ((__p,  ")") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ")") : __input, __end) (action66 __pos a
                                                              b) __stk
; S53 ((__p,  "*") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "*") : __input, __end) (action66 __pos a
                                                              b) __stk
; S53 ((__p,  "+") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "+") : __input, __end) (action66 __pos a
                                                              b) __stk
; S54 ((__p,  ")") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ")") : __input, __end) (action66 __pos a
                                                              b) __stk
; S54 ((__p,  "*") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "*") : __input, __end) (action66 __pos a
                                                              b) __stk
; S54 ((__p,  "+") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "+") : __input, __end) (action66 __pos a
                                                              b) __stk
; S54 ((__p,  ",") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ",") : __input, __end) (action66 __pos a
                                                              b) __stk
; S55 ((__p,  ")") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ")") : __input, __end) (action66 __pos a
                                                              b) __stk
; S55 ((__p,  "*") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "*") : __input, __end) (action66 __pos a
                                                              b) __stk
; S55 ((__p,  "+") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "+") : __input, __end) (action66 __pos a
                                                              b) __stk
; S55 ((__p,  ",") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ",") : __input, __end) (action66 __pos a
                                                              b) __stk
; S55 ((__p,  "=") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "=") : __input, __end) (action66 __pos a
                                                              b) __stk
; S56 ((__p,  ")") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ")") : __input, __end) (action66 __pos a
                                                              b) __stk
; S56 ((__p,  "*") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "*") : __input, __end) (action66 __pos a
                                                              b) __stk
; S56 ((__p,  "+") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "+") : __input, __end) (action66 __pos a
                                                              b) __stk
; S56 ((__p,  "=") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "=") : __input, __end) (action66 __pos a
                                                              b) __stk
; S57 ((__p,  ")") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ")") : __input, __end) (action70 __pos e) __stk
; S57 ((__p,  "*") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action70 __pos e) __stk
; S57 ((__p,  "+") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action70 __pos e) __stk
; S58 ((__p,  ")") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ")") : __input, __end) (action70 __pos e) __stk
; S58 ((__p,  "*") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action70 __pos e) __stk
; S58 ((__p,  "+") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action70 __pos e) __stk
; S58 ((__p,  ",") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ",") : __input, __end) (action70 __pos e) __stk
; S59 ((__p,  ")") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ")") : __input, __end) (action70 __pos e) __stk
; S59 ((__p,  "*") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action70 __pos e) __stk
; S59 ((__p,  "+") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action70 __pos e) __stk
; S59 ((__p,  ",") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ",") : __input, __end) (action70 __pos e) __stk
; S59 ((__p,  "=") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "=") : __input, __end) (action70 __pos e) __stk
; S60 ((__p,  ")") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ")") : __input, __end) (action70 __pos e) __stk
; S60 ((__p,  "*") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action70 __pos e) __stk
; S60 ((__p,  "+") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action70 __pos e) __stk
; S60 ((__p,  "=") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "=") : __input, __end) (action70 __pos e) __stk
; S61 ((__p,  "+") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "+") : __input, __end) (action63 __pos a) __stk
; S61 ((__p,  ",") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ",") : __input, __end) (action63 __pos a) __stk
; S61 ((__p,  ".") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ".") : __input, __end) (action63 __pos a) __stk
; S62 ((__p,  "+") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "+") : __input, __end) (action63 __pos a) __stk
; S62 ((__p,  ",") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ",") : __input, __end) (action63 __pos a) __stk
; S62 ((__p,  ".") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ".") : __input, __end) (action63 __pos a) __stk
; S62 ((__p,  "=") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "=") : __input, __end) (action63 __pos a) __stk
; S63 ((__p,  "+") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "+") : __input, __end) (action63 __pos a) __stk
; S63 ((__p,  ",") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ",") : __input, __end) (action63 __pos a) __stk
; S63 ((__p,  ".") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ".") : __input, __end) (action63 __pos a) __stk
; S63 ((__p,  "=") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "=") : __input, __end) (action63 __pos a) __stk
; S63 ((__p,  "=>") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "=>") : __input, __end) (action63 __pos a) __stk
; S64 ((__p,  "+") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "+") : __input, __end) (action63 __pos a) __stk
; S64 ((__p,  ",") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ",") : __input, __end) (action63 __pos a) __stk
; S64 ((__p,  ".") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ".") : __input, __end) (action63 __pos a) __stk
; S64 ((__p,  "=>") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "=>") : __input, __end) (action63 __pos a) __stk
; S65 ((__p,  "+") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "+") : __input, __end) (action62 __pos a b) __stk
; S65 ((__p,  ",") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ",") : __input, __end) (action62 __pos a b) __stk
; S65 ((__p,  ".") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ".") : __input, __end) (action62 __pos a b) __stk
; S66 ((__p,  "+") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "+") : __input, __end) (action62 __pos a b) __stk
; S66 ((__p,  ",") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ",") : __input, __end) (action62 __pos a b) __stk
; S66 ((__p,  ".") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ".") : __input, __end) (action62 __pos a b) __stk
; S66 ((__p,  "=") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "=") : __input, __end) (action62 __pos a b) __stk
; S67 ((__p,  "+") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "+") : __input, __end) (action62 __pos a b) __stk
; S67 ((__p,  ",") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ",") : __input, __end) (action62 __pos a b) __stk
; S67 ((__p,  ".") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ".") : __input, __end) (action62 __pos a b) __stk
; S67 ((__p,  "=") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "=") : __input, __end) (action62 __pos a b) __stk
; S67 ((__p,  "=>") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "=>") : __input, __end) (action62 __pos a
                                                              b) __stk
; S68 ((__p,  "+") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "+") : __input, __end) (action62 __pos a b) __stk
; S68 ((__p,  ",") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ",") : __input, __end) (action62 __pos a b) __stk
; S68 ((__p,  ".") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  ".") : __input, __end) (action62 __pos a b) __stk
; S68 ((__p,  "=>") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoAdd ((__p,  "=>") : __input, __end) (action62 __pos a
                                                              b) __stk
; S69 ((__p,  "*") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "*") : __input, __end) (action67 __pos a) __stk
; S69 ((__p,  "+") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "+") : __input, __end) (action67 __pos a) __stk
; S69 ((__p,  ",") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ",") : __input, __end) (action67 __pos a) __stk
; S69 ((__p,  ".") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ".") : __input, __end) (action67 __pos a) __stk
; S70 ((__p,  "*") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "*") : __input, __end) (action67 __pos a) __stk
; S70 ((__p,  "+") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "+") : __input, __end) (action67 __pos a) __stk
; S70 ((__p,  ",") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ",") : __input, __end) (action67 __pos a) __stk
; S70 ((__p,  ".") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ".") : __input, __end) (action67 __pos a) __stk
; S70 ((__p,  "=") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "=") : __input, __end) (action67 __pos a) __stk
; S71 ((__p,  "*") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "*") : __input, __end) (action67 __pos a) __stk
; S71 ((__p,  "+") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "+") : __input, __end) (action67 __pos a) __stk
; S71 ((__p,  ",") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ",") : __input, __end) (action67 __pos a) __stk
; S71 ((__p,  ".") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ".") : __input, __end) (action67 __pos a) __stk
; S71 ((__p,  "=") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "=") : __input, __end) (action67 __pos a) __stk
; S71 ((__p,  "=>") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "=>") : __input, __end) (action67 __pos a) __stk
; S72 ((__p,  "*") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "*") : __input, __end) (action67 __pos a) __stk
; S72 ((__p,  "+") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "+") : __input, __end) (action67 __pos a) __stk
; S72 ((__p,  ",") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ",") : __input, __end) (action67 __pos a) __stk
; S72 ((__p,  ".") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ".") : __input, __end) (action67 __pos a) __stk
; S72 ((__p,  "=>") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "=>") : __input, __end) (action67 __pos a) __stk
; S77 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action71 __pos n) __stk
; S77 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action71 __pos n) __stk
; S77 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ",") : __input, __end) (action71 __pos n) __stk
; S77 ((__p,  ".") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ".") : __input, __end) (action71 __pos n) __stk
; S78 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action71 __pos n) __stk
; S78 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action71 __pos n) __stk
; S78 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ",") : __input, __end) (action71 __pos n) __stk
; S78 ((__p,  ".") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ".") : __input, __end) (action71 __pos n) __stk
; S78 ((__p,  "=") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "=") : __input, __end) (action71 __pos n) __stk
; S79 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action71 __pos n) __stk
; S79 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action71 __pos n) __stk
; S79 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ",") : __input, __end) (action71 __pos n) __stk
; S79 ((__p,  ".") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ".") : __input, __end) (action71 __pos n) __stk
; S79 ((__p,  "=") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "=") : __input, __end) (action71 __pos n) __stk
; S79 ((__p,  "=>") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "=>") : __input, __end) (action71 __pos n) __stk
; S80 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action71 __pos n) __stk
; S80 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action71 __pos n) __stk
; S80 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ",") : __input, __end) (action71 __pos n) __stk
; S80 ((__p,  ".") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ".") : __input, __end) (action71 __pos n) __stk
; S80 ((__p,  "=>") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "=>") : __input, __end) (action71 __pos n) __stk
; S81 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action72 __pos n) __stk
; S81 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action72 __pos n) __stk
; S81 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ",") : __input, __end) (action72 __pos n) __stk
; S81 ((__p,  ".") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ".") : __input, __end) (action72 __pos n) __stk
; S82 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action72 __pos n) __stk
; S82 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action72 __pos n) __stk
; S82 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ",") : __input, __end) (action72 __pos n) __stk
; S82 ((__p,  ".") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ".") : __input, __end) (action72 __pos n) __stk
; S82 ((__p,  "=") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "=") : __input, __end) (action72 __pos n) __stk
; S83 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action72 __pos n) __stk
; S83 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action72 __pos n) __stk
; S83 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ",") : __input, __end) (action72 __pos n) __stk
; S83 ((__p,  ".") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ".") : __input, __end) (action72 __pos n) __stk
; S83 ((__p,  "=") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "=") : __input, __end) (action72 __pos n) __stk
; S83 ((__p,  "=>") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "=>") : __input, __end) (action72 __pos n) __stk
; S84 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action72 __pos n) __stk
; S84 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action72 __pos n) __stk
; S84 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ",") : __input, __end) (action72 __pos n) __stk
; S84 ((__p,  ".") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ".") : __input, __end) (action72 __pos n) __stk
; S84 ((__p,  "=>") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "=>") : __input, __end) (action72 __pos n) __stk
; S85 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "*") : __input, __end) (action75 __pos n) __stk
; S85 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "+") : __input, __end) (action75 __pos n) __stk
; S85 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ",") : __input, __end) (action75 __pos n) __stk
; S85 ((__p,  ".") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ".") : __input, __end) (action75 __pos n) __stk
; S85 ((__p,  "=") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "=") : __input, __end) (action75 __pos n) __stk
; S86 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "*") : __input, __end) (action75 __pos n) __stk
; S86 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "+") : __input, __end) (action75 __pos n) __stk
; S86 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ",") : __input, __end) (action75 __pos n) __stk
; S86 ((__p,  ".") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ".") : __input, __end) (action75 __pos n) __stk
; S86 ((__p,  "=") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "=") : __input, __end) (action75 __pos n) __stk
; S86 ((__p,  "=>") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "=>") : __input, __end) (action75 __pos n) __stk
; S87 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "*") : __input, __end) (action75 __pos n) __stk
; S87 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "+") : __input, __end) (action75 __pos n) __stk
; S87 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ",") : __input, __end) (action75 __pos n) __stk
; S87 ((__p,  ".") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ".") : __input, __end) (action75 __pos n) __stk
; S88 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "*") : __input, __end) (action75 __pos n) __stk
; S88 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "+") : __input, __end) (action75 __pos n) __stk
; S88 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ",") : __input, __end) (action75 __pos n) __stk
; S88 ((__p,  ".") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ".") : __input, __end) (action75 __pos n) __stk
; S88 ((__p,  "=") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "=") : __input, __end) (action75 __pos n) __stk
; S89 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "*") : __input, __end) (action75 __pos n) __stk
; S89 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "+") : __input, __end) (action75 __pos n) __stk
; S89 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ",") : __input, __end) (action75 __pos n) __stk
; S89 ((__p,  ".") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ".") : __input, __end) (action75 __pos n) __stk
; S89 ((__p,  "=") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "=") : __input, __end) (action75 __pos n) __stk
; S89 ((__p,  "=>") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "=>") : __input, __end) (action75 __pos n) __stk
; S90 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "*") : __input, __end) (action75 __pos n) __stk
; S90 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "+") : __input, __end) (action75 __pos n) __stk
; S90 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ",") : __input, __end) (action75 __pos n) __stk
; S90 ((__p,  ".") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ".") : __input, __end) (action75 __pos n) __stk
; S90 ((__p,  "=>") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "=>") : __input, __end) (action75 __pos n) __stk
; S91 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "*") : __input, __end) (action76 __pos n) __stk
; S91 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "+") : __input, __end) (action76 __pos n) __stk
; S91 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ",") : __input, __end) (action76 __pos n) __stk
; S91 ((__p,  ".") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ".") : __input, __end) (action76 __pos n) __stk
; S92 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "*") : __input, __end) (action76 __pos n) __stk
; S92 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "+") : __input, __end) (action76 __pos n) __stk
; S92 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ",") : __input, __end) (action76 __pos n) __stk
; S92 ((__p,  ".") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ".") : __input, __end) (action76 __pos n) __stk
; S92 ((__p,  "=") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "=") : __input, __end) (action76 __pos n) __stk
; S93 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "*") : __input, __end) (action76 __pos n) __stk
; S93 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "+") : __input, __end) (action76 __pos n) __stk
; S93 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ",") : __input, __end) (action76 __pos n) __stk
; S93 ((__p,  ".") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ".") : __input, __end) (action76 __pos n) __stk
; S93 ((__p,  "=") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "=") : __input, __end) (action76 __pos n) __stk
; S93 ((__p,  "=>") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "=>") : __input, __end) (action76 __pos n) __stk
; S94 ((__p,  "*") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "*") : __input, __end) (action76 __pos n) __stk
; S94 ((__p,  "+") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "+") : __input, __end) (action76 __pos n) __stk
; S94 ((__p,  ",") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ",") : __input, __end) (action76 __pos n) __stk
; S94 ((__p,  ".") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  ".") : __input, __end) (action76 __pos n) __stk
; S94 ((__p,  "=>") : __input, __end) (n :> __stk@(_, __pos, _)) ->
    __gotoConst ((__p,  "=>") : __input, __end) (action76 __pos n) __stk
; S103 ((__p,  "*") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "*") : __input, __end) (action66 __pos a
                                                              b) __stk
; S103 ((__p,  "+") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "+") : __input, __end) (action66 __pos a
                                                              b) __stk
; S103 ((__p,  ",") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ",") : __input, __end) (action66 __pos a
                                                              b) __stk
; S103 ((__p,  ".") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ".") : __input, __end) (action66 __pos a
                                                              b) __stk
; S104 ((__p,  "*") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "*") : __input, __end) (action66 __pos a
                                                              b) __stk
; S104 ((__p,  "+") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "+") : __input, __end) (action66 __pos a
                                                              b) __stk
; S104 ((__p,  ",") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ",") : __input, __end) (action66 __pos a
                                                              b) __stk
; S104 ((__p,  ".") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ".") : __input, __end) (action66 __pos a
                                                              b) __stk
; S104 ((__p,  "=") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "=") : __input, __end) (action66 __pos a
                                                              b) __stk
; S105 ((__p,  "*") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "*") : __input, __end) (action66 __pos a
                                                              b) __stk
; S105 ((__p,  "+") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "+") : __input, __end) (action66 __pos a
                                                              b) __stk
; S105 ((__p,  ",") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ",") : __input, __end) (action66 __pos a
                                                              b) __stk
; S105 ((__p,  ".") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ".") : __input, __end) (action66 __pos a
                                                              b) __stk
; S105 ((__p,  "=") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "=") : __input, __end) (action66 __pos a
                                                              b) __stk
; S105 ((__p,  "=>") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "=>") : __input, __end) (action66 __pos a
                                                               b) __stk
; S106 ((__p,  "*") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "*") : __input, __end) (action66 __pos a
                                                              b) __stk
; S106 ((__p,  "+") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "+") : __input, __end) (action66 __pos a
                                                              b) __stk
; S106 ((__p,  ",") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ",") : __input, __end) (action66 __pos a
                                                              b) __stk
; S106 ((__p,  ".") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  ".") : __input, __end) (action66 __pos a
                                                              b) __stk
; S106 ((__p,  "=>") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoMult ((__p,  "=>") : __input, __end) (action66 __pos a
                                                               b) __stk
; S107 ((__p,  "*") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action70 __pos e) __stk
; S107 ((__p,  "+") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action70 __pos e) __stk
; S107 ((__p,  ",") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ",") : __input, __end) (action70 __pos e) __stk
; S107 ((__p,  ".") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ".") : __input, __end) (action70 __pos e) __stk
; S108 ((__p,  "*") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action70 __pos e) __stk
; S108 ((__p,  "+") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action70 __pos e) __stk
; S108 ((__p,  ",") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ",") : __input, __end) (action70 __pos e) __stk
; S108 ((__p,  ".") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ".") : __input, __end) (action70 __pos e) __stk
; S108 ((__p,  "=") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "=") : __input, __end) (action70 __pos e) __stk
; S109 ((__p,  "*") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action70 __pos e) __stk
; S109 ((__p,  "+") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action70 __pos e) __stk
; S109 ((__p,  ",") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ",") : __input, __end) (action70 __pos e) __stk
; S109 ((__p,  ".") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ".") : __input, __end) (action70 __pos e) __stk
; S109 ((__p,  "=") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "=") : __input, __end) (action70 __pos e) __stk
; S109 ((__p,  "=>") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "=>") : __input, __end) (action70 __pos e) __stk
; S110 ((__p,  "*") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "*") : __input, __end) (action70 __pos e) __stk
; S110 ((__p,  "+") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "+") : __input, __end) (action70 __pos e) __stk
; S110 ((__p,  ",") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ",") : __input, __end) (action70 __pos e) __stk
; S110 ((__p,  ".") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  ".") : __input, __end) (action70 __pos e) __stk
; S110 ((__p,  "=>") : __input, __end) (_ :> e :? _ :? __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "=>") : __input, __end) (action70 __pos e) __stk
; S111 ((__p,  ",") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoExpr ((__p,  ",") : __input, __end) (action59 __pos a) __stk
; S111 ((__p,  ".") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoExpr ((__p,  ".") : __input, __end) (action59 __pos a) __stk
; S112 ((__p,  ",") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoExpr ((__p,  ",") : __input, __end) (action59 __pos a) __stk
; S112 ((__p,  ".") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoExpr ((__p,  ".") : __input, __end) (action59 __pos a) __stk
; S112 ((__p,  "=>") : __input, __end) (a :> __stk@(_, __pos, _)) ->
    __gotoExpr ((__p,  "=>") : __input, __end) (action59 __pos a) __stk
; S113 ((__p,  ",") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoExpr ((__p,  ",") : __input, __end) (action58 __pos a
                                                              b) __stk
; S113 ((__p,  ".") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoExpr ((__p,  ".") : __input, __end) (action58 __pos a
                                                              b) __stk
; S114 ((__p,  ",") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoExpr ((__p,  ",") : __input, __end) (action58 __pos a
                                                              b) __stk
; S114 ((__p,  ".") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoExpr ((__p,  ".") : __input, __end) (action58 __pos a
                                                              b) __stk
; S114 ((__p,  "=>") : __input, __end) (b :> _ :? a :? __stk@(_, __pos, _)) ->
    __gotoExpr ((__p,  "=>") : __input, __end) (action58 __pos a
                                                               b) __stk
; S121 ((__p,  ",") : __input, __end) (c :> __stk@(_, __pos, _)) ->
    __gotoCond ((__p,  ",") : __input, __end) (action42 __pos c) __stk
; S121 ((__p,  ".") : __input, __end) (c :> __stk@(_, __pos, _)) ->
    __gotoCond ((__p,  ".") : __input, __end) (action42 __pos c) __stk
; S122 ((__p,  ",") : __input, __end) (c :> __stk@(_, __pos, _)) ->
    __gotoCond ((__p,  ",") : __input, __end) (action42 __pos c) __stk
; S122 ((__p,  ".") : __input, __end) (c :> __stk@(_, __pos, _)) ->
    __gotoCond ((__p,  ".") : __input, __end) (action42 __pos c) __stk
; S122 ((__p,  "=>") : __input, __end) (c :> __stk@(_, __pos, _)) ->
    __gotoCond ((__p,  "=>") : __input, __end) (action42 __pos c) __stk
; S125 ((__p,  ",") : __input, __end) (e :> __stk@(_, __pos, _)) ->
    __gotoCond ((__p,  ",") : __input, __end) (action44 __pos e) __stk
; S125 ((__p,  ".") : __input, __end) (e :> __stk@(_, __pos, _)) ->
    __gotoCond ((__p,  ".") : __input, __end) (action44 __pos e) __stk
; S126 ((__p,  ",") : __input, __end) (e :> __stk@(_, __pos, _)) ->
    __gotoCond ((__p,  ",") : __input, __end) (action44 __pos e) __stk
; S126 ((__p,  ".") : __input, __end) (e :> __stk@(_, __pos, _)) ->
    __gotoCond ((__p,  ".") : __input, __end) (action44 __pos e) __stk
; S126 ((__p,  "=>") : __input, __end) (e :> __stk@(_, __pos, _)) ->
    __gotoCond ((__p,  "=>") : __input, __end) (action44 __pos e) __stk
; S131 ((__p,  ",") : __input, __end) (c :> _ :? __stk@(_, __pos, _)) ->
    __gotoChange ((__p,  ",") : __input, __end) (action34 __pos c) __stk
; S131 ((__p,  ".") : __input, __end) (c :> _ :? __stk@(_, __pos, _)) ->
    __gotoChange ((__p,  ".") : __input, __end) (action34 __pos c) __stk
; S132 ((__p,  ",") : __input, __end) (c :> _ :? __stk@(_, __pos, _)) ->
    __gotoChange ((__p,  ",") : __input, __end) (action35 __pos c) __stk
; S132 ((__p,  ".") : __input, __end) (c :> _ :? __stk@(_, __pos, _)) ->
    __gotoChange ((__p,  ".") : __input, __end) (action35 __pos c) __stk
; S133 ((__p,  ",") : __input, __end) (c :> _ :? __stk@(_, __pos, _)) ->
    __gotoCond ((__p,  ",") : __input, __end) (action43 __pos c) __stk
; S133 ((__p,  ".") : __input, __end) (c :> _ :? __stk@(_, __pos, _)) ->
    __gotoCond ((__p,  ".") : __input, __end) (action43 __pos c) __stk
; S134 ((__p,  ",") : __input, __end) (c :> _ :? __stk@(_, __pos, _)) ->
    __gotoCond ((__p,  ",") : __input, __end) (action43 __pos c) __stk
; S134 ((__p,  ".") : __input, __end) (c :> _ :? __stk@(_, __pos, _)) ->
    __gotoCond ((__p,  ".") : __input, __end) (action43 __pos c) __stk
; S134 ((__p,  "=>") : __input, __end) (c :> _ :? __stk@(_, __pos, _)) ->
    __gotoCond ((__p,  "=>") : __input, __end) (action43 __pos c) __stk
; S135 ((__p,  ",") : __input, __end) (t :> pred :? __stk@(_, __pos, _)) ->
    __gotoCall ((__p,  ",") : __input, __end) (action47 __pos pred
                                                              t) __stk
; S135 ((__p,  ".") : __input, __end) (t :> pred :? __stk@(_, __pos, _)) ->
    __gotoCall ((__p,  ".") : __input, __end) (action47 __pos pred
                                                              t) __stk
; S136 ((__p,  ",") : __input, __end) (t :> pred :? __stk@(_, __pos, _)) ->
    __gotoCall ((__p,  ",") : __input, __end) (action47 __pos pred
                                                              t) __stk
; S136 ((__p,  ".") : __input, __end) (t :> pred :? __stk@(_, __pos, _)) ->
    __gotoCall ((__p,  ".") : __input, __end) (action47 __pos pred
                                                              t) __stk
; S136 ((__p,  "=>") : __input, __end) (t :> pred :? __stk@(_, __pos, _)) ->
    __gotoCall ((__p,  "=>") : __input, __end) (action47 __pos pred
                                                               t) __stk
; S137 ((__p,  ",") : __input, __end) (_ :> _ :? __stk@(_, __pos, _)) ->
    __gotoTuple ((__p,  ",") : __input, __end) (action50 __pos ) __stk
; S137 ((__p,  ".") : __input, __end) (_ :> _ :? __stk@(_, __pos, _)) ->
    __gotoTuple ((__p,  ".") : __input, __end) (action50 __pos ) __stk
; S138 ((__p,  ",") : __input, __end) (_ :> _ :? __stk@(_, __pos, _)) ->
    __gotoTuple ((__p,  ",") : __input, __end) (action50 __pos ) __stk
; S138 ((__p,  ".") : __input, __end) (_ :> _ :? __stk@(_, __pos, _)) ->
    __gotoTuple ((__p,  ".") : __input, __end) (action50 __pos ) __stk
; S138 ((__p,  "=>") : __input, __end) (_ :> _ :? __stk@(_, __pos, _)) ->
    __gotoTuple ((__p,  "=>") : __input, __end) (action50 __pos ) __stk
; S143 ((__p,  ",") : __input, __end) (_ :> es :? _ :? __stk@(_, __pos, _)) ->
    __gotoTuple ((__p,  ",") : __input, __end) (action51 __pos es) __stk
; S143 ((__p,  ".") : __input, __end) (_ :> es :? _ :? __stk@(_, __pos, _)) ->
    __gotoTuple ((__p,  ".") : __input, __end) (action51 __pos es) __stk
; S144 ((__p,  ",") : __input, __end) (_ :> es :? _ :? __stk@(_, __pos, _)) ->
    __gotoTuple ((__p,  ",") : __input, __end) (action51 __pos es) __stk
; S144 ((__p,  ".") : __input, __end) (_ :> es :? _ :? __stk@(_, __pos, _)) ->
    __gotoTuple ((__p,  ".") : __input, __end) (action51 __pos es) __stk
; S144 ((__p,  "=>") : __input, __end) (_ :> es :? _ :? __stk@(_, __pos, _)) ->
    __gotoTuple ((__p,  "=>") : __input, __end) (action51 __pos es) __stk
; S147 ((__p,  "->") : __input, __end) (t :> pred :? __stk@(_, __pos, _)) ->
    __gotoCall ((__p,  "->") : __input, __end) (action47 __pos pred
                                                               t) __stk
; S147 ((__p,  ".") : __input, __end) (t :> pred :? __stk@(_, __pos, _)) ->
    __gotoCall ((__p,  ".") : __input, __end) (action47 __pos pred
                                                              t) __stk
; S147 ((__p,  "<-") : __input, __end) (t :> pred :? __stk@(_, __pos, _)) ->
    __gotoCall ((__p,  "<-") : __input, __end) (action47 __pos pred
                                                               t) __stk
; S147 ((__p,  "=>") : __input, __end) (t :> pred :? __stk@(_, __pos, _)) ->
    __gotoCall ((__p,  "=>") : __input, __end) (action47 __pos pred
                                                               t) __stk
; S148 ((__p,  "->") : __input, __end) (_ :> _ :? __stk@(_, __pos, _)) ->
    __gotoTuple ((__p,  "->") : __input, __end) (action50 __pos ) __stk
; S148 ((__p,  ".") : __input, __end) (_ :> _ :? __stk@(_, __pos, _)) ->
    __gotoTuple ((__p,  ".") : __input, __end) (action50 __pos ) __stk
; S148 ((__p,  "<-") : __input, __end) (_ :> _ :? __stk@(_, __pos, _)) ->
    __gotoTuple ((__p,  "<-") : __input, __end) (action50 __pos ) __stk
; S148 ((__p,  "=>") : __input, __end) (_ :> _ :? __stk@(_, __pos, _)) ->
    __gotoTuple ((__p,  "=>") : __input, __end) (action50 __pos ) __stk
; S150 ((__p,  "->") : __input, __end) (_ :> es :? _ :? __stk@(_, __pos, _)) ->
    __gotoTuple ((__p,  "->") : __input, __end) (action51 __pos es) __stk
; S150 ((__p,  ".") : __input, __end) (_ :> es :? _ :? __stk@(_, __pos, _)) ->
    __gotoTuple ((__p,  ".") : __input, __end) (action51 __pos es) __stk
; S150 ((__p,  "<-") : __input, __end) (_ :> es :? _ :? __stk@(_, __pos, _)) ->
    __gotoTuple ((__p,  "<-") : __input, __end) (action51 __pos es) __stk
; S150 ((__p,  "=>") : __input, __end) (_ :> es :? _ :? __stk@(_, __pos, _)) ->
    __gotoTuple ((__p,  "=>") : __input, __end) (action51 __pos es) __stk
; S151 ((__p,  ".") : __input, __end) (c :> __stk@(_, __pos, _)) ->
    __gotoChanges ((__p,  ".") : __input, __end) (action31 __pos c) __stk
; S152 ((__p,  ".") : __input, __end) (c :> __stk@(_, __pos, _)) ->
    __gotoConds ((__p,  ".") : __input, __end) (action39 __pos c) __stk
; S153 ((__p,  ".") : __input, __end) (c :> __stk@(_, __pos, _)) ->
    __gotoConds ((__p,  ".") : __input, __end) (action39 __pos c) __stk
; S153 ((__p,  "=>") : __input, __end) (c :> __stk@(_, __pos, _)) ->
    __gotoConds ((__p,  "=>") : __input, __end) (action39 __pos c) __stk
; S157 ((__p,  ".") : __input, __end) (cs :> _ :? c :? __stk@(_, __pos, _)) ->
    __gotoChanges ((__p,  ".") : __input, __end) (action30 __pos c
                                                                 cs) __stk
; S158 ((__p,  ".") : __input, __end) (cs :> _ :? c :? __stk@(_, __pos, _)) ->
    __gotoConds ((__p,  ".") : __input, __end) (action38 __pos c
                                                               cs) __stk
; S159 ((__p,  ".") : __input, __end) (cs :> _ :? c :? __stk@(_, __pos, _)) ->
    __gotoConds ((__p,  ".") : __input, __end) (action38 __pos c
                                                               cs) __stk
; S159 ((__p,  "=>") : __input, __end) (cs :> _ :? c :? __stk@(_, __pos, _)) ->
    __gotoConds ((__p,  "=>") : __input, __end) (action38 __pos c
                                                                cs) __stk
; S161 ([], __end) (res :> __stk@(_, __pos, _)) -> pure res
; S162 ([], __end) (stmts :> __stk@(_, __pos, _)) ->
    __gotoProgram ([], __end) (action10 __pos stmts) __stk
; S163 ([], __end) (c :> __stk@(_, __pos, _)) ->
    __gotoStmts ([], __end) (action14 __pos c) __stk
; S164 ([], __end) (c :> __stk@(_, __pos, _)) ->
    __gotoStmt ([], __end) (action17 __pos c) __stk
; S164 ((__p, LowercaseName tok) : __input, __end) (c :> __stk@(_, __pos, _)) ->
    __gotoStmt ((__p, LowercaseName tok) : __input, __end) (action17 __pos c) __stk
; S165 ([], __end) (e :> __stk@(_, __pos, _)) ->
    __gotoStmt ([], __end) (action18 __pos e) __stk
; S165 ((__p, LowercaseName tok) : __input, __end) (e :> __stk@(_, __pos, _)) ->
    __gotoStmt ((__p, LowercaseName tok) : __input, __end) (action18 __pos e) __stk
; S167 ([], __end) (cs :> c :? __stk@(_, __pos, _)) ->
    __gotoStmts ([], __end) (action13 __pos c cs) __stk
; S170 ([], __end) (_ :> c :? __stk@(_, __pos, _)) ->
    __gotoClause ([], __end) (action26 __pos c) __stk
; S170 ((__p, LowercaseName tok) : __input, __end) (_ :> c :? __stk@(_, __pos, _)) ->
    __gotoClause ((__p, LowercaseName tok) : __input, __end) (action26 __pos c) __stk
; S175 ([], __end) (_ :> ds :? _ :? c :? __stk@(_, __pos, _)) ->
    __gotoEffect ([], __end) (action21 __pos c ds) __stk
; S175 ((__p, LowercaseName tok) : __input, __end) (_ :> ds :? _ :? c :? __stk@(_, __pos, _)) ->
    __gotoEffect ((__p, LowercaseName tok) : __input, __end) (action21 __pos c
                                                                             ds) __stk
; S177 ([], __end) (_ :> cs :? _ :? c :? __stk@(_, __pos, _)) ->
    __gotoEffect ([], __end) (action23 __pos c cs) __stk
; S177 ((__p, LowercaseName tok) : __input, __end) (_ :> cs :? _ :? c :? __stk@(_, __pos, _)) ->
    __gotoEffect ((__p, LowercaseName tok) : __input, __end) (action23 __pos c
                                                                             cs) __stk
; S178 ([], __end) (_ :> cs :? _ :? c :? __stk@(_, __pos, _)) ->
    __gotoClause ([], __end) (action27 __pos c cs) __stk
; S178 ((__p, LowercaseName tok) : __input, __end) (_ :> cs :? _ :? c :? __stk@(_, __pos, _)) ->
    __gotoClause ((__p, LowercaseName tok) : __input, __end) (action27 __pos c
                                                                             cs) __stk
; S180 ([], __end) (_ :> ds :? _ :? cs :? _ :? c :? __stk@(_, __pos, _)) ->
    __gotoEffect ([], __end) (action22 __pos c cs ds) __stk
; S180 ((__p, LowercaseName tok) : __input, __end) (_ :> ds :? _ :? cs :? _ :? c :? __stk@(_, __pos, _)) ->
    __gotoEffect ((__p, LowercaseName tok) : __input, __end) (action22 __pos c
                                                                             cs ds) __stk
; S0 __input _ -> Left  (currentPos __input, [")", ","])
; S1 __input _ -> Left  (currentPos __input, [")", "+", ",", "="])
; S2 __input _ -> Left  (currentPos __input, [")", "+", "="])
; S3 __input _ -> Left  (currentPos __input, [")", "+"])
; S4 __input _ -> Left  (currentPos __input, [")", "+", ","])
; S5 __input _ -> Left  (currentPos __input, [")", "*", "+"])
; S6 __input _ -> Left  (currentPos __input, [")", "*", "+", ","])
; S7 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; S8 __input _ -> Left  (currentPos __input, [")", "*", "+", "="])
; S9 __input _ -> Left  (currentPos __input, [")", "*", "+"])
; S10 __input _ -> Left  (currentPos __input, [")", "*", "+", ","])
; S11 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; S12 __input _ -> Left  (currentPos __input, [")", "*", "+", "="])
; S13 __input _ -> Left  (currentPos __input, [")", "*", "+"])
; S14 __input _ -> Left  (currentPos __input, [")", "*", "+", ","])
; S15 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; S16 __input _ -> Left  (currentPos __input, [")", "*", "+", "="])
; S17 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S18 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S19 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S20 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S21 __input _ -> Left  (currentPos __input, [")", "*", "+"])
; S22 __input _ -> Left  (currentPos __input, [")", "*", "+", ","])
; S23 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; S24 __input _ -> Left  (currentPos __input, [")", "*", "+", "="])
; S25 __input _ -> Left  (currentPos __input, [")", "*", "+"])
; S26 __input _ -> Left  (currentPos __input, [")", "*", "+", ","])
; S27 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; S28 __input _ -> Left  (currentPos __input, [")", "*", "+", "="])
; S29 __input _ -> Left  (currentPos __input, [")", "*", "+"])
; S30 __input _ -> Left  (currentPos __input, [")", "*", "+", ","])
; S31 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; S32 __input _ -> Left  (currentPos __input, [")", "*", "+", "="])
; S33 __input _ -> Left  (currentPos __input, [")", "*", "+"])
; S34 __input _ -> Left  (currentPos __input, [")", "*", "+", ","])
; S35 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; S36 __input _ -> Left  (currentPos __input, [")", "*", "+", "="])
; S37 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S38 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S39 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S40 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S41 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S42 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S43 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S44 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S45 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S46 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S47 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S48 __input _ -> Left  (currentPos __input, [")"])
; S49 __input _ -> Left  (currentPos __input, [")"])
; S50 __input _ -> Left  (currentPos __input, [")"])
; S51 __input _ -> Left  (currentPos __input, [")"])
; S52 __input _ -> Left  (currentPos __input, [")"])
; S53 __input _ -> Left  (currentPos __input, [")", "*", "+"])
; S54 __input _ -> Left  (currentPos __input, [")", "*", "+", ","])
; S55 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; S56 __input _ -> Left  (currentPos __input, [")", "*", "+", "="])
; S57 __input _ -> Left  (currentPos __input, [")", "*", "+"])
; S58 __input _ -> Left  (currentPos __input, [")", "*", "+", ","])
; S59 __input _ ->
    Left  (currentPos __input, [")", "*", "+", ",", "="])
; S60 __input _ -> Left  (currentPos __input, [")", "*", "+", "="])
; S61 __input _ -> Left  (currentPos __input, ["*", "+", ",", "."])
; S62 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; S63 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; S64 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; S65 __input _ -> Left  (currentPos __input, ["*", "+", ",", "."])
; S66 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; S67 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; S68 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; S69 __input _ -> Left  (currentPos __input, ["*", "+", ",", "."])
; S70 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; S71 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; S72 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; S73 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S74 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S75 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S76 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S77 __input _ -> Left  (currentPos __input, ["*", "+", ",", "."])
; S78 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; S79 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; S80 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; S81 __input _ -> Left  (currentPos __input, ["*", "+", ",", "."])
; S82 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; S83 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; S84 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; S85 __input _ ->
    Left  (currentPos __input, ["(", "*", "+", ",", ".", "="])
; S86 __input _ ->
    Left  (currentPos __input, ["(", "*", "+", ",", ".", "=", "=>"])
; S87 __input _ -> Left  (currentPos __input, ["*", "+", ",", "."])
; S88 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; S89 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; S90 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; S91 __input _ -> Left  (currentPos __input, ["*", "+", ",", "."])
; S92 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; S93 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; S94 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; S95 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S96 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S97 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S98 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S99 __input _ -> Left  (currentPos __input, [")"])
; S100 __input _ -> Left  (currentPos __input, [")"])
; S101 __input _ -> Left  (currentPos __input, [")"])
; S102 __input _ -> Left  (currentPos __input, [")"])
; S103 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; S104 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; S105 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; S106 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; S107 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", "."])
; S108 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "="])
; S109 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=", "=>"])
; S110 __input _ ->
    Left  (currentPos __input, ["*", "+", ",", ".", "=>"])
; S111 __input _ ->
    Left  (currentPos __input, ["+", ",", ".", "="])
; S112 __input _ ->
    Left  (currentPos __input, ["+", ",", ".", "=", "=>"])
; S113 __input _ -> Left  (currentPos __input, ["+", ",", "."])
; S114 __input _ ->
    Left  (currentPos __input, ["+", ",", ".", "=>"])
; S115 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S116 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S117 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S118 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S119 __input _ -> Left  (currentPos __input, ["<name>"])
; S120 __input _ -> Left  (currentPos __input, ["<name>"])
; S121 __input _ -> Left  (currentPos __input, [",", "."])
; S122 __input _ -> Left  (currentPos __input, [",", ".", "=>"])
; S123 __input _ -> Left  (currentPos __input, ["<name>"])
; S124 __input _ -> Left  (currentPos __input, ["<name>"])
; S125 __input _ -> Left  (currentPos __input, [",", "."])
; S126 __input _ -> Left  (currentPos __input, [",", ".", "=>"])
; S127 __input _ -> Left  (currentPos __input, ["("])
; S128 __input _ -> Left  (currentPos __input, ["("])
; S129 __input _ ->
    Left  (currentPos __input, ["(", ")", "<Name>", "<name>", "<num>"])
; S130 __input _ ->
    Left  (currentPos __input, ["(", ")", "<Name>", "<name>", "<num>"])
; S131 __input _ -> Left  (currentPos __input, [",", "."])
; S132 __input _ -> Left  (currentPos __input, [",", "."])
; S133 __input _ -> Left  (currentPos __input, [",", "."])
; S134 __input _ -> Left  (currentPos __input, [",", ".", "=>"])
; S135 __input _ -> Left  (currentPos __input, [",", "."])
; S136 __input _ -> Left  (currentPos __input, [",", ".", "=>"])
; S137 __input _ -> Left  (currentPos __input, [",", "."])
; S138 __input _ -> Left  (currentPos __input, [",", ".", "=>"])
; S139 __input _ -> Left  (currentPos __input, [")"])
; S140 __input _ -> Left  (currentPos __input, [")"])
; S141 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S142 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>"])
; S143 __input _ -> Left  (currentPos __input, [",", "."])
; S144 __input _ -> Left  (currentPos __input, [",", ".", "=>"])
; S145 __input _ -> Left  (currentPos __input, ["("])
; S146 __input _ ->
    Left  (currentPos __input, ["(", ")", "<Name>", "<name>", "<num>"])
; S147 __input _ ->
    Left  (currentPos __input, ["->", ".", "<-", "=>"])
; S148 __input _ ->
    Left  (currentPos __input, ["->", ".", "<-", "=>"])
; S149 __input _ -> Left  (currentPos __input, [")"])
; S150 __input _ ->
    Left  (currentPos __input, ["->", ".", "<-", "=>"])
; S151 __input _ -> Left  (currentPos __input, [",", "."])
; S152 __input _ -> Left  (currentPos __input, [",", "."])
; S153 __input _ -> Left  (currentPos __input, [",", ".", "=>"])
; S154 __input _ -> Left  (currentPos __input, ["+", "-"])
; S155 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>", "~"])
; S156 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>", "~"])
; S157 __input _ -> Left  (currentPos __input, ["."])
; S158 __input _ -> Left  (currentPos __input, ["."])
; S159 __input _ -> Left  (currentPos __input, [".", "=>"])
; S160 __input _ -> Left  (currentPos __input, ["<name>"])
; S161 __input _ -> Left  (currentPos __input, ["<eof>"])
; S162 __input _ -> Left  (currentPos __input, ["<eof>"])
; S163 __input _ -> Left  (currentPos __input, ["<eof>", "<name>"])
; S164 __input _ -> Left  (currentPos __input, ["<eof>", "<name>"])
; S165 __input _ -> Left  (currentPos __input, ["<eof>", "<name>"])
; S166 __input _ ->
    Left  (currentPos __input, ["->", ".", "<-", "=>"])
; S167 __input _ -> Left  (currentPos __input, ["<eof>"])
; S168 __input _ -> Left  (currentPos __input, ["+", "-"])
; S169 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>", "~"])
; S170 __input _ -> Left  (currentPos __input, ["<eof>", "<name>"])
; S171 __input _ ->
    Left  (currentPos __input, ["(", "<Name>", "<name>", "<num>", "~"])
; S172 __input _ -> Left  (currentPos __input, ["."])
; S173 __input _ -> Left  (currentPos __input, [".", "=>"])
; S174 __input _ -> Left  (currentPos __input, ["."])
; S175 __input _ -> Left  (currentPos __input, ["<eof>", "<name>"])
; S176 __input _ -> Left  (currentPos __input, ["+", "-"])
; S177 __input _ -> Left  (currentPos __input, ["<eof>", "<name>"])
; S178 __input _ -> Left  (currentPos __input, ["<eof>", "<name>"])
; S179 __input _ -> Left  (currentPos __input, ["."])
; S180 __input _ -> Left  (currentPos __input, ["<eof>", "<name>"])
} where {
; action0 pos res =
res
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
; action47 pos pred t =
    Call pos pred t
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
}
  
currentPos :: ([Lexeme], Pos) -> Pos
currentPos = \case
  ([],           end) -> end
  ((pos, _) : _, _)   -> pos
  
parseProgram :: FilePath -> IO (Either LexerError (Either (Pos, [String]) Program))
parseProgram filepath = do
  text <- Text.readFile filepath
  case lexText filepath text ["(", ")", "*", "+", ",", "-", "->",
                              ".", "<-", "=", "=>", "~"] of
    Left  err   -> pure (Left err)
    Right input -> pure (Right (__runProgram  S160 input Nil))