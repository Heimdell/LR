{-# language PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Parser (parse) where
  
import Data.Text.IO.Utf8 qualified as Text
import Data.Kind qualified as Kind
import Backend.DefaultLexer
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
  S0 :: forall a. St (a)
  S1 :: forall a. St (Program : a)
  S2 :: forall a. St (Stmts : a)
  S3 :: forall a. St (Stmt : a)
  S4 :: forall a. St (Clause : a)
  S5 :: forall a. St (Effect : a)
  S6 :: forall a. St (Call : a)
  S7 :: forall a. St (Stmts : Stmt : a)
  S8 :: forall a. St (() : Call : a)
  S9 :: forall a. St (() : Call : a)
  S10 :: forall a. St (() : Call : a)
  S11 :: forall a. St (() : Call : a)
  S12 :: forall a. St (Changes : () : Call : a)
  S13 :: forall a. St (Conds : () : Call : a)
  S14 :: forall a. St (Conds : () : Call : a)
  S15 :: forall a. St (() : Changes : () : Call : a)
  S16 :: forall a. St (() : Conds : () : Call : a)
  S17 :: forall a. St (() : Conds : () : Call : a)
  S18 :: forall a. St (() : Conds : () : Call : a)
  S19 :: forall a. St (Changes : () : Conds : () : Call : a)
  S20 :: forall a. St (() : Changes : () : Conds : () : Call : a)
  S21 :: forall a. St (Expr : a)
  S22 :: forall a. St (ExprAdd : a)
  S23 :: forall a. St (ExprAdd : a)
  S24 :: forall a. St (ExprAdd : () : ExprAdd : a)
  S25 :: forall a. St (ExprAdd : () : ExprAdd : a)
  S26 :: forall a. St (ExprMult : a)
  S27 :: forall a. St (ExprMult : a)
  S28 :: forall a. St (ExprMult : a)
  S29 :: forall a. St (ExprMult : a)
  S30 :: forall a. St (ExprMult : () : ExprAdd : a)
  S31 :: forall a. St (ExprMult : () : ExprAdd : a)
  S32 :: forall a. St (ExprMult : () : ExprAdd : a)
  S33 :: forall a. St (ExprMult : () : ExprAdd : a)
  S34 :: forall a. St (ExprTerm : a)
  S35 :: forall a. St (ExprTerm : a)
  S36 :: forall a. St (ExprTerm : a)
  S37 :: forall a. St (ExprTerm : a)
  S38 :: forall a. St (() : a)
  S39 :: forall a. St (() : a)
  S40 :: forall a. St (() : a)
  S41 :: forall a. St (() : a)
  S42 :: forall a. St (Text : a)
  S43 :: forall a. St (Text : a)
  S44 :: forall a. St (Text : a)
  S45 :: forall a. St (Text : a)
  S46 :: forall a. St (Const : a)
  S47 :: forall a. St (Const : a)
  S48 :: forall a. St (Const : a)
  S49 :: forall a. St (Const : a)
  S50 :: forall a. St (Text : a)
  S51 :: forall a. St (Text : a)
  S52 :: forall a. St (Text : a)
  S53 :: forall a. St (Text : a)
  S54 :: forall a. St (Integer : a)
  S55 :: forall a. St (Integer : a)
  S56 :: forall a. St (Integer : a)
  S57 :: forall a. St (Integer : a)
  S58 :: forall a. St (() : Expr : a)
  S59 :: forall a. St (() : ExprAdd : a)
  S60 :: forall a. St (() : ExprAdd : a)
  S61 :: forall a. St (() : ExprAdd : a)
  S62 :: forall a. St (() : ExprAdd : a)
  S63 :: forall a. St (() : ExprAdd : a)
  S64 :: forall a. St (() : ExprAdd : a)
  S65 :: forall a. St (() : ExprMult : a)
  S66 :: forall a. St (() : ExprMult : a)
  S67 :: forall a. St (() : ExprMult : a)
  S68 :: forall a. St (() : ExprMult : a)
  S69 :: forall a. St (Expr : () : a)
  S70 :: forall a. St (Expr : () : a)
  S71 :: forall a. St (Expr : () : a)
  S72 :: forall a. St (Expr : () : a)
  S73 :: forall a. St (Exprs1 : () : Expr : a)
  S74 :: forall a. St (ExprTerm : () : ExprMult : a)
  S75 :: forall a. St (ExprTerm : () : ExprMult : a)
  S76 :: forall a. St (ExprTerm : () : ExprMult : a)
  S77 :: forall a. St (ExprTerm : () : ExprMult : a)
  S78 :: forall a. St (() : Expr : () : a)
  S79 :: forall a. St (() : Expr : () : a)
  S80 :: forall a. St (() : Expr : () : a)
  S81 :: forall a. St (() : Expr : () : a)
  S82 :: forall a. St (ExprMult : a)
  S83 :: forall a. St (ExprMult : a)
  S84 :: forall a. St (ExprMult : a)
  S85 :: forall a. St (ExprMult : a)
  S86 :: forall a. St (ExprMult : () : ExprAdd : a)
  S87 :: forall a. St (ExprMult : () : ExprAdd : a)
  S88 :: forall a. St (ExprMult : () : ExprAdd : a)
  S89 :: forall a. St (ExprMult : () : ExprAdd : a)
  S90 :: forall a. St (ExprTerm : a)
  S91 :: forall a. St (ExprTerm : a)
  S92 :: forall a. St (ExprTerm : a)
  S93 :: forall a. St (ExprTerm : a)
  S94 :: forall a. St (() : a)
  S95 :: forall a. St (() : a)
  S96 :: forall a. St (() : a)
  S97 :: forall a. St (() : a)
  S98 :: forall a. St (Text : a)
  S99 :: forall a. St (Text : a)
  S100 :: forall a. St (Text : a)
  S101 :: forall a. St (Text : a)
  S102 :: forall a. St (Const : a)
  S103 :: forall a. St (Const : a)
  S104 :: forall a. St (Const : a)
  S105 :: forall a. St (Const : a)
  S106 :: forall a. St (Text : a)
  S107 :: forall a. St (Text : a)
  S108 :: forall a. St (Text : a)
  S109 :: forall a. St (Text : a)
  S110 :: forall a. St (Text : a)
  S111 :: forall a. St (Text : a)
  S112 :: forall a. St (Integer : a)
  S113 :: forall a. St (Integer : a)
  S114 :: forall a. St (Integer : a)
  S115 :: forall a. St (Integer : a)
  S116 :: forall a. St (() : ExprMult : a)
  S117 :: forall a. St (() : ExprMult : a)
  S118 :: forall a. St (() : ExprMult : a)
  S119 :: forall a. St (() : ExprMult : a)
  S120 :: forall a. St (Expr : () : a)
  S121 :: forall a. St (Expr : () : a)
  S122 :: forall a. St (Expr : () : a)
  S123 :: forall a. St (Expr : () : a)
  S124 :: forall a. St (ExprTerm : () : ExprMult : a)
  S125 :: forall a. St (ExprTerm : () : ExprMult : a)
  S126 :: forall a. St (ExprTerm : () : ExprMult : a)
  S127 :: forall a. St (ExprTerm : () : ExprMult : a)
  S128 :: forall a. St (() : Expr : () : a)
  S129 :: forall a. St (() : Expr : () : a)
  S130 :: forall a. St (() : Expr : () : a)
  S131 :: forall a. St (() : Expr : () : a)
  S132 :: forall a. St (ExprAdd : a)
  S133 :: forall a. St (ExprAdd : a)
  S134 :: forall a. St (ExprAdd : () : ExprAdd : a)
  S135 :: forall a. St (ExprAdd : () : ExprAdd : a)
  S136 :: forall a. St (() : ExprAdd : a)
  S137 :: forall a. St (() : ExprAdd : a)
  S138 :: forall a. St (() : ExprAdd : a)
  S139 :: forall a. St (() : ExprAdd : a)
  S140 :: forall a. St (() : a)
  S141 :: forall a. St (() : a)
  S142 :: forall a. St (Call : a)
  S143 :: forall a. St (Call : a)
  S144 :: forall a. St (() : a)
  S145 :: forall a. St (() : a)
  S146 :: forall a. St (Expr : a)
  S147 :: forall a. St (Expr : a)
  S148 :: forall a. St (Text : a)
  S149 :: forall a. St (Text : a)
  S150 :: forall a. St (() : a)
  S151 :: forall a. St (() : a)
  S152 :: forall a. St (Call : () : a)
  S153 :: forall a. St (Call : () : a)
  S154 :: forall a. St (Call : () : a)
  S155 :: forall a. St (Call : () : a)
  S156 :: forall a. St (Tuple : Text : a)
  S157 :: forall a. St (Tuple : Text : a)
  S158 :: forall a. St (() : () : a)
  S159 :: forall a. St (() : () : a)
  S160 :: forall a. St (Exprs1 : () : a)
  S161 :: forall a. St (Exprs1 : () : a)
  S162 :: forall a. St (() : ExprAdd : a)
  S163 :: forall a. St (() : ExprAdd : a)
  S164 :: forall a. St (() : Exprs1 : () : a)
  S165 :: forall a. St (() : Exprs1 : () : a)
  S166 :: forall a. St (Text : a)
  S167 :: forall a. St (() : a)
  S168 :: forall a. St (Tuple : Text : a)
  S169 :: forall a. St (() : () : a)
  S170 :: forall a. St (Exprs1 : () : a)
  S171 :: forall a. St (() : Exprs1 : () : a)
  S172 :: forall a. St (Change : a)
  S173 :: forall a. St (Cond : a)
  S174 :: forall a. St (Cond : a)
  S175 :: forall a. St (() : Change : a)
  S176 :: forall a. St (() : Cond : a)
  S177 :: forall a. St (() : Cond : a)
  S178 :: forall a. St (Changes : () : Change : a)
  S179 :: forall a. St (Conds : () : Cond : a)
  S180 :: forall a. St (Conds : () : Cond : a)
  
gotoCall :: ([Lexeme], Pos) -> Call -> Stack a -> Either (Pos, [String]) Program
gotoCall toks term stk@(state, _, _) = case state of
  S0 -> run S6 toks (term :> stk)
  S3 -> run S6 toks (term :> stk)
  S9 -> run S143 toks (term :> stk)
  S11 -> run S142 toks (term :> stk)
  S140 -> run S152 toks (term :> stk)
  S141 -> run S153 toks (term :> stk)
  S144 -> run S154 toks (term :> stk)
  S145 -> run S155 toks (term :> stk)
  S176 -> run S142 toks (term :> stk)
  S177 -> run S143 toks (term :> stk)
  _ -> error ""

gotoChange :: ([Lexeme], Pos) -> Change -> Stack a -> Either (Pos, [String]) Program
gotoChange toks term stk@(state, _, _) = case state of
  S8 -> run S172 toks (term :> stk)
  S16 -> run S172 toks (term :> stk)
  S175 -> run S172 toks (term :> stk)
  _ -> error ""

gotoChanges :: ([Lexeme], Pos) -> Changes -> Stack a -> Either (Pos, [String]) Program
gotoChanges toks term stk@(state, _, _) = case state of
  S8 -> run S12 toks (term :> stk)
  S16 -> run S19 toks (term :> stk)
  S175 -> run S178 toks (term :> stk)
  _ -> error ""

gotoClause :: ([Lexeme], Pos) -> Clause -> Stack a -> Either (Pos, [String]) Program
gotoClause toks term stk@(state, _, _) = case state of
  S0 -> run S4 toks (term :> stk)
  S3 -> run S4 toks (term :> stk)
  _ -> error ""

gotoCond :: ([Lexeme], Pos) -> Cond -> Stack a -> Either (Pos, [String]) Program
gotoCond toks term stk@(state, _, _) = case state of
  S9 -> run S174 toks (term :> stk)
  S11 -> run S173 toks (term :> stk)
  S176 -> run S173 toks (term :> stk)
  S177 -> run S174 toks (term :> stk)
  _ -> error ""

gotoConds :: ([Lexeme], Pos) -> Conds -> Stack a -> Either (Pos, [String]) Program
gotoConds toks term stk@(state, _, _) = case state of
  S9 -> run S13 toks (term :> stk)
  S11 -> run S14 toks (term :> stk)
  S176 -> run S179 toks (term :> stk)
  S177 -> run S180 toks (term :> stk)
  _ -> error ""

gotoConst :: ([Lexeme], Pos) -> Const -> Stack a -> Either (Pos, [String]) Program
gotoConst toks term stk@(state, _, _) = case state of
  S9 -> run S104 toks (term :> stk)
  S11 -> run S103 toks (term :> stk)
  S38 -> run S49 toks (term :> stk)
  S39 -> run S49 toks (term :> stk)
  S40 -> run S49 toks (term :> stk)
  S41 -> run S49 toks (term :> stk)
  S58 -> run S48 toks (term :> stk)
  S59 -> run S46 toks (term :> stk)
  S60 -> run S47 toks (term :> stk)
  S61 -> run S46 toks (term :> stk)
  S62 -> run S47 toks (term :> stk)
  S63 -> run S48 toks (term :> stk)
  S64 -> run S49 toks (term :> stk)
  S65 -> run S46 toks (term :> stk)
  S66 -> run S47 toks (term :> stk)
  S67 -> run S48 toks (term :> stk)
  S68 -> run S49 toks (term :> stk)
  S94 -> run S49 toks (term :> stk)
  S95 -> run S49 toks (term :> stk)
  S96 -> run S49 toks (term :> stk)
  S97 -> run S49 toks (term :> stk)
  S116 -> run S102 toks (term :> stk)
  S117 -> run S103 toks (term :> stk)
  S118 -> run S104 toks (term :> stk)
  S119 -> run S105 toks (term :> stk)
  S136 -> run S102 toks (term :> stk)
  S137 -> run S103 toks (term :> stk)
  S138 -> run S104 toks (term :> stk)
  S139 -> run S105 toks (term :> stk)
  S150 -> run S48 toks (term :> stk)
  S151 -> run S48 toks (term :> stk)
  S162 -> run S102 toks (term :> stk)
  S163 -> run S105 toks (term :> stk)
  S167 -> run S48 toks (term :> stk)
  S176 -> run S103 toks (term :> stk)
  S177 -> run S104 toks (term :> stk)
  _ -> error ""

gotoEffect :: ([Lexeme], Pos) -> Effect -> Stack a -> Either (Pos, [String]) Program
gotoEffect toks term stk@(state, _, _) = case state of
  S0 -> run S5 toks (term :> stk)
  S3 -> run S5 toks (term :> stk)
  _ -> error ""

gotoExpr :: ([Lexeme], Pos) -> Expr -> Stack a -> Either (Pos, [String]) Program
gotoExpr toks term stk@(state, _, _) = case state of
  S9 -> run S147 toks (term :> stk)
  S11 -> run S146 toks (term :> stk)
  S38 -> run S69 toks (term :> stk)
  S39 -> run S70 toks (term :> stk)
  S40 -> run S71 toks (term :> stk)
  S41 -> run S72 toks (term :> stk)
  S58 -> run S21 toks (term :> stk)
  S94 -> run S120 toks (term :> stk)
  S95 -> run S121 toks (term :> stk)
  S96 -> run S122 toks (term :> stk)
  S97 -> run S123 toks (term :> stk)
  S150 -> run S21 toks (term :> stk)
  S151 -> run S21 toks (term :> stk)
  S167 -> run S21 toks (term :> stk)
  S176 -> run S146 toks (term :> stk)
  S177 -> run S147 toks (term :> stk)
  _ -> error ""

gotoExprAdd :: ([Lexeme], Pos) -> ExprAdd -> Stack a -> Either (Pos, [String]) Program
gotoExprAdd toks term stk@(state, _, _) = case state of
  S9 -> run S133 toks (term :> stk)
  S11 -> run S132 toks (term :> stk)
  S38 -> run S23 toks (term :> stk)
  S39 -> run S23 toks (term :> stk)
  S40 -> run S23 toks (term :> stk)
  S41 -> run S23 toks (term :> stk)
  S58 -> run S22 toks (term :> stk)
  S59 -> run S24 toks (term :> stk)
  S60 -> run S25 toks (term :> stk)
  S94 -> run S23 toks (term :> stk)
  S95 -> run S23 toks (term :> stk)
  S96 -> run S23 toks (term :> stk)
  S97 -> run S23 toks (term :> stk)
  S150 -> run S22 toks (term :> stk)
  S151 -> run S22 toks (term :> stk)
  S162 -> run S134 toks (term :> stk)
  S163 -> run S135 toks (term :> stk)
  S167 -> run S22 toks (term :> stk)
  S176 -> run S132 toks (term :> stk)
  S177 -> run S133 toks (term :> stk)
  _ -> error ""

gotoExprMult :: ([Lexeme], Pos) -> ExprMult -> Stack a -> Either (Pos, [String]) Program
gotoExprMult toks term stk@(state, _, _) = case state of
  S9 -> run S84 toks (term :> stk)
  S11 -> run S83 toks (term :> stk)
  S38 -> run S29 toks (term :> stk)
  S39 -> run S29 toks (term :> stk)
  S40 -> run S29 toks (term :> stk)
  S41 -> run S29 toks (term :> stk)
  S58 -> run S28 toks (term :> stk)
  S59 -> run S26 toks (term :> stk)
  S60 -> run S27 toks (term :> stk)
  S61 -> run S30 toks (term :> stk)
  S62 -> run S31 toks (term :> stk)
  S63 -> run S32 toks (term :> stk)
  S64 -> run S33 toks (term :> stk)
  S94 -> run S29 toks (term :> stk)
  S95 -> run S29 toks (term :> stk)
  S96 -> run S29 toks (term :> stk)
  S97 -> run S29 toks (term :> stk)
  S136 -> run S86 toks (term :> stk)
  S137 -> run S87 toks (term :> stk)
  S138 -> run S88 toks (term :> stk)
  S139 -> run S89 toks (term :> stk)
  S150 -> run S28 toks (term :> stk)
  S151 -> run S28 toks (term :> stk)
  S162 -> run S82 toks (term :> stk)
  S163 -> run S85 toks (term :> stk)
  S167 -> run S28 toks (term :> stk)
  S176 -> run S83 toks (term :> stk)
  S177 -> run S84 toks (term :> stk)
  _ -> error ""

gotoExprTerm :: ([Lexeme], Pos) -> ExprTerm -> Stack a -> Either (Pos, [String]) Program
gotoExprTerm toks term stk@(state, _, _) = case state of
  S9 -> run S92 toks (term :> stk)
  S11 -> run S91 toks (term :> stk)
  S38 -> run S37 toks (term :> stk)
  S39 -> run S37 toks (term :> stk)
  S40 -> run S37 toks (term :> stk)
  S41 -> run S37 toks (term :> stk)
  S58 -> run S36 toks (term :> stk)
  S59 -> run S34 toks (term :> stk)
  S60 -> run S35 toks (term :> stk)
  S61 -> run S34 toks (term :> stk)
  S62 -> run S35 toks (term :> stk)
  S63 -> run S36 toks (term :> stk)
  S64 -> run S37 toks (term :> stk)
  S65 -> run S74 toks (term :> stk)
  S66 -> run S75 toks (term :> stk)
  S67 -> run S76 toks (term :> stk)
  S68 -> run S77 toks (term :> stk)
  S94 -> run S37 toks (term :> stk)
  S95 -> run S37 toks (term :> stk)
  S96 -> run S37 toks (term :> stk)
  S97 -> run S37 toks (term :> stk)
  S116 -> run S124 toks (term :> stk)
  S117 -> run S125 toks (term :> stk)
  S118 -> run S126 toks (term :> stk)
  S119 -> run S127 toks (term :> stk)
  S136 -> run S90 toks (term :> stk)
  S137 -> run S91 toks (term :> stk)
  S138 -> run S92 toks (term :> stk)
  S139 -> run S93 toks (term :> stk)
  S150 -> run S36 toks (term :> stk)
  S151 -> run S36 toks (term :> stk)
  S162 -> run S90 toks (term :> stk)
  S163 -> run S93 toks (term :> stk)
  S167 -> run S36 toks (term :> stk)
  S176 -> run S91 toks (term :> stk)
  S177 -> run S92 toks (term :> stk)
  _ -> error ""

gotoExprs1 :: ([Lexeme], Pos) -> Exprs1 -> Stack a -> Either (Pos, [String]) Program
gotoExprs1 toks term stk@(state, _, _) = case state of
  S58 -> run S73 toks (term :> stk)
  S150 -> run S160 toks (term :> stk)
  S151 -> run S161 toks (term :> stk)
  S167 -> run S170 toks (term :> stk)
  _ -> error ""

gotoProgram :: ([Lexeme], Pos) -> Program -> Stack a -> Either (Pos, [String]) Program
gotoProgram toks term stk@(state, _, _) = case state of
  S0 -> run S1 toks (term :> stk)
  _ -> error ""

gotoStmt :: ([Lexeme], Pos) -> Stmt -> Stack a -> Either (Pos, [String]) Program
gotoStmt toks term stk@(state, _, _) = case state of
  S0 -> run S3 toks (term :> stk)
  S3 -> run S3 toks (term :> stk)
  _ -> error ""

gotoStmts :: ([Lexeme], Pos) -> Stmts -> Stack a -> Either (Pos, [String]) Program
gotoStmts toks term stk@(state, _, _) = case state of
  S0 -> run S2 toks (term :> stk)
  S3 -> run S7 toks (term :> stk)
  _ -> error ""

gotoTuple :: ([Lexeme], Pos) -> Tuple -> Stack a -> Either (Pos, [String]) Program
gotoTuple toks term stk@(state, _, _) = case state of
  S106 -> run S156 toks (term :> stk)
  S107 -> run S157 toks (term :> stk)
  S148 -> run S156 toks (term :> stk)
  S149 -> run S157 toks (term :> stk)
  S166 -> run S168 toks (term :> stk)
  _ -> error ""
  
run :: St a -> ([Lexeme], Pos) -> Stack' a -> Either (Pos, [String]) Program
run = \cases {
; S0 ((p, LowercaseName n) : input, end) stk ->
    run S166 (input, end) (n :> (S0, p, stk))
; S3 ((p, LowercaseName n) : input, end) stk ->
    run S166 (input, end) (n :> (S3, p, stk))
; S6 ((p, Reserved "->") : input, end) stk ->
    run S9 (input, end) (() :> (S6, p, stk))
; S6 ((p, Reserved ".") : input, end) stk ->
    run S10 (input, end) (() :> (S6, p, stk))
; S6 ((p, Reserved "<-") : input, end) stk ->
    run S11 (input, end) (() :> (S6, p, stk))
; S6 ((p, Reserved "=>") : input, end) stk ->
    run S8 (input, end) (() :> (S6, p, stk))
; S8 ((p, Reserved "+") : input, end) stk ->
    run S140 (input, end) (() :> (S8, p, stk))
; S8 ((p, Reserved "-") : input, end) stk ->
    run S141 (input, end) (() :> (S8, p, stk))
; S9 ((p, Reserved "(") : input, end) stk ->
    run S96 (input, end) (() :> (S9, p, stk))
; S9 ((p, UppercaseName n) : input, end) stk ->
    run S100 (input, end) (n :> (S9, p, stk))
; S9 ((p, LowercaseName n) : input, end) stk ->
    run S107 (input, end) (n :> (S9, p, stk))
; S9 ((p, NumberLiteral n) : input, end) stk ->
    run S114 (input, end) (n :> (S9, p, stk))
; S9 ((p, Reserved "~") : input, end) stk ->
    run S145 (input, end) (() :> (S9, p, stk))
; S11 ((p, Reserved "(") : input, end) stk ->
    run S95 (input, end) (() :> (S11, p, stk))
; S11 ((p, UppercaseName n) : input, end) stk ->
    run S99 (input, end) (n :> (S11, p, stk))
; S11 ((p, LowercaseName n) : input, end) stk ->
    run S106 (input, end) (n :> (S11, p, stk))
; S11 ((p, NumberLiteral n) : input, end) stk ->
    run S113 (input, end) (n :> (S11, p, stk))
; S11 ((p, Reserved "~") : input, end) stk ->
    run S144 (input, end) (() :> (S11, p, stk))
; S12 ((p, Reserved ".") : input, end) stk ->
    run S15 (input, end) (() :> (S12, p, stk))
; S13 ((p, Reserved ".") : input, end) stk ->
    run S17 (input, end) (() :> (S13, p, stk))
; S13 ((p, Reserved "=>") : input, end) stk ->
    run S16 (input, end) (() :> (S13, p, stk))
; S14 ((p, Reserved ".") : input, end) stk ->
    run S18 (input, end) (() :> (S14, p, stk))
; S16 ((p, Reserved "+") : input, end) stk ->
    run S140 (input, end) (() :> (S16, p, stk))
; S16 ((p, Reserved "-") : input, end) stk ->
    run S141 (input, end) (() :> (S16, p, stk))
; S19 ((p, Reserved ".") : input, end) stk ->
    run S20 (input, end) (() :> (S19, p, stk))
; S21 ((p, Reserved ",") : input, end) stk ->
    run S58 (input, end) (() :> (S21, p, stk))
; S22 ((p, Reserved "+") : input, end) stk ->
    run S63 (input, end) (() :> (S22, p, stk))
; S22 ((p, Reserved "=") : input, end) stk ->
    run S60 (input, end) (() :> (S22, p, stk))
; S23 ((p, Reserved "+") : input, end) stk ->
    run S64 (input, end) (() :> (S23, p, stk))
; S23 ((p, Reserved "=") : input, end) stk ->
    run S59 (input, end) (() :> (S23, p, stk))
; S24 ((p, Reserved "+") : input, end) stk ->
    run S61 (input, end) (() :> (S24, p, stk))
; S25 ((p, Reserved "+") : input, end) stk ->
    run S62 (input, end) (() :> (S25, p, stk))
; S26 ((p, Reserved "*") : input, end) stk ->
    run S65 (input, end) (() :> (S26, p, stk))
; S27 ((p, Reserved "*") : input, end) stk ->
    run S66 (input, end) (() :> (S27, p, stk))
; S28 ((p, Reserved "*") : input, end) stk ->
    run S67 (input, end) (() :> (S28, p, stk))
; S29 ((p, Reserved "*") : input, end) stk ->
    run S68 (input, end) (() :> (S29, p, stk))
; S30 ((p, Reserved "*") : input, end) stk ->
    run S65 (input, end) (() :> (S30, p, stk))
; S31 ((p, Reserved "*") : input, end) stk ->
    run S66 (input, end) (() :> (S31, p, stk))
; S32 ((p, Reserved "*") : input, end) stk ->
    run S67 (input, end) (() :> (S32, p, stk))
; S33 ((p, Reserved "*") : input, end) stk ->
    run S68 (input, end) (() :> (S33, p, stk))
; S38 ((p, Reserved "(") : input, end) stk ->
    run S41 (input, end) (() :> (S38, p, stk))
; S38 ((p, UppercaseName n) : input, end) stk ->
    run S45 (input, end) (n :> (S38, p, stk))
; S38 ((p, LowercaseName n) : input, end) stk ->
    run S53 (input, end) (n :> (S38, p, stk))
; S38 ((p, NumberLiteral n) : input, end) stk ->
    run S57 (input, end) (n :> (S38, p, stk))
; S39 ((p, Reserved "(") : input, end) stk ->
    run S41 (input, end) (() :> (S39, p, stk))
; S39 ((p, UppercaseName n) : input, end) stk ->
    run S45 (input, end) (n :> (S39, p, stk))
; S39 ((p, LowercaseName n) : input, end) stk ->
    run S53 (input, end) (n :> (S39, p, stk))
; S39 ((p, NumberLiteral n) : input, end) stk ->
    run S57 (input, end) (n :> (S39, p, stk))
; S40 ((p, Reserved "(") : input, end) stk ->
    run S41 (input, end) (() :> (S40, p, stk))
; S40 ((p, UppercaseName n) : input, end) stk ->
    run S45 (input, end) (n :> (S40, p, stk))
; S40 ((p, LowercaseName n) : input, end) stk ->
    run S53 (input, end) (n :> (S40, p, stk))
; S40 ((p, NumberLiteral n) : input, end) stk ->
    run S57 (input, end) (n :> (S40, p, stk))
; S41 ((p, Reserved "(") : input, end) stk ->
    run S41 (input, end) (() :> (S41, p, stk))
; S41 ((p, UppercaseName n) : input, end) stk ->
    run S45 (input, end) (n :> (S41, p, stk))
; S41 ((p, LowercaseName n) : input, end) stk ->
    run S53 (input, end) (n :> (S41, p, stk))
; S41 ((p, NumberLiteral n) : input, end) stk ->
    run S57 (input, end) (n :> (S41, p, stk))
; S58 ((p, Reserved "(") : input, end) stk ->
    run S40 (input, end) (() :> (S58, p, stk))
; S58 ((p, UppercaseName n) : input, end) stk ->
    run S44 (input, end) (n :> (S58, p, stk))
; S58 ((p, LowercaseName n) : input, end) stk ->
    run S52 (input, end) (n :> (S58, p, stk))
; S58 ((p, NumberLiteral n) : input, end) stk ->
    run S56 (input, end) (n :> (S58, p, stk))
; S59 ((p, Reserved "(") : input, end) stk ->
    run S38 (input, end) (() :> (S59, p, stk))
; S59 ((p, UppercaseName n) : input, end) stk ->
    run S42 (input, end) (n :> (S59, p, stk))
; S59 ((p, LowercaseName n) : input, end) stk ->
    run S50 (input, end) (n :> (S59, p, stk))
; S59 ((p, NumberLiteral n) : input, end) stk ->
    run S54 (input, end) (n :> (S59, p, stk))
; S60 ((p, Reserved "(") : input, end) stk ->
    run S39 (input, end) (() :> (S60, p, stk))
; S60 ((p, UppercaseName n) : input, end) stk ->
    run S43 (input, end) (n :> (S60, p, stk))
; S60 ((p, LowercaseName n) : input, end) stk ->
    run S51 (input, end) (n :> (S60, p, stk))
; S60 ((p, NumberLiteral n) : input, end) stk ->
    run S55 (input, end) (n :> (S60, p, stk))
; S61 ((p, Reserved "(") : input, end) stk ->
    run S38 (input, end) (() :> (S61, p, stk))
; S61 ((p, UppercaseName n) : input, end) stk ->
    run S42 (input, end) (n :> (S61, p, stk))
; S61 ((p, LowercaseName n) : input, end) stk ->
    run S50 (input, end) (n :> (S61, p, stk))
; S61 ((p, NumberLiteral n) : input, end) stk ->
    run S54 (input, end) (n :> (S61, p, stk))
; S62 ((p, Reserved "(") : input, end) stk ->
    run S39 (input, end) (() :> (S62, p, stk))
; S62 ((p, UppercaseName n) : input, end) stk ->
    run S43 (input, end) (n :> (S62, p, stk))
; S62 ((p, LowercaseName n) : input, end) stk ->
    run S51 (input, end) (n :> (S62, p, stk))
; S62 ((p, NumberLiteral n) : input, end) stk ->
    run S55 (input, end) (n :> (S62, p, stk))
; S63 ((p, Reserved "(") : input, end) stk ->
    run S40 (input, end) (() :> (S63, p, stk))
; S63 ((p, UppercaseName n) : input, end) stk ->
    run S44 (input, end) (n :> (S63, p, stk))
; S63 ((p, LowercaseName n) : input, end) stk ->
    run S52 (input, end) (n :> (S63, p, stk))
; S63 ((p, NumberLiteral n) : input, end) stk ->
    run S56 (input, end) (n :> (S63, p, stk))
; S64 ((p, Reserved "(") : input, end) stk ->
    run S41 (input, end) (() :> (S64, p, stk))
; S64 ((p, UppercaseName n) : input, end) stk ->
    run S45 (input, end) (n :> (S64, p, stk))
; S64 ((p, LowercaseName n) : input, end) stk ->
    run S53 (input, end) (n :> (S64, p, stk))
; S64 ((p, NumberLiteral n) : input, end) stk ->
    run S57 (input, end) (n :> (S64, p, stk))
; S65 ((p, Reserved "(") : input, end) stk ->
    run S38 (input, end) (() :> (S65, p, stk))
; S65 ((p, UppercaseName n) : input, end) stk ->
    run S42 (input, end) (n :> (S65, p, stk))
; S65 ((p, LowercaseName n) : input, end) stk ->
    run S50 (input, end) (n :> (S65, p, stk))
; S65 ((p, NumberLiteral n) : input, end) stk ->
    run S54 (input, end) (n :> (S65, p, stk))
; S66 ((p, Reserved "(") : input, end) stk ->
    run S39 (input, end) (() :> (S66, p, stk))
; S66 ((p, UppercaseName n) : input, end) stk ->
    run S43 (input, end) (n :> (S66, p, stk))
; S66 ((p, LowercaseName n) : input, end) stk ->
    run S51 (input, end) (n :> (S66, p, stk))
; S66 ((p, NumberLiteral n) : input, end) stk ->
    run S55 (input, end) (n :> (S66, p, stk))
; S67 ((p, Reserved "(") : input, end) stk ->
    run S40 (input, end) (() :> (S67, p, stk))
; S67 ((p, UppercaseName n) : input, end) stk ->
    run S44 (input, end) (n :> (S67, p, stk))
; S67 ((p, LowercaseName n) : input, end) stk ->
    run S52 (input, end) (n :> (S67, p, stk))
; S67 ((p, NumberLiteral n) : input, end) stk ->
    run S56 (input, end) (n :> (S67, p, stk))
; S68 ((p, Reserved "(") : input, end) stk ->
    run S41 (input, end) (() :> (S68, p, stk))
; S68 ((p, UppercaseName n) : input, end) stk ->
    run S45 (input, end) (n :> (S68, p, stk))
; S68 ((p, LowercaseName n) : input, end) stk ->
    run S53 (input, end) (n :> (S68, p, stk))
; S68 ((p, NumberLiteral n) : input, end) stk ->
    run S57 (input, end) (n :> (S68, p, stk))
; S69 ((p, Reserved ")") : input, end) stk ->
    run S78 (input, end) (() :> (S69, p, stk))
; S70 ((p, Reserved ")") : input, end) stk ->
    run S79 (input, end) (() :> (S70, p, stk))
; S71 ((p, Reserved ")") : input, end) stk ->
    run S80 (input, end) (() :> (S71, p, stk))
; S72 ((p, Reserved ")") : input, end) stk ->
    run S81 (input, end) (() :> (S72, p, stk))
; S82 ((p, Reserved "*") : input, end) stk ->
    run S116 (input, end) (() :> (S82, p, stk))
; S83 ((p, Reserved "*") : input, end) stk ->
    run S117 (input, end) (() :> (S83, p, stk))
; S84 ((p, Reserved "*") : input, end) stk ->
    run S118 (input, end) (() :> (S84, p, stk))
; S85 ((p, Reserved "*") : input, end) stk ->
    run S119 (input, end) (() :> (S85, p, stk))
; S86 ((p, Reserved "*") : input, end) stk ->
    run S116 (input, end) (() :> (S86, p, stk))
; S87 ((p, Reserved "*") : input, end) stk ->
    run S117 (input, end) (() :> (S87, p, stk))
; S88 ((p, Reserved "*") : input, end) stk ->
    run S118 (input, end) (() :> (S88, p, stk))
; S89 ((p, Reserved "*") : input, end) stk ->
    run S119 (input, end) (() :> (S89, p, stk))
; S94 ((p, Reserved "(") : input, end) stk ->
    run S41 (input, end) (() :> (S94, p, stk))
; S94 ((p, UppercaseName n) : input, end) stk ->
    run S45 (input, end) (n :> (S94, p, stk))
; S94 ((p, LowercaseName n) : input, end) stk ->
    run S53 (input, end) (n :> (S94, p, stk))
; S94 ((p, NumberLiteral n) : input, end) stk ->
    run S57 (input, end) (n :> (S94, p, stk))
; S95 ((p, Reserved "(") : input, end) stk ->
    run S41 (input, end) (() :> (S95, p, stk))
; S95 ((p, UppercaseName n) : input, end) stk ->
    run S45 (input, end) (n :> (S95, p, stk))
; S95 ((p, LowercaseName n) : input, end) stk ->
    run S53 (input, end) (n :> (S95, p, stk))
; S95 ((p, NumberLiteral n) : input, end) stk ->
    run S57 (input, end) (n :> (S95, p, stk))
; S96 ((p, Reserved "(") : input, end) stk ->
    run S41 (input, end) (() :> (S96, p, stk))
; S96 ((p, UppercaseName n) : input, end) stk ->
    run S45 (input, end) (n :> (S96, p, stk))
; S96 ((p, LowercaseName n) : input, end) stk ->
    run S53 (input, end) (n :> (S96, p, stk))
; S96 ((p, NumberLiteral n) : input, end) stk ->
    run S57 (input, end) (n :> (S96, p, stk))
; S97 ((p, Reserved "(") : input, end) stk ->
    run S41 (input, end) (() :> (S97, p, stk))
; S97 ((p, UppercaseName n) : input, end) stk ->
    run S45 (input, end) (n :> (S97, p, stk))
; S97 ((p, LowercaseName n) : input, end) stk ->
    run S53 (input, end) (n :> (S97, p, stk))
; S97 ((p, NumberLiteral n) : input, end) stk ->
    run S57 (input, end) (n :> (S97, p, stk))
; S106 ((p, Reserved "(") : input, end) stk ->
    run S150 (input, end) (() :> (S106, p, stk))
; S107 ((p, Reserved "(") : input, end) stk ->
    run S151 (input, end) (() :> (S107, p, stk))
; S116 ((p, Reserved "(") : input, end) stk ->
    run S94 (input, end) (() :> (S116, p, stk))
; S116 ((p, UppercaseName n) : input, end) stk ->
    run S98 (input, end) (n :> (S116, p, stk))
; S116 ((p, LowercaseName n) : input, end) stk ->
    run S108 (input, end) (n :> (S116, p, stk))
; S116 ((p, NumberLiteral n) : input, end) stk ->
    run S112 (input, end) (n :> (S116, p, stk))
; S117 ((p, Reserved "(") : input, end) stk ->
    run S95 (input, end) (() :> (S117, p, stk))
; S117 ((p, UppercaseName n) : input, end) stk ->
    run S99 (input, end) (n :> (S117, p, stk))
; S117 ((p, LowercaseName n) : input, end) stk ->
    run S109 (input, end) (n :> (S117, p, stk))
; S117 ((p, NumberLiteral n) : input, end) stk ->
    run S113 (input, end) (n :> (S117, p, stk))
; S118 ((p, Reserved "(") : input, end) stk ->
    run S96 (input, end) (() :> (S118, p, stk))
; S118 ((p, UppercaseName n) : input, end) stk ->
    run S100 (input, end) (n :> (S118, p, stk))
; S118 ((p, LowercaseName n) : input, end) stk ->
    run S110 (input, end) (n :> (S118, p, stk))
; S118 ((p, NumberLiteral n) : input, end) stk ->
    run S114 (input, end) (n :> (S118, p, stk))
; S119 ((p, Reserved "(") : input, end) stk ->
    run S97 (input, end) (() :> (S119, p, stk))
; S119 ((p, UppercaseName n) : input, end) stk ->
    run S101 (input, end) (n :> (S119, p, stk))
; S119 ((p, LowercaseName n) : input, end) stk ->
    run S111 (input, end) (n :> (S119, p, stk))
; S119 ((p, NumberLiteral n) : input, end) stk ->
    run S115 (input, end) (n :> (S119, p, stk))
; S120 ((p, Reserved ")") : input, end) stk ->
    run S128 (input, end) (() :> (S120, p, stk))
; S121 ((p, Reserved ")") : input, end) stk ->
    run S129 (input, end) (() :> (S121, p, stk))
; S122 ((p, Reserved ")") : input, end) stk ->
    run S130 (input, end) (() :> (S122, p, stk))
; S123 ((p, Reserved ")") : input, end) stk ->
    run S131 (input, end) (() :> (S123, p, stk))
; S132 ((p, Reserved "+") : input, end) stk ->
    run S137 (input, end) (() :> (S132, p, stk))
; S132 ((p, Reserved "=") : input, end) stk ->
    run S162 (input, end) (() :> (S132, p, stk))
; S133 ((p, Reserved "+") : input, end) stk ->
    run S138 (input, end) (() :> (S133, p, stk))
; S133 ((p, Reserved "=") : input, end) stk ->
    run S163 (input, end) (() :> (S133, p, stk))
; S134 ((p, Reserved "+") : input, end) stk ->
    run S136 (input, end) (() :> (S134, p, stk))
; S135 ((p, Reserved "+") : input, end) stk ->
    run S139 (input, end) (() :> (S135, p, stk))
; S136 ((p, Reserved "(") : input, end) stk ->
    run S94 (input, end) (() :> (S136, p, stk))
; S136 ((p, UppercaseName n) : input, end) stk ->
    run S98 (input, end) (n :> (S136, p, stk))
; S136 ((p, LowercaseName n) : input, end) stk ->
    run S108 (input, end) (n :> (S136, p, stk))
; S136 ((p, NumberLiteral n) : input, end) stk ->
    run S112 (input, end) (n :> (S136, p, stk))
; S137 ((p, Reserved "(") : input, end) stk ->
    run S95 (input, end) (() :> (S137, p, stk))
; S137 ((p, UppercaseName n) : input, end) stk ->
    run S99 (input, end) (n :> (S137, p, stk))
; S137 ((p, LowercaseName n) : input, end) stk ->
    run S109 (input, end) (n :> (S137, p, stk))
; S137 ((p, NumberLiteral n) : input, end) stk ->
    run S113 (input, end) (n :> (S137, p, stk))
; S138 ((p, Reserved "(") : input, end) stk ->
    run S96 (input, end) (() :> (S138, p, stk))
; S138 ((p, UppercaseName n) : input, end) stk ->
    run S100 (input, end) (n :> (S138, p, stk))
; S138 ((p, LowercaseName n) : input, end) stk ->
    run S110 (input, end) (n :> (S138, p, stk))
; S138 ((p, NumberLiteral n) : input, end) stk ->
    run S114 (input, end) (n :> (S138, p, stk))
; S139 ((p, Reserved "(") : input, end) stk ->
    run S97 (input, end) (() :> (S139, p, stk))
; S139 ((p, UppercaseName n) : input, end) stk ->
    run S101 (input, end) (n :> (S139, p, stk))
; S139 ((p, LowercaseName n) : input, end) stk ->
    run S111 (input, end) (n :> (S139, p, stk))
; S139 ((p, NumberLiteral n) : input, end) stk ->
    run S115 (input, end) (n :> (S139, p, stk))
; S140 ((p, LowercaseName n) : input, end) stk ->
    run S148 (input, end) (n :> (S140, p, stk))
; S141 ((p, LowercaseName n) : input, end) stk ->
    run S148 (input, end) (n :> (S141, p, stk))
; S144 ((p, LowercaseName n) : input, end) stk ->
    run S148 (input, end) (n :> (S144, p, stk))
; S145 ((p, LowercaseName n) : input, end) stk ->
    run S149 (input, end) (n :> (S145, p, stk))
; S148 ((p, Reserved "(") : input, end) stk ->
    run S150 (input, end) (() :> (S148, p, stk))
; S149 ((p, Reserved "(") : input, end) stk ->
    run S151 (input, end) (() :> (S149, p, stk))
; S150 ((p, Reserved "(") : input, end) stk ->
    run S40 (input, end) (() :> (S150, p, stk))
; S150 ((p, Reserved ")") : input, end) stk ->
    run S158 (input, end) (() :> (S150, p, stk))
; S150 ((p, UppercaseName n) : input, end) stk ->
    run S44 (input, end) (n :> (S150, p, stk))
; S150 ((p, LowercaseName n) : input, end) stk ->
    run S52 (input, end) (n :> (S150, p, stk))
; S150 ((p, NumberLiteral n) : input, end) stk ->
    run S56 (input, end) (n :> (S150, p, stk))
; S151 ((p, Reserved "(") : input, end) stk ->
    run S40 (input, end) (() :> (S151, p, stk))
; S151 ((p, Reserved ")") : input, end) stk ->
    run S159 (input, end) (() :> (S151, p, stk))
; S151 ((p, UppercaseName n) : input, end) stk ->
    run S44 (input, end) (n :> (S151, p, stk))
; S151 ((p, LowercaseName n) : input, end) stk ->
    run S52 (input, end) (n :> (S151, p, stk))
; S151 ((p, NumberLiteral n) : input, end) stk ->
    run S56 (input, end) (n :> (S151, p, stk))
; S160 ((p, Reserved ")") : input, end) stk ->
    run S164 (input, end) (() :> (S160, p, stk))
; S161 ((p, Reserved ")") : input, end) stk ->
    run S165 (input, end) (() :> (S161, p, stk))
; S162 ((p, Reserved "(") : input, end) stk ->
    run S94 (input, end) (() :> (S162, p, stk))
; S162 ((p, UppercaseName n) : input, end) stk ->
    run S98 (input, end) (n :> (S162, p, stk))
; S162 ((p, LowercaseName n) : input, end) stk ->
    run S108 (input, end) (n :> (S162, p, stk))
; S162 ((p, NumberLiteral n) : input, end) stk ->
    run S112 (input, end) (n :> (S162, p, stk))
; S163 ((p, Reserved "(") : input, end) stk ->
    run S97 (input, end) (() :> (S163, p, stk))
; S163 ((p, UppercaseName n) : input, end) stk ->
    run S101 (input, end) (n :> (S163, p, stk))
; S163 ((p, LowercaseName n) : input, end) stk ->
    run S111 (input, end) (n :> (S163, p, stk))
; S163 ((p, NumberLiteral n) : input, end) stk ->
    run S115 (input, end) (n :> (S163, p, stk))
; S166 ((p, Reserved "(") : input, end) stk ->
    run S167 (input, end) (() :> (S166, p, stk))
; S167 ((p, Reserved "(") : input, end) stk ->
    run S40 (input, end) (() :> (S167, p, stk))
; S167 ((p, Reserved ")") : input, end) stk ->
    run S169 (input, end) (() :> (S167, p, stk))
; S167 ((p, UppercaseName n) : input, end) stk ->
    run S44 (input, end) (n :> (S167, p, stk))
; S167 ((p, LowercaseName n) : input, end) stk ->
    run S52 (input, end) (n :> (S167, p, stk))
; S167 ((p, NumberLiteral n) : input, end) stk ->
    run S56 (input, end) (n :> (S167, p, stk))
; S170 ((p, Reserved ")") : input, end) stk ->
    run S171 (input, end) (() :> (S170, p, stk))
; S172 ((p, Reserved ",") : input, end) stk ->
    run S175 (input, end) (() :> (S172, p, stk))
; S173 ((p, Reserved ",") : input, end) stk ->
    run S176 (input, end) (() :> (S173, p, stk))
; S174 ((p, Reserved ",") : input, end) stk ->
    run S177 (input, end) (() :> (S174, p, stk))
; S175 ((p, Reserved "+") : input, end) stk ->
    run S140 (input, end) (() :> (S175, p, stk))
; S175 ((p, Reserved "-") : input, end) stk ->
    run S141 (input, end) (() :> (S175, p, stk))
; S176 ((p, Reserved "(") : input, end) stk ->
    run S95 (input, end) (() :> (S176, p, stk))
; S176 ((p, UppercaseName n) : input, end) stk ->
    run S99 (input, end) (n :> (S176, p, stk))
; S176 ((p, LowercaseName n) : input, end) stk ->
    run S106 (input, end) (n :> (S176, p, stk))
; S176 ((p, NumberLiteral n) : input, end) stk ->
    run S113 (input, end) (n :> (S176, p, stk))
; S176 ((p, Reserved "~") : input, end) stk ->
    run S144 (input, end) (() :> (S176, p, stk))
; S177 ((p, Reserved "(") : input, end) stk ->
    run S96 (input, end) (() :> (S177, p, stk))
; S177 ((p, UppercaseName n) : input, end) stk ->
    run S100 (input, end) (n :> (S177, p, stk))
; S177 ((p, LowercaseName n) : input, end) stk ->
    run S107 (input, end) (n :> (S177, p, stk))
; S177 ((p, NumberLiteral n) : input, end) stk ->
    run S114 (input, end) (n :> (S177, p, stk))
; S177 ((p, Reserved "~") : input, end) stk ->
    run S145 (input, end) (() :> (S177, p, stk))
; S1 ([], end) (res :> stk@(_, pos, _)) -> pure res
; S2 ([], end) (stmts :> stk@(_, pos, _)) ->
    gotoProgram ([], end) (action10 pos stmts) stk
; S3 ([], end) (c :> stk@(_, pos, _)) ->
    gotoStmts ([], end) (action13 pos c) stk
; S4 ([], end) (c :> stk@(_, pos, _)) ->
    gotoStmt ([], end) (action15 pos c) stk
; S4 ((p, LowercaseName tok) : input, end) (c :> stk@(_, pos, _)) ->
    gotoStmt ((p, LowercaseName tok) : input, end) (action15 pos c) stk
; S5 ([], end) (e :> stk@(_, pos, _)) ->
    gotoStmt ([], end) (action16 pos e) stk
; S5 ((p, LowercaseName tok) : input, end) (e :> stk@(_, pos, _)) ->
    gotoStmt ((p, LowercaseName tok) : input, end) (action16 pos e) stk
; S7 ([], end) (cs :> c :? stk@(_, pos, _)) ->
    gotoStmts ([], end) (action12 pos c cs) stk
; S10 ([], end) (_ :> c :? stk@(_, pos, _)) ->
    gotoEffect ([], end) (action24 pos c) stk
; S10 ([], end) (_ :> c :? stk@(_, pos, _)) ->
    gotoClause ([], end) (action32 pos c) stk
; S10 ((p, LowercaseName tok) : input, end) (_ :> c :? stk@(_, pos, _)) ->
    gotoEffect ((p, LowercaseName tok) : input, end) (action24 pos c) stk
; S10 ((p, LowercaseName tok) : input, end) (_ :> c :? stk@(_, pos, _)) ->
    gotoClause ((p, LowercaseName tok) : input, end) (action32 pos c) stk
; S15 ([], end) (_ :> ds :? _ :? c :? stk@(_, pos, _)) ->
    gotoEffect ([], end) (action18 pos c ds) stk
; S15 ([], end) (_ :> ds :? _ :? c :? stk@(_, pos, _)) ->
    gotoEffect ([], end) (action21 pos c ds) stk
; S15 ((p, LowercaseName tok) : input, end) (_ :> ds :? _ :? c :? stk@(_, pos, _)) ->
    gotoEffect ((p, LowercaseName tok) : input, end) (action18 pos c
                                                                   ds) stk
; S15 ((p, LowercaseName tok) : input, end) (_ :> ds :? _ :? c :? stk@(_, pos, _)) ->
    gotoEffect ((p, LowercaseName tok) : input, end) (action21 pos c
                                                                   ds) stk
; S17 ([], end) (_ :> cs :? _ :? c :? stk@(_, pos, _)) ->
    gotoEffect ([], end) (action23 pos c cs) stk
; S17 ((p, LowercaseName tok) : input, end) (_ :> cs :? _ :? c :? stk@(_, pos, _)) ->
    gotoEffect ((p, LowercaseName tok) : input, end) (action23 pos c
                                                                   cs) stk
; S18 ([], end) (_ :> cs :? _ :? c :? stk@(_, pos, _)) ->
    gotoClause ([], end) (action33 pos c cs) stk
; S18 ((p, LowercaseName tok) : input, end) (_ :> cs :? _ :? c :? stk@(_, pos, _)) ->
    gotoClause ((p, LowercaseName tok) : input, end) (action33 pos c
                                                                   cs) stk
; S20 ([], end) (_ :> ds :? _ :? cs :? _ :? c :? stk@(_, pos, _)) ->
    gotoEffect ([], end) (action19 pos c cs ds) stk
; S20 ([], end) (_ :> ds :? _ :? cs :? _ :? c :? stk@(_, pos, _)) ->
    gotoEffect ([], end) (action22 pos c cs ds) stk
; S20 ((p, LowercaseName tok) : input, end) (_ :> ds :? _ :? cs :? _ :? c :? stk@(_, pos, _)) ->
    gotoEffect ((p, LowercaseName tok) : input, end) (action19 pos c cs
                                                                   ds) stk
; S20 ((p, LowercaseName tok) : input, end) (_ :> ds :? _ :? cs :? _ :? c :? stk@(_, pos, _)) ->
    gotoEffect ((p, LowercaseName tok) : input, end) (action22 pos c cs
                                                                   ds) stk
; S21 ((p, Reserved ")") : input, end) (e :> stk@(_, pos, _)) ->
    gotoExprs1 ((p, Reserved ")") : input, end) (action48 pos e) stk
; S22 ((p, Reserved ")") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved ")") : input, end) (action51 pos a) stk
; S22 ((p, Reserved ",") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved ",") : input, end) (action51 pos a) stk
; S23 ((p, Reserved ")") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved ")") : input, end) (action51 pos a) stk
; S24 ((p, Reserved ")") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved ")") : input, end) (action50 pos a b) stk
; S25 ((p, Reserved ")") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved ")") : input, end) (action50 pos a b) stk
; S25 ((p, Reserved ",") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved ",") : input, end) (action50 pos a b) stk
; S26 ((p, Reserved ")") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ")") : input, end) (action54 pos a) stk
; S26 ((p, Reserved "+") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "+") : input, end) (action54 pos a) stk
; S27 ((p, Reserved ")") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ")") : input, end) (action54 pos a) stk
; S27 ((p, Reserved "+") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "+") : input, end) (action54 pos a) stk
; S27 ((p, Reserved ",") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ",") : input, end) (action54 pos a) stk
; S28 ((p, Reserved ")") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ")") : input, end) (action54 pos a) stk
; S28 ((p, Reserved "+") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "+") : input, end) (action54 pos a) stk
; S28 ((p, Reserved ",") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ",") : input, end) (action54 pos a) stk
; S28 ((p, Reserved "=") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "=") : input, end) (action54 pos a) stk
; S29 ((p, Reserved ")") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ")") : input, end) (action54 pos a) stk
; S29 ((p, Reserved "+") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "+") : input, end) (action54 pos a) stk
; S29 ((p, Reserved "=") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "=") : input, end) (action54 pos a) stk
; S30 ((p, Reserved ")") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ")") : input, end) (action53 pos a b) stk
; S30 ((p, Reserved "+") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "+") : input, end) (action53 pos a b) stk
; S31 ((p, Reserved ")") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ")") : input, end) (action53 pos a b) stk
; S31 ((p, Reserved "+") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "+") : input, end) (action53 pos a b) stk
; S31 ((p, Reserved ",") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ",") : input, end) (action53 pos a b) stk
; S32 ((p, Reserved ")") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ")") : input, end) (action53 pos a b) stk
; S32 ((p, Reserved "+") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "+") : input, end) (action53 pos a b) stk
; S32 ((p, Reserved ",") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ",") : input, end) (action53 pos a b) stk
; S32 ((p, Reserved "=") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "=") : input, end) (action53 pos a b) stk
; S33 ((p, Reserved ")") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ")") : input, end) (action53 pos a b) stk
; S33 ((p, Reserved "+") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "+") : input, end) (action53 pos a b) stk
; S33 ((p, Reserved "=") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "=") : input, end) (action53 pos a b) stk
; S34 ((p, Reserved ")") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ")") : input, end) (action57 pos a) stk
; S34 ((p, Reserved "*") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "*") : input, end) (action57 pos a) stk
; S34 ((p, Reserved "+") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "+") : input, end) (action57 pos a) stk
; S35 ((p, Reserved ")") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ")") : input, end) (action57 pos a) stk
; S35 ((p, Reserved "*") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "*") : input, end) (action57 pos a) stk
; S35 ((p, Reserved "+") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "+") : input, end) (action57 pos a) stk
; S35 ((p, Reserved ",") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ",") : input, end) (action57 pos a) stk
; S36 ((p, Reserved ")") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ")") : input, end) (action57 pos a) stk
; S36 ((p, Reserved "*") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "*") : input, end) (action57 pos a) stk
; S36 ((p, Reserved "+") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "+") : input, end) (action57 pos a) stk
; S36 ((p, Reserved ",") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ",") : input, end) (action57 pos a) stk
; S36 ((p, Reserved "=") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "=") : input, end) (action57 pos a) stk
; S37 ((p, Reserved ")") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ")") : input, end) (action57 pos a) stk
; S37 ((p, Reserved "*") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "*") : input, end) (action57 pos a) stk
; S37 ((p, Reserved "+") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "+") : input, end) (action57 pos a) stk
; S37 ((p, Reserved "=") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "=") : input, end) (action57 pos a) stk
; S42 ((p, Reserved ")") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ")") : input, end) (action60 pos n) stk
; S42 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action60 pos n) stk
; S42 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action60 pos n) stk
; S43 ((p, Reserved ")") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ")") : input, end) (action60 pos n) stk
; S43 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action60 pos n) stk
; S43 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action60 pos n) stk
; S43 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ",") : input, end) (action60 pos n) stk
; S44 ((p, Reserved ")") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ")") : input, end) (action60 pos n) stk
; S44 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action60 pos n) stk
; S44 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action60 pos n) stk
; S44 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ",") : input, end) (action60 pos n) stk
; S44 ((p, Reserved "=") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "=") : input, end) (action60 pos n) stk
; S45 ((p, Reserved ")") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ")") : input, end) (action60 pos n) stk
; S45 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action60 pos n) stk
; S45 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action60 pos n) stk
; S45 ((p, Reserved "=") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "=") : input, end) (action60 pos n) stk
; S46 ((p, Reserved ")") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ")") : input, end) (action61 pos n) stk
; S46 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action61 pos n) stk
; S46 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action61 pos n) stk
; S47 ((p, Reserved ")") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ")") : input, end) (action61 pos n) stk
; S47 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action61 pos n) stk
; S47 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action61 pos n) stk
; S47 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ",") : input, end) (action61 pos n) stk
; S48 ((p, Reserved ")") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ")") : input, end) (action61 pos n) stk
; S48 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action61 pos n) stk
; S48 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action61 pos n) stk
; S48 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ",") : input, end) (action61 pos n) stk
; S48 ((p, Reserved "=") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "=") : input, end) (action61 pos n) stk
; S49 ((p, Reserved ")") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ")") : input, end) (action61 pos n) stk
; S49 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action61 pos n) stk
; S49 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action61 pos n) stk
; S49 ((p, Reserved "=") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "=") : input, end) (action61 pos n) stk
; S50 ((p, Reserved ")") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ")") : input, end) (action63 pos n) stk
; S50 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "*") : input, end) (action63 pos n) stk
; S50 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "+") : input, end) (action63 pos n) stk
; S51 ((p, Reserved ")") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ")") : input, end) (action63 pos n) stk
; S51 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "*") : input, end) (action63 pos n) stk
; S51 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "+") : input, end) (action63 pos n) stk
; S51 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ",") : input, end) (action63 pos n) stk
; S52 ((p, Reserved ")") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ")") : input, end) (action63 pos n) stk
; S52 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "*") : input, end) (action63 pos n) stk
; S52 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "+") : input, end) (action63 pos n) stk
; S52 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ",") : input, end) (action63 pos n) stk
; S52 ((p, Reserved "=") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "=") : input, end) (action63 pos n) stk
; S53 ((p, Reserved ")") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ")") : input, end) (action63 pos n) stk
; S53 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "*") : input, end) (action63 pos n) stk
; S53 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "+") : input, end) (action63 pos n) stk
; S53 ((p, Reserved "=") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "=") : input, end) (action63 pos n) stk
; S54 ((p, Reserved ")") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ")") : input, end) (action64 pos n) stk
; S54 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "*") : input, end) (action64 pos n) stk
; S54 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "+") : input, end) (action64 pos n) stk
; S55 ((p, Reserved ")") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ")") : input, end) (action64 pos n) stk
; S55 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "*") : input, end) (action64 pos n) stk
; S55 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "+") : input, end) (action64 pos n) stk
; S55 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ",") : input, end) (action64 pos n) stk
; S56 ((p, Reserved ")") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ")") : input, end) (action64 pos n) stk
; S56 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "*") : input, end) (action64 pos n) stk
; S56 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "+") : input, end) (action64 pos n) stk
; S56 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ",") : input, end) (action64 pos n) stk
; S56 ((p, Reserved "=") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "=") : input, end) (action64 pos n) stk
; S57 ((p, Reserved ")") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ")") : input, end) (action64 pos n) stk
; S57 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "*") : input, end) (action64 pos n) stk
; S57 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "+") : input, end) (action64 pos n) stk
; S57 ((p, Reserved "=") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "=") : input, end) (action64 pos n) stk
; S73 ((p, Reserved ")") : input, end) (es :> _ :? e :? stk@(_, pos, _)) ->
    gotoExprs1 ((p, Reserved ")") : input, end) (action47 pos e es) stk
; S74 ((p, Reserved ")") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ")") : input, end) (action56 pos a
                                                                b) stk
; S74 ((p, Reserved "*") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "*") : input, end) (action56 pos a
                                                                b) stk
; S74 ((p, Reserved "+") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "+") : input, end) (action56 pos a
                                                                b) stk
; S75 ((p, Reserved ")") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ")") : input, end) (action56 pos a
                                                                b) stk
; S75 ((p, Reserved "*") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "*") : input, end) (action56 pos a
                                                                b) stk
; S75 ((p, Reserved "+") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "+") : input, end) (action56 pos a
                                                                b) stk
; S75 ((p, Reserved ",") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ",") : input, end) (action56 pos a
                                                                b) stk
; S76 ((p, Reserved ")") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ")") : input, end) (action56 pos a
                                                                b) stk
; S76 ((p, Reserved "*") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "*") : input, end) (action56 pos a
                                                                b) stk
; S76 ((p, Reserved "+") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "+") : input, end) (action56 pos a
                                                                b) stk
; S76 ((p, Reserved ",") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ",") : input, end) (action56 pos a
                                                                b) stk
; S76 ((p, Reserved "=") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "=") : input, end) (action56 pos a
                                                                b) stk
; S77 ((p, Reserved ")") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ")") : input, end) (action56 pos a
                                                                b) stk
; S77 ((p, Reserved "*") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "*") : input, end) (action56 pos a
                                                                b) stk
; S77 ((p, Reserved "+") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "+") : input, end) (action56 pos a
                                                                b) stk
; S77 ((p, Reserved "=") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "=") : input, end) (action56 pos a
                                                                b) stk
; S78 ((p, Reserved ")") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ")") : input, end) (action59 pos e) stk
; S78 ((p, Reserved "*") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action59 pos e) stk
; S78 ((p, Reserved "+") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action59 pos e) stk
; S79 ((p, Reserved ")") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ")") : input, end) (action59 pos e) stk
; S79 ((p, Reserved "*") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action59 pos e) stk
; S79 ((p, Reserved "+") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action59 pos e) stk
; S79 ((p, Reserved ",") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ",") : input, end) (action59 pos e) stk
; S80 ((p, Reserved ")") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ")") : input, end) (action59 pos e) stk
; S80 ((p, Reserved "*") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action59 pos e) stk
; S80 ((p, Reserved "+") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action59 pos e) stk
; S80 ((p, Reserved ",") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ",") : input, end) (action59 pos e) stk
; S80 ((p, Reserved "=") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "=") : input, end) (action59 pos e) stk
; S81 ((p, Reserved ")") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ")") : input, end) (action59 pos e) stk
; S81 ((p, Reserved "*") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action59 pos e) stk
; S81 ((p, Reserved "+") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action59 pos e) stk
; S81 ((p, Reserved "=") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "=") : input, end) (action59 pos e) stk
; S82 ((p, Reserved "+") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "+") : input, end) (action54 pos a) stk
; S82 ((p, Reserved ",") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ",") : input, end) (action54 pos a) stk
; S82 ((p, Reserved ".") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ".") : input, end) (action54 pos a) stk
; S83 ((p, Reserved "+") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "+") : input, end) (action54 pos a) stk
; S83 ((p, Reserved ",") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ",") : input, end) (action54 pos a) stk
; S83 ((p, Reserved ".") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ".") : input, end) (action54 pos a) stk
; S83 ((p, Reserved "=") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "=") : input, end) (action54 pos a) stk
; S84 ((p, Reserved "+") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "+") : input, end) (action54 pos a) stk
; S84 ((p, Reserved ",") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ",") : input, end) (action54 pos a) stk
; S84 ((p, Reserved ".") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ".") : input, end) (action54 pos a) stk
; S84 ((p, Reserved "=") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "=") : input, end) (action54 pos a) stk
; S84 ((p, Reserved "=>") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "=>") : input, end) (action54 pos a) stk
; S85 ((p, Reserved "+") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "+") : input, end) (action54 pos a) stk
; S85 ((p, Reserved ",") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ",") : input, end) (action54 pos a) stk
; S85 ((p, Reserved ".") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ".") : input, end) (action54 pos a) stk
; S85 ((p, Reserved "=>") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "=>") : input, end) (action54 pos a) stk
; S86 ((p, Reserved "+") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "+") : input, end) (action53 pos a b) stk
; S86 ((p, Reserved ",") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ",") : input, end) (action53 pos a b) stk
; S86 ((p, Reserved ".") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ".") : input, end) (action53 pos a b) stk
; S87 ((p, Reserved "+") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "+") : input, end) (action53 pos a b) stk
; S87 ((p, Reserved ",") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ",") : input, end) (action53 pos a b) stk
; S87 ((p, Reserved ".") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ".") : input, end) (action53 pos a b) stk
; S87 ((p, Reserved "=") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "=") : input, end) (action53 pos a b) stk
; S88 ((p, Reserved "+") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "+") : input, end) (action53 pos a b) stk
; S88 ((p, Reserved ",") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ",") : input, end) (action53 pos a b) stk
; S88 ((p, Reserved ".") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ".") : input, end) (action53 pos a b) stk
; S88 ((p, Reserved "=") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "=") : input, end) (action53 pos a b) stk
; S88 ((p, Reserved "=>") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "=>") : input, end) (action53 pos a
                                                                b) stk
; S89 ((p, Reserved "+") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "+") : input, end) (action53 pos a b) stk
; S89 ((p, Reserved ",") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ",") : input, end) (action53 pos a b) stk
; S89 ((p, Reserved ".") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved ".") : input, end) (action53 pos a b) stk
; S89 ((p, Reserved "=>") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprAdd ((p, Reserved "=>") : input, end) (action53 pos a
                                                                b) stk
; S90 ((p, Reserved "*") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "*") : input, end) (action57 pos a) stk
; S90 ((p, Reserved "+") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "+") : input, end) (action57 pos a) stk
; S90 ((p, Reserved ",") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ",") : input, end) (action57 pos a) stk
; S90 ((p, Reserved ".") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ".") : input, end) (action57 pos a) stk
; S91 ((p, Reserved "*") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "*") : input, end) (action57 pos a) stk
; S91 ((p, Reserved "+") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "+") : input, end) (action57 pos a) stk
; S91 ((p, Reserved ",") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ",") : input, end) (action57 pos a) stk
; S91 ((p, Reserved ".") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ".") : input, end) (action57 pos a) stk
; S91 ((p, Reserved "=") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "=") : input, end) (action57 pos a) stk
; S92 ((p, Reserved "*") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "*") : input, end) (action57 pos a) stk
; S92 ((p, Reserved "+") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "+") : input, end) (action57 pos a) stk
; S92 ((p, Reserved ",") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ",") : input, end) (action57 pos a) stk
; S92 ((p, Reserved ".") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ".") : input, end) (action57 pos a) stk
; S92 ((p, Reserved "=") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "=") : input, end) (action57 pos a) stk
; S92 ((p, Reserved "=>") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "=>") : input, end) (action57 pos a) stk
; S93 ((p, Reserved "*") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "*") : input, end) (action57 pos a) stk
; S93 ((p, Reserved "+") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "+") : input, end) (action57 pos a) stk
; S93 ((p, Reserved ",") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ",") : input, end) (action57 pos a) stk
; S93 ((p, Reserved ".") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ".") : input, end) (action57 pos a) stk
; S93 ((p, Reserved "=>") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "=>") : input, end) (action57 pos a) stk
; S98 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action60 pos n) stk
; S98 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action60 pos n) stk
; S98 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ",") : input, end) (action60 pos n) stk
; S98 ((p, Reserved ".") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ".") : input, end) (action60 pos n) stk
; S99 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action60 pos n) stk
; S99 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action60 pos n) stk
; S99 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ",") : input, end) (action60 pos n) stk
; S99 ((p, Reserved ".") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ".") : input, end) (action60 pos n) stk
; S99 ((p, Reserved "=") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "=") : input, end) (action60 pos n) stk
; S100 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action60 pos n) stk
; S100 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action60 pos n) stk
; S100 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ",") : input, end) (action60 pos n) stk
; S100 ((p, Reserved ".") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ".") : input, end) (action60 pos n) stk
; S100 ((p, Reserved "=") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "=") : input, end) (action60 pos n) stk
; S100 ((p, Reserved "=>") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "=>") : input, end) (action60 pos n) stk
; S101 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action60 pos n) stk
; S101 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action60 pos n) stk
; S101 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ",") : input, end) (action60 pos n) stk
; S101 ((p, Reserved ".") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ".") : input, end) (action60 pos n) stk
; S101 ((p, Reserved "=>") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "=>") : input, end) (action60 pos n) stk
; S102 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action61 pos n) stk
; S102 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action61 pos n) stk
; S102 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ",") : input, end) (action61 pos n) stk
; S102 ((p, Reserved ".") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ".") : input, end) (action61 pos n) stk
; S103 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action61 pos n) stk
; S103 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action61 pos n) stk
; S103 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ",") : input, end) (action61 pos n) stk
; S103 ((p, Reserved ".") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ".") : input, end) (action61 pos n) stk
; S103 ((p, Reserved "=") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "=") : input, end) (action61 pos n) stk
; S104 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action61 pos n) stk
; S104 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action61 pos n) stk
; S104 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ",") : input, end) (action61 pos n) stk
; S104 ((p, Reserved ".") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ".") : input, end) (action61 pos n) stk
; S104 ((p, Reserved "=") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "=") : input, end) (action61 pos n) stk
; S104 ((p, Reserved "=>") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "=>") : input, end) (action61 pos n) stk
; S105 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action61 pos n) stk
; S105 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action61 pos n) stk
; S105 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ",") : input, end) (action61 pos n) stk
; S105 ((p, Reserved ".") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ".") : input, end) (action61 pos n) stk
; S105 ((p, Reserved "=>") : input, end) (n :> stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "=>") : input, end) (action61 pos n) stk
; S106 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "*") : input, end) (action63 pos n) stk
; S106 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "+") : input, end) (action63 pos n) stk
; S106 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ",") : input, end) (action63 pos n) stk
; S106 ((p, Reserved ".") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ".") : input, end) (action63 pos n) stk
; S106 ((p, Reserved "=") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "=") : input, end) (action63 pos n) stk
; S107 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "*") : input, end) (action63 pos n) stk
; S107 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "+") : input, end) (action63 pos n) stk
; S107 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ",") : input, end) (action63 pos n) stk
; S107 ((p, Reserved ".") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ".") : input, end) (action63 pos n) stk
; S107 ((p, Reserved "=") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "=") : input, end) (action63 pos n) stk
; S107 ((p, Reserved "=>") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "=>") : input, end) (action63 pos n) stk
; S108 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "*") : input, end) (action63 pos n) stk
; S108 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "+") : input, end) (action63 pos n) stk
; S108 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ",") : input, end) (action63 pos n) stk
; S108 ((p, Reserved ".") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ".") : input, end) (action63 pos n) stk
; S109 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "*") : input, end) (action63 pos n) stk
; S109 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "+") : input, end) (action63 pos n) stk
; S109 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ",") : input, end) (action63 pos n) stk
; S109 ((p, Reserved ".") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ".") : input, end) (action63 pos n) stk
; S109 ((p, Reserved "=") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "=") : input, end) (action63 pos n) stk
; S110 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "*") : input, end) (action63 pos n) stk
; S110 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "+") : input, end) (action63 pos n) stk
; S110 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ",") : input, end) (action63 pos n) stk
; S110 ((p, Reserved ".") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ".") : input, end) (action63 pos n) stk
; S110 ((p, Reserved "=") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "=") : input, end) (action63 pos n) stk
; S110 ((p, Reserved "=>") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "=>") : input, end) (action63 pos n) stk
; S111 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "*") : input, end) (action63 pos n) stk
; S111 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "+") : input, end) (action63 pos n) stk
; S111 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ",") : input, end) (action63 pos n) stk
; S111 ((p, Reserved ".") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ".") : input, end) (action63 pos n) stk
; S111 ((p, Reserved "=>") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "=>") : input, end) (action63 pos n) stk
; S112 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "*") : input, end) (action64 pos n) stk
; S112 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "+") : input, end) (action64 pos n) stk
; S112 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ",") : input, end) (action64 pos n) stk
; S112 ((p, Reserved ".") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ".") : input, end) (action64 pos n) stk
; S113 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "*") : input, end) (action64 pos n) stk
; S113 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "+") : input, end) (action64 pos n) stk
; S113 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ",") : input, end) (action64 pos n) stk
; S113 ((p, Reserved ".") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ".") : input, end) (action64 pos n) stk
; S113 ((p, Reserved "=") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "=") : input, end) (action64 pos n) stk
; S114 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "*") : input, end) (action64 pos n) stk
; S114 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "+") : input, end) (action64 pos n) stk
; S114 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ",") : input, end) (action64 pos n) stk
; S114 ((p, Reserved ".") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ".") : input, end) (action64 pos n) stk
; S114 ((p, Reserved "=") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "=") : input, end) (action64 pos n) stk
; S114 ((p, Reserved "=>") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "=>") : input, end) (action64 pos n) stk
; S115 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "*") : input, end) (action64 pos n) stk
; S115 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "+") : input, end) (action64 pos n) stk
; S115 ((p, Reserved ",") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ",") : input, end) (action64 pos n) stk
; S115 ((p, Reserved ".") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved ".") : input, end) (action64 pos n) stk
; S115 ((p, Reserved "=>") : input, end) (n :> stk@(_, pos, _)) ->
    gotoConst ((p, Reserved "=>") : input, end) (action64 pos n) stk
; S124 ((p, Reserved "*") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "*") : input, end) (action56 pos a
                                                                b) stk
; S124 ((p, Reserved "+") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "+") : input, end) (action56 pos a
                                                                b) stk
; S124 ((p, Reserved ",") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ",") : input, end) (action56 pos a
                                                                b) stk
; S124 ((p, Reserved ".") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ".") : input, end) (action56 pos a
                                                                b) stk
; S125 ((p, Reserved "*") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "*") : input, end) (action56 pos a
                                                                b) stk
; S125 ((p, Reserved "+") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "+") : input, end) (action56 pos a
                                                                b) stk
; S125 ((p, Reserved ",") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ",") : input, end) (action56 pos a
                                                                b) stk
; S125 ((p, Reserved ".") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ".") : input, end) (action56 pos a
                                                                b) stk
; S125 ((p, Reserved "=") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "=") : input, end) (action56 pos a
                                                                b) stk
; S126 ((p, Reserved "*") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "*") : input, end) (action56 pos a
                                                                b) stk
; S126 ((p, Reserved "+") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "+") : input, end) (action56 pos a
                                                                b) stk
; S126 ((p, Reserved ",") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ",") : input, end) (action56 pos a
                                                                b) stk
; S126 ((p, Reserved ".") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ".") : input, end) (action56 pos a
                                                                b) stk
; S126 ((p, Reserved "=") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "=") : input, end) (action56 pos a
                                                                b) stk
; S126 ((p, Reserved "=>") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "=>") : input, end) (action56 pos a
                                                                 b) stk
; S127 ((p, Reserved "*") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "*") : input, end) (action56 pos a
                                                                b) stk
; S127 ((p, Reserved "+") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "+") : input, end) (action56 pos a
                                                                b) stk
; S127 ((p, Reserved ",") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ",") : input, end) (action56 pos a
                                                                b) stk
; S127 ((p, Reserved ".") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved ".") : input, end) (action56 pos a
                                                                b) stk
; S127 ((p, Reserved "=>") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExprMult ((p, Reserved "=>") : input, end) (action56 pos a
                                                                 b) stk
; S128 ((p, Reserved "*") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action59 pos e) stk
; S128 ((p, Reserved "+") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action59 pos e) stk
; S128 ((p, Reserved ",") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ",") : input, end) (action59 pos e) stk
; S128 ((p, Reserved ".") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ".") : input, end) (action59 pos e) stk
; S129 ((p, Reserved "*") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action59 pos e) stk
; S129 ((p, Reserved "+") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action59 pos e) stk
; S129 ((p, Reserved ",") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ",") : input, end) (action59 pos e) stk
; S129 ((p, Reserved ".") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ".") : input, end) (action59 pos e) stk
; S129 ((p, Reserved "=") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "=") : input, end) (action59 pos e) stk
; S130 ((p, Reserved "*") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action59 pos e) stk
; S130 ((p, Reserved "+") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action59 pos e) stk
; S130 ((p, Reserved ",") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ",") : input, end) (action59 pos e) stk
; S130 ((p, Reserved ".") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ".") : input, end) (action59 pos e) stk
; S130 ((p, Reserved "=") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "=") : input, end) (action59 pos e) stk
; S130 ((p, Reserved "=>") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "=>") : input, end) (action59 pos e) stk
; S131 ((p, Reserved "*") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "*") : input, end) (action59 pos e) stk
; S131 ((p, Reserved "+") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "+") : input, end) (action59 pos e) stk
; S131 ((p, Reserved ",") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ",") : input, end) (action59 pos e) stk
; S131 ((p, Reserved ".") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved ".") : input, end) (action59 pos e) stk
; S131 ((p, Reserved "=>") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoExprTerm ((p, Reserved "=>") : input, end) (action59 pos e) stk
; S132 ((p, Reserved ",") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved ",") : input, end) (action51 pos a) stk
; S132 ((p, Reserved ".") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved ".") : input, end) (action51 pos a) stk
; S133 ((p, Reserved ",") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved ",") : input, end) (action51 pos a) stk
; S133 ((p, Reserved ".") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved ".") : input, end) (action51 pos a) stk
; S133 ((p, Reserved "=>") : input, end) (a :> stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved "=>") : input, end) (action51 pos a) stk
; S134 ((p, Reserved ",") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved ",") : input, end) (action50 pos a b) stk
; S134 ((p, Reserved ".") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved ".") : input, end) (action50 pos a b) stk
; S135 ((p, Reserved ",") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved ",") : input, end) (action50 pos a b) stk
; S135 ((p, Reserved ".") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved ".") : input, end) (action50 pos a b) stk
; S135 ((p, Reserved "=>") : input, end) (b :> _ :? a :? stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved "=>") : input, end) (action50 pos a b) stk
; S142 ((p, Reserved ",") : input, end) (c :> stk@(_, pos, _)) ->
    gotoCond ((p, Reserved ",") : input, end) (action38 pos c) stk
; S142 ((p, Reserved ".") : input, end) (c :> stk@(_, pos, _)) ->
    gotoCond ((p, Reserved ".") : input, end) (action38 pos c) stk
; S143 ((p, Reserved ",") : input, end) (c :> stk@(_, pos, _)) ->
    gotoCond ((p, Reserved ",") : input, end) (action38 pos c) stk
; S143 ((p, Reserved ".") : input, end) (c :> stk@(_, pos, _)) ->
    gotoCond ((p, Reserved ".") : input, end) (action38 pos c) stk
; S143 ((p, Reserved "=>") : input, end) (c :> stk@(_, pos, _)) ->
    gotoCond ((p, Reserved "=>") : input, end) (action38 pos c) stk
; S146 ((p, Reserved ",") : input, end) (e :> stk@(_, pos, _)) ->
    gotoCond ((p, Reserved ",") : input, end) (action40 pos e) stk
; S146 ((p, Reserved ".") : input, end) (e :> stk@(_, pos, _)) ->
    gotoCond ((p, Reserved ".") : input, end) (action40 pos e) stk
; S147 ((p, Reserved ",") : input, end) (e :> stk@(_, pos, _)) ->
    gotoCond ((p, Reserved ",") : input, end) (action40 pos e) stk
; S147 ((p, Reserved ".") : input, end) (e :> stk@(_, pos, _)) ->
    gotoCond ((p, Reserved ".") : input, end) (action40 pos e) stk
; S147 ((p, Reserved "=>") : input, end) (e :> stk@(_, pos, _)) ->
    gotoCond ((p, Reserved "=>") : input, end) (action40 pos e) stk
; S152 ((p, Reserved ",") : input, end) (c :> _ :? stk@(_, pos, _)) ->
    gotoChange ((p, Reserved ",") : input, end) (action29 pos c) stk
; S152 ((p, Reserved ".") : input, end) (c :> _ :? stk@(_, pos, _)) ->
    gotoChange ((p, Reserved ".") : input, end) (action29 pos c) stk
; S153 ((p, Reserved ",") : input, end) (c :> _ :? stk@(_, pos, _)) ->
    gotoChange ((p, Reserved ",") : input, end) (action30 pos c) stk
; S153 ((p, Reserved ".") : input, end) (c :> _ :? stk@(_, pos, _)) ->
    gotoChange ((p, Reserved ".") : input, end) (action30 pos c) stk
; S154 ((p, Reserved ",") : input, end) (c :> _ :? stk@(_, pos, _)) ->
    gotoCond ((p, Reserved ",") : input, end) (action39 pos c) stk
; S154 ((p, Reserved ".") : input, end) (c :> _ :? stk@(_, pos, _)) ->
    gotoCond ((p, Reserved ".") : input, end) (action39 pos c) stk
; S155 ((p, Reserved ",") : input, end) (c :> _ :? stk@(_, pos, _)) ->
    gotoCond ((p, Reserved ",") : input, end) (action39 pos c) stk
; S155 ((p, Reserved ".") : input, end) (c :> _ :? stk@(_, pos, _)) ->
    gotoCond ((p, Reserved ".") : input, end) (action39 pos c) stk
; S155 ((p, Reserved "=>") : input, end) (c :> _ :? stk@(_, pos, _)) ->
    gotoCond ((p, Reserved "=>") : input, end) (action39 pos c) stk
; S156 ((p, Reserved ",") : input, end) (t :> pred :? stk@(_, pos, _)) ->
    gotoCall ((p, Reserved ",") : input, end) (action42 pos pred t) stk
; S156 ((p, Reserved ".") : input, end) (t :> pred :? stk@(_, pos, _)) ->
    gotoCall ((p, Reserved ".") : input, end) (action42 pos pred t) stk
; S157 ((p, Reserved ",") : input, end) (t :> pred :? stk@(_, pos, _)) ->
    gotoCall ((p, Reserved ",") : input, end) (action42 pos pred t) stk
; S157 ((p, Reserved ".") : input, end) (t :> pred :? stk@(_, pos, _)) ->
    gotoCall ((p, Reserved ".") : input, end) (action42 pos pred t) stk
; S157 ((p, Reserved "=>") : input, end) (t :> pred :? stk@(_, pos, _)) ->
    gotoCall ((p, Reserved "=>") : input, end) (action42 pos pred
                                                             t) stk
; S158 ((p, Reserved ",") : input, end) (_ :> _ :? stk@(_, pos, _)) ->
    gotoTuple ((p, Reserved ",") : input, end) (action44 pos ) stk
; S158 ((p, Reserved ".") : input, end) (_ :> _ :? stk@(_, pos, _)) ->
    gotoTuple ((p, Reserved ".") : input, end) (action44 pos ) stk
; S159 ((p, Reserved ",") : input, end) (_ :> _ :? stk@(_, pos, _)) ->
    gotoTuple ((p, Reserved ",") : input, end) (action44 pos ) stk
; S159 ((p, Reserved ".") : input, end) (_ :> _ :? stk@(_, pos, _)) ->
    gotoTuple ((p, Reserved ".") : input, end) (action44 pos ) stk
; S159 ((p, Reserved "=>") : input, end) (_ :> _ :? stk@(_, pos, _)) ->
    gotoTuple ((p, Reserved "=>") : input, end) (action44 pos ) stk
; S164 ((p, Reserved ",") : input, end) (_ :> es :? _ :? stk@(_, pos, _)) ->
    gotoTuple ((p, Reserved ",") : input, end) (action45 pos es) stk
; S164 ((p, Reserved ".") : input, end) (_ :> es :? _ :? stk@(_, pos, _)) ->
    gotoTuple ((p, Reserved ".") : input, end) (action45 pos es) stk
; S165 ((p, Reserved ",") : input, end) (_ :> es :? _ :? stk@(_, pos, _)) ->
    gotoTuple ((p, Reserved ",") : input, end) (action45 pos es) stk
; S165 ((p, Reserved ".") : input, end) (_ :> es :? _ :? stk@(_, pos, _)) ->
    gotoTuple ((p, Reserved ".") : input, end) (action45 pos es) stk
; S165 ((p, Reserved "=>") : input, end) (_ :> es :? _ :? stk@(_, pos, _)) ->
    gotoTuple ((p, Reserved "=>") : input, end) (action45 pos es) stk
; S168 ((p, Reserved "->") : input, end) (t :> pred :? stk@(_, pos, _)) ->
    gotoCall ((p, Reserved "->") : input, end) (action42 pos pred
                                                             t) stk
; S168 ((p, Reserved ".") : input, end) (t :> pred :? stk@(_, pos, _)) ->
    gotoCall ((p, Reserved ".") : input, end) (action42 pos pred t) stk
; S168 ((p, Reserved "<-") : input, end) (t :> pred :? stk@(_, pos, _)) ->
    gotoCall ((p, Reserved "<-") : input, end) (action42 pos pred
                                                             t) stk
; S168 ((p, Reserved "=>") : input, end) (t :> pred :? stk@(_, pos, _)) ->
    gotoCall ((p, Reserved "=>") : input, end) (action42 pos pred
                                                             t) stk
; S169 ((p, Reserved "->") : input, end) (_ :> _ :? stk@(_, pos, _)) ->
    gotoTuple ((p, Reserved "->") : input, end) (action44 pos ) stk
; S169 ((p, Reserved ".") : input, end) (_ :> _ :? stk@(_, pos, _)) ->
    gotoTuple ((p, Reserved ".") : input, end) (action44 pos ) stk
; S169 ((p, Reserved "<-") : input, end) (_ :> _ :? stk@(_, pos, _)) ->
    gotoTuple ((p, Reserved "<-") : input, end) (action44 pos ) stk
; S169 ((p, Reserved "=>") : input, end) (_ :> _ :? stk@(_, pos, _)) ->
    gotoTuple ((p, Reserved "=>") : input, end) (action44 pos ) stk
; S171 ((p, Reserved "->") : input, end) (_ :> es :? _ :? stk@(_, pos, _)) ->
    gotoTuple ((p, Reserved "->") : input, end) (action45 pos es) stk
; S171 ((p, Reserved ".") : input, end) (_ :> es :? _ :? stk@(_, pos, _)) ->
    gotoTuple ((p, Reserved ".") : input, end) (action45 pos es) stk
; S171 ((p, Reserved "<-") : input, end) (_ :> es :? _ :? stk@(_, pos, _)) ->
    gotoTuple ((p, Reserved "<-") : input, end) (action45 pos es) stk
; S171 ((p, Reserved "=>") : input, end) (_ :> es :? _ :? stk@(_, pos, _)) ->
    gotoTuple ((p, Reserved "=>") : input, end) (action45 pos es) stk
; S172 ((p, Reserved ".") : input, end) (c :> stk@(_, pos, _)) ->
    gotoChanges ((p, Reserved ".") : input, end) (action27 pos c) stk
; S173 ((p, Reserved ".") : input, end) (c :> stk@(_, pos, _)) ->
    gotoConds ((p, Reserved ".") : input, end) (action36 pos c) stk
; S174 ((p, Reserved ".") : input, end) (c :> stk@(_, pos, _)) ->
    gotoConds ((p, Reserved ".") : input, end) (action36 pos c) stk
; S174 ((p, Reserved "=>") : input, end) (c :> stk@(_, pos, _)) ->
    gotoConds ((p, Reserved "=>") : input, end) (action36 pos c) stk
; S178 ((p, Reserved ".") : input, end) (cs :> _ :? c :? stk@(_, pos, _)) ->
    gotoChanges ((p, Reserved ".") : input, end) (action26 pos c
                                                               cs) stk
; S179 ((p, Reserved ".") : input, end) (cs :> _ :? c :? stk@(_, pos, _)) ->
    gotoConds ((p, Reserved ".") : input, end) (action35 pos c cs) stk
; S180 ((p, Reserved ".") : input, end) (cs :> _ :? c :? stk@(_, pos, _)) ->
    gotoConds ((p, Reserved ".") : input, end) (action35 pos c cs) stk
; S180 ((p, Reserved "=>") : input, end) (cs :> _ :? c :? stk@(_, pos, _)) ->
    gotoConds ((p, Reserved "=>") : input, end) (action35 pos c cs) stk
; S0 input _ -> Left  (currentPos input, ["<name>"])
; S1 input _ -> Left  (currentPos input, ["$"])
; S2 input _ -> Left  (currentPos input, ["$"])
; S3 input _ -> Left  (currentPos input, ["$", "<name>"])
; S4 input _ -> Left  (currentPos input, ["$", "<name>"])
; S5 input _ -> Left  (currentPos input, ["$", "<name>"])
; S6 input _ -> Left  (currentPos input, ["->", ".", "<-", "=>"])
; S7 input _ -> Left  (currentPos input, ["$"])
; S8 input _ -> Left  (currentPos input, ["+", "-"])
; S9 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>", "~"])
; S10 input _ -> Left  (currentPos input, ["$", "<name>"])
; S11 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>", "~"])
; S12 input _ -> Left  (currentPos input, ["."])
; S13 input _ -> Left  (currentPos input, [".", "=>"])
; S14 input _ -> Left  (currentPos input, ["."])
; S15 input _ -> Left  (currentPos input, ["$", "<name>"])
; S16 input _ -> Left  (currentPos input, ["+", "-"])
; S17 input _ -> Left  (currentPos input, ["$", "<name>"])
; S18 input _ -> Left  (currentPos input, ["$", "<name>"])
; S19 input _ -> Left  (currentPos input, ["."])
; S20 input _ -> Left  (currentPos input, ["$", "<name>"])
; S21 input _ -> Left  (currentPos input, [")", ","])
; S22 input _ -> Left  (currentPos input, [")", "+", ",", "="])
; S23 input _ -> Left  (currentPos input, [")", "+", "="])
; S24 input _ -> Left  (currentPos input, [")", "+"])
; S25 input _ -> Left  (currentPos input, [")", "+", ","])
; S26 input _ -> Left  (currentPos input, [")", "*", "+"])
; S27 input _ -> Left  (currentPos input, [")", "*", "+", ","])
; S28 input _ ->
    Left  (currentPos input, [")", "*", "+", ",", "="])
; S29 input _ -> Left  (currentPos input, [")", "*", "+", "="])
; S30 input _ -> Left  (currentPos input, [")", "*", "+"])
; S31 input _ -> Left  (currentPos input, [")", "*", "+", ","])
; S32 input _ ->
    Left  (currentPos input, [")", "*", "+", ",", "="])
; S33 input _ -> Left  (currentPos input, [")", "*", "+", "="])
; S34 input _ -> Left  (currentPos input, [")", "*", "+"])
; S35 input _ -> Left  (currentPos input, [")", "*", "+", ","])
; S36 input _ ->
    Left  (currentPos input, [")", "*", "+", ",", "="])
; S37 input _ -> Left  (currentPos input, [")", "*", "+", "="])
; S38 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S39 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S40 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S41 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S42 input _ -> Left  (currentPos input, [")", "*", "+"])
; S43 input _ -> Left  (currentPos input, [")", "*", "+", ","])
; S44 input _ ->
    Left  (currentPos input, [")", "*", "+", ",", "="])
; S45 input _ -> Left  (currentPos input, [")", "*", "+", "="])
; S46 input _ -> Left  (currentPos input, [")", "*", "+"])
; S47 input _ -> Left  (currentPos input, [")", "*", "+", ","])
; S48 input _ ->
    Left  (currentPos input, [")", "*", "+", ",", "="])
; S49 input _ -> Left  (currentPos input, [")", "*", "+", "="])
; S50 input _ -> Left  (currentPos input, [")", "*", "+"])
; S51 input _ -> Left  (currentPos input, [")", "*", "+", ","])
; S52 input _ ->
    Left  (currentPos input, [")", "*", "+", ",", "="])
; S53 input _ -> Left  (currentPos input, [")", "*", "+", "="])
; S54 input _ -> Left  (currentPos input, [")", "*", "+"])
; S55 input _ -> Left  (currentPos input, [")", "*", "+", ","])
; S56 input _ ->
    Left  (currentPos input, [")", "*", "+", ",", "="])
; S57 input _ -> Left  (currentPos input, [")", "*", "+", "="])
; S58 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S59 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S60 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S61 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S62 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S63 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S64 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S65 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S66 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S67 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S68 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S69 input _ -> Left  (currentPos input, [")"])
; S70 input _ -> Left  (currentPos input, [")"])
; S71 input _ -> Left  (currentPos input, [")"])
; S72 input _ -> Left  (currentPos input, [")"])
; S73 input _ -> Left  (currentPos input, [")"])
; S74 input _ -> Left  (currentPos input, [")", "*", "+"])
; S75 input _ -> Left  (currentPos input, [")", "*", "+", ","])
; S76 input _ ->
    Left  (currentPos input, [")", "*", "+", ",", "="])
; S77 input _ -> Left  (currentPos input, [")", "*", "+", "="])
; S78 input _ -> Left  (currentPos input, [")", "*", "+"])
; S79 input _ -> Left  (currentPos input, [")", "*", "+", ","])
; S80 input _ ->
    Left  (currentPos input, [")", "*", "+", ",", "="])
; S81 input _ -> Left  (currentPos input, [")", "*", "+", "="])
; S82 input _ -> Left  (currentPos input, ["*", "+", ",", "."])
; S83 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "="])
; S84 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "=", "=>"])
; S85 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "=>"])
; S86 input _ -> Left  (currentPos input, ["*", "+", ",", "."])
; S87 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "="])
; S88 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "=", "=>"])
; S89 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "=>"])
; S90 input _ -> Left  (currentPos input, ["*", "+", ",", "."])
; S91 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "="])
; S92 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "=", "=>"])
; S93 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "=>"])
; S94 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S95 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S96 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S97 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S98 input _ -> Left  (currentPos input, ["*", "+", ",", "."])
; S99 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "="])
; S100 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "=", "=>"])
; S101 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "=>"])
; S102 input _ -> Left  (currentPos input, ["*", "+", ",", "."])
; S103 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "="])
; S104 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "=", "=>"])
; S105 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "=>"])
; S106 input _ ->
    Left  (currentPos input, ["(", "*", "+", ",", ".", "="])
; S107 input _ ->
    Left  (currentPos input, ["(", "*", "+", ",", ".", "=", "=>"])
; S108 input _ -> Left  (currentPos input, ["*", "+", ",", "."])
; S109 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "="])
; S110 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "=", "=>"])
; S111 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "=>"])
; S112 input _ -> Left  (currentPos input, ["*", "+", ",", "."])
; S113 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "="])
; S114 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "=", "=>"])
; S115 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "=>"])
; S116 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S117 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S118 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S119 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S120 input _ -> Left  (currentPos input, [")"])
; S121 input _ -> Left  (currentPos input, [")"])
; S122 input _ -> Left  (currentPos input, [")"])
; S123 input _ -> Left  (currentPos input, [")"])
; S124 input _ -> Left  (currentPos input, ["*", "+", ",", "."])
; S125 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "="])
; S126 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "=", "=>"])
; S127 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "=>"])
; S128 input _ -> Left  (currentPos input, ["*", "+", ",", "."])
; S129 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "="])
; S130 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "=", "=>"])
; S131 input _ ->
    Left  (currentPos input, ["*", "+", ",", ".", "=>"])
; S132 input _ -> Left  (currentPos input, ["+", ",", ".", "="])
; S133 input _ ->
    Left  (currentPos input, ["+", ",", ".", "=", "=>"])
; S134 input _ -> Left  (currentPos input, ["+", ",", "."])
; S135 input _ -> Left  (currentPos input, ["+", ",", ".", "=>"])
; S136 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S137 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S138 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S139 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S140 input _ -> Left  (currentPos input, ["<name>"])
; S141 input _ -> Left  (currentPos input, ["<name>"])
; S142 input _ -> Left  (currentPos input, [",", "."])
; S143 input _ -> Left  (currentPos input, [",", ".", "=>"])
; S144 input _ -> Left  (currentPos input, ["<name>"])
; S145 input _ -> Left  (currentPos input, ["<name>"])
; S146 input _ -> Left  (currentPos input, [",", "."])
; S147 input _ -> Left  (currentPos input, [",", ".", "=>"])
; S148 input _ -> Left  (currentPos input, ["("])
; S149 input _ -> Left  (currentPos input, ["("])
; S150 input _ ->
    Left  (currentPos input, ["(", ")", "<Name>", "<name>", "<num>"])
; S151 input _ ->
    Left  (currentPos input, ["(", ")", "<Name>", "<name>", "<num>"])
; S152 input _ -> Left  (currentPos input, [",", "."])
; S153 input _ -> Left  (currentPos input, [",", "."])
; S154 input _ -> Left  (currentPos input, [",", "."])
; S155 input _ -> Left  (currentPos input, [",", ".", "=>"])
; S156 input _ -> Left  (currentPos input, [",", "."])
; S157 input _ -> Left  (currentPos input, [",", ".", "=>"])
; S158 input _ -> Left  (currentPos input, [",", "."])
; S159 input _ -> Left  (currentPos input, [",", ".", "=>"])
; S160 input _ -> Left  (currentPos input, [")"])
; S161 input _ -> Left  (currentPos input, [")"])
; S162 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S163 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>"])
; S164 input _ -> Left  (currentPos input, [",", "."])
; S165 input _ -> Left  (currentPos input, [",", ".", "=>"])
; S166 input _ -> Left  (currentPos input, ["("])
; S167 input _ ->
    Left  (currentPos input, ["(", ")", "<Name>", "<name>", "<num>"])
; S168 input _ -> Left  (currentPos input, ["->", ".", "<-", "=>"])
; S169 input _ -> Left  (currentPos input, ["->", ".", "<-", "=>"])
; S170 input _ -> Left  (currentPos input, [")"])
; S171 input _ -> Left  (currentPos input, ["->", ".", "<-", "=>"])
; S172 input _ -> Left  (currentPos input, [",", "."])
; S173 input _ -> Left  (currentPos input, [",", "."])
; S174 input _ -> Left  (currentPos input, [",", ".", "=>"])
; S175 input _ -> Left  (currentPos input, ["+", "-"])
; S176 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>", "~"])
; S177 input _ ->
    Left  (currentPos input, ["(", "<Name>", "<name>", "<num>", "~"])
; S178 input _ -> Left  (currentPos input, ["."])
; S179 input _ -> Left  (currentPos input, ["."])
; S180 input _ -> Left  (currentPos input, [".", "=>"])
} where {
; action0 pos res =
{-# LINE  0 "<nowhere>" #-}
res
; action10 pos stmts =
{-# LINE  10 "gen-lr1-parser/example/language.grammar" #-}
                                       Program {stmts} 
; action12 pos c cs =
{-# LINE  12 "gen-lr1-parser/example/language.grammar" #-}
                                       c : cs 
; action13 pos c =
{-# LINE  13 "gen-lr1-parser/example/language.grammar" #-}
                                       [c] 
; action15 pos c =
{-# LINE  15 "gen-lr1-parser/example/language.grammar" #-}
                                       StmtClause pos c 
; action16 pos e =
{-# LINE  16 "gen-lr1-parser/example/language.grammar" #-}
                                       StmtEffect pos e 
; action18 pos c ds =
{-# LINE  18 "gen-lr1-parser/example/language.grammar" #-}
                                                       Effect pos c [] ds 
; action19 pos c cs ds =
{-# LINE  19 "gen-lr1-parser/example/language.grammar" #-}
                                                       Effect pos c cs ds 
; action21 pos c ds =
{-# LINE  21 "gen-lr1-parser/example/language.grammar" #-}
                                                       Effect pos c [] ds 
; action22 pos c cs ds =
{-# LINE  22 "gen-lr1-parser/example/language.grammar" #-}
                                                       Effect pos c cs ds 
; action23 pos c cs =
{-# LINE  23 "gen-lr1-parser/example/language.grammar" #-}
                                                       Effect pos c cs [] 
; action24 pos c =
{-# LINE  24 "gen-lr1-parser/example/language.grammar" #-}
                                                       Effect pos c [] [] 
; action26 pos c cs =
{-# LINE  26 "gen-lr1-parser/example/language.grammar" #-}
                                       c : cs 
; action27 pos c =
{-# LINE  27 "gen-lr1-parser/example/language.grammar" #-}
                                       [c] 
; action29 pos c =
{-# LINE  29 "gen-lr1-parser/example/language.grammar" #-}
                                       Assert pos c 
; action30 pos c =
{-# LINE  30 "gen-lr1-parser/example/language.grammar" #-}
                                       Refute pos c 
; action32 pos c =
{-# LINE  32 "gen-lr1-parser/example/language.grammar" #-}
                                       Clause pos c [] 
; action33 pos c cs =
{-# LINE  33 "gen-lr1-parser/example/language.grammar" #-}
                                       Clause pos c cs 
; action35 pos c cs =
{-# LINE  35 "gen-lr1-parser/example/language.grammar" #-}
                                       c : cs 
; action36 pos c =
{-# LINE  36 "gen-lr1-parser/example/language.grammar" #-}
                                       [c] 
; action38 pos c =
{-# LINE  38 "gen-lr1-parser/example/language.grammar" #-}
                                       CondAssert pos c 
; action39 pos c =
{-# LINE  39 "gen-lr1-parser/example/language.grammar" #-}
                                       CondRefute pos c 
; action40 pos e =
{-# LINE  40 "gen-lr1-parser/example/language.grammar" #-}
                                       CondGuard  pos e 
; action42 pos pred t =
{-# LINE  42 "gen-lr1-parser/example/language.grammar" #-}
                                       Call pos pred t 
; action44 pos =
{-# LINE  44 "gen-lr1-parser/example/language.grammar" #-}
                                       [] 
; action45 pos es =
{-# LINE  45 "gen-lr1-parser/example/language.grammar" #-}
                                       es 
; action47 pos e es =
{-# LINE  47 "gen-lr1-parser/example/language.grammar" #-}
                                       e : es 
; action48 pos e =
{-# LINE  48 "gen-lr1-parser/example/language.grammar" #-}
                                       [e] 
; action50 pos a b =
{-# LINE  50 "gen-lr1-parser/example/language.grammar" #-}
                                       ExprBinary pos a Equals b 
; action51 pos a =
{-# LINE  51 "gen-lr1-parser/example/language.grammar" #-}
                                       a 
; action53 pos a b =
{-# LINE  53 "gen-lr1-parser/example/language.grammar" #-}
                                       ExprBinary pos a Add b 
; action54 pos a =
{-# LINE  54 "gen-lr1-parser/example/language.grammar" #-}
                                       a 
; action56 pos a b =
{-# LINE  56 "gen-lr1-parser/example/language.grammar" #-}
                                       ExprBinary pos a Mult b 
; action57 pos a =
{-# LINE  57 "gen-lr1-parser/example/language.grammar" #-}
                                       a 
; action59 pos e =
{-# LINE  59 "gen-lr1-parser/example/language.grammar" #-}
                                       e 
; action60 pos n =
{-# LINE  60 "gen-lr1-parser/example/language.grammar" #-}
                                       ExprVar   pos n 
; action61 pos n =
{-# LINE  61 "gen-lr1-parser/example/language.grammar" #-}
                                       ExprConst pos n 
; action63 pos n =
{-# LINE  63 "gen-lr1-parser/example/language.grammar" #-}
                                       ConstNamed pos n 
; action64 pos n =
{-# LINE  64 "gen-lr1-parser/example/language.grammar" #-}
                                       ConstInt   pos n 
}
  
currentPos :: ([Lexeme], Pos) -> Pos
currentPos = \case
  ([],           end) -> end
  ((pos, _) : _, _)   -> pos
  
parse :: FilePath -> IO (Either LexerError (Either (Pos, [String]) Program))
parse filepath = do
  text <- Text.readFile filepath
  case lexText filepath text ["(", ")", "*", "+", ",", "-", "->",
                              ".", "<-", "=", "=>", "~"] of
    Left  err   -> pure (Left err)
    Right input -> pure (Right (run S0 input Nil))