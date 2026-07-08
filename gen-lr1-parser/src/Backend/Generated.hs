{-# language PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Backend.Generated (parse) where

import Data.Text.IO.Utf8 qualified as Text
import Data.Kind qualified as Kind
import Backend.AST
import Data.Text.Position (Pos)
import Backend.DefaultLexer

data Stack' xs where
  Nil  ::      Stack' '[]
  (:>) :: x -> Stack xs -> Stack' (x : xs)

type Stack a = (St a, Pos, Stack' a)

pattern (:?) :: a -> Stack xs -> Stack (a : xs)
pattern a :? xs <- (_, _, a :> xs)

infixr 2 :>, :?

data St :: [Kind.Type] -> Kind.Type where
  S0 :: forall a. St (a)
  S1 :: forall a. St (Expr : a)
  S2 :: forall a. St (Factor : a)
  S3 :: forall a. St (Factor : () : Expr : a)
  S4 :: forall a. St (Term : a)
  S5 :: forall a. St (Integer : a)
  S6 :: forall a. St (() : a)
  S7 :: forall a. St (() : Expr : a)
  S8 :: forall a. St (() : Factor : a)
  S9 :: forall a. St (Expr : () : a)
  S10 :: forall a. St (Term : () : Factor : a)
  S11 :: forall a. St (() : Expr : () : a)
  S12 :: forall a. St (Expr : () : a)
  S13 :: forall a. St (Factor : a)
  S14 :: forall a. St (Factor : () : Expr : a)
  S15 :: forall a. St (Term : a)
  S16 :: forall a. St (Integer : a)
  S17 :: forall a. St (() : a)
  S18 :: forall a. St (() : Expr : a)
  S19 :: forall a. St (() : Factor : a)
  S20 :: forall a. St (Term : () : Factor : a)
  S21 :: forall a. St (() : Expr : () : a)

gotoExpr :: ([Lexeme], Pos) -> Expr -> Stack a -> Either (Pos, [String]) Expr
gotoExpr toks term stk@(state, _, _) = case state of
  S0 -> run S1 toks (term :> stk)
  S6 -> run S9 toks (term :> stk)
  S17 -> run S12 toks (term :> stk)
  _ -> error ""

gotoFactor :: ([Lexeme], Pos) -> Factor -> Stack a -> Either (Pos, [String]) Expr
gotoFactor toks term stk@(state, _, _) = case state of
  S0 -> run S2 toks (term :> stk)
  S6 -> run S13 toks (term :> stk)
  S7 -> run S3 toks (term :> stk)
  S17 -> run S13 toks (term :> stk)
  S18 -> run S14 toks (term :> stk)
  _ -> error ""

gotoTerm :: ([Lexeme], Pos) -> Term -> Stack a -> Either (Pos, [String]) Expr
gotoTerm toks term stk@(state, _, _) = case state of
  S0 -> run S4 toks (term :> stk)
  S6 -> run S15 toks (term :> stk)
  S7 -> run S4 toks (term :> stk)
  S8 -> run S10 toks (term :> stk)
  S17 -> run S15 toks (term :> stk)
  S18 -> run S15 toks (term :> stk)
  S19 -> run S20 toks (term :> stk)
  _ -> error ""

run :: St a -> ([Lexeme], Pos) -> Stack' a -> Either (Pos, [String]) Expr
run = \cases {
; S0 ((p, Reserved "(") : input, end) stk ->
    run S6 (input, end) (() :> (S0, p, stk))
; S0 ((p, NumberLiteral n) : input, end) stk ->
    run S5 (input, end) (n :> (S0, p, stk))
; S1 ((p, Reserved "+") : input, end) stk ->
    run S7 (input, end) (() :> (S1, p, stk))
; S2 ((p, Reserved "*") : input, end) stk ->
    run S8 (input, end) (() :> (S2, p, stk))
; S3 ((p, Reserved "*") : input, end) stk ->
    run S8 (input, end) (() :> (S3, p, stk))
; S6 ((p, Reserved "(") : input, end) stk ->
    run S17 (input, end) (() :> (S6, p, stk))
; S6 ((p, NumberLiteral n) : input, end) stk ->
    run S16 (input, end) (n :> (S6, p, stk))
; S7 ((p, Reserved "(") : input, end) stk ->
    run S6 (input, end) (() :> (S7, p, stk))
; S7 ((p, NumberLiteral n) : input, end) stk ->
    run S5 (input, end) (n :> (S7, p, stk))
; S8 ((p, Reserved "(") : input, end) stk ->
    run S6 (input, end) (() :> (S8, p, stk))
; S8 ((p, NumberLiteral n) : input, end) stk ->
    run S5 (input, end) (n :> (S8, p, stk))
; S9 ((p, Reserved ")") : input, end) stk ->
    run S11 (input, end) (() :> (S9, p, stk))
; S9 ((p, Reserved "+") : input, end) stk ->
    run S18 (input, end) (() :> (S9, p, stk))
; S12 ((p, Reserved ")") : input, end) stk ->
    run S21 (input, end) (() :> (S12, p, stk))
; S12 ((p, Reserved "+") : input, end) stk ->
    run S18 (input, end) (() :> (S12, p, stk))
; S13 ((p, Reserved "*") : input, end) stk ->
    run S19 (input, end) (() :> (S13, p, stk))
; S14 ((p, Reserved "*") : input, end) stk ->
    run S19 (input, end) (() :> (S14, p, stk))
; S17 ((p, Reserved "(") : input, end) stk ->
    run S17 (input, end) (() :> (S17, p, stk))
; S17 ((p, NumberLiteral n) : input, end) stk ->
    run S16 (input, end) (n :> (S17, p, stk))
; S18 ((p, Reserved "(") : input, end) stk ->
    run S17 (input, end) (() :> (S18, p, stk))
; S18 ((p, NumberLiteral n) : input, end) stk ->
    run S16 (input, end) (n :> (S18, p, stk))
; S19 ((p, Reserved "(") : input, end) stk ->
    run S17 (input, end) (() :> (S19, p, stk))
; S19 ((p, NumberLiteral n) : input, end) stk ->
    run S16 (input, end) (n :> (S19, p, stk))
; S1 ([], end) (res :> stk@(_, pos, _)) ->
    gotoExpr ([], end) (action0 pos res) stk
; S2 ([], end) (f :> stk@(_, pos, _)) ->
    gotoExpr ([], end) (action8 pos f) stk
; S2 ((p, Reserved "+") : input, end) (f :> stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved "+") : input, end) (action8 pos f) stk
; S3 ([], end) (f :> _ :? e :? stk@(_, pos, _)) ->
    gotoExpr ([], end) (action7 pos e f) stk
; S3 ((p, Reserved "+") : input, end) (f :> _ :? e :? stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved "+") : input, end) (action7 pos e f) stk
; S4 ([], end) (t :> stk@(_, pos, _)) ->
    gotoFactor ([], end) (action11 pos t) stk
; S4 ((p, Reserved "*") : input, end) (t :> stk@(_, pos, _)) ->
    gotoFactor ((p, Reserved "*") : input, end) (action11 pos t) stk
; S4 ((p, Reserved "+") : input, end) (t :> stk@(_, pos, _)) ->
    gotoFactor ((p, Reserved "+") : input, end) (action11 pos t) stk
; S5 ([], end) (n :> stk@(_, pos, _)) ->
    gotoTerm ([], end) (action13 pos n) stk
; S5 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoTerm ((p, Reserved "*") : input, end) (action13 pos n) stk
; S5 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoTerm ((p, Reserved "+") : input, end) (action13 pos n) stk
; S10 ([], end) (t :> _ :? f :? stk@(_, pos, _)) ->
    gotoFactor ([], end) (action10 pos f t) stk
; S10 ((p, Reserved "*") : input, end) (t :> _ :? f :? stk@(_, pos, _)) ->
    gotoFactor ((p, Reserved "*") : input, end) (action10 pos f t) stk
; S10 ((p, Reserved "+") : input, end) (t :> _ :? f :? stk@(_, pos, _)) ->
    gotoFactor ((p, Reserved "+") : input, end) (action10 pos f t) stk
; S11 ([], end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoTerm ([], end) (action14 pos e) stk
; S11 ((p, Reserved "*") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoTerm ((p, Reserved "*") : input, end) (action14 pos e) stk
; S11 ((p, Reserved "+") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoTerm ((p, Reserved "+") : input, end) (action14 pos e) stk
; S13 ((p, Reserved ")") : input, end) (f :> stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved ")") : input, end) (action8 pos f) stk
; S13 ((p, Reserved "+") : input, end) (f :> stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved "+") : input, end) (action8 pos f) stk
; S14 ((p, Reserved ")") : input, end) (f :> _ :? e :? stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved ")") : input, end) (action7 pos e f) stk
; S14 ((p, Reserved "+") : input, end) (f :> _ :? e :? stk@(_, pos, _)) ->
    gotoExpr ((p, Reserved "+") : input, end) (action7 pos e f) stk
; S15 ((p, Reserved ")") : input, end) (t :> stk@(_, pos, _)) ->
    gotoFactor ((p, Reserved ")") : input, end) (action11 pos t) stk
; S15 ((p, Reserved "*") : input, end) (t :> stk@(_, pos, _)) ->
    gotoFactor ((p, Reserved "*") : input, end) (action11 pos t) stk
; S15 ((p, Reserved "+") : input, end) (t :> stk@(_, pos, _)) ->
    gotoFactor ((p, Reserved "+") : input, end) (action11 pos t) stk
; S16 ((p, Reserved ")") : input, end) (n :> stk@(_, pos, _)) ->
    gotoTerm ((p, Reserved ")") : input, end) (action13 pos n) stk
; S16 ((p, Reserved "*") : input, end) (n :> stk@(_, pos, _)) ->
    gotoTerm ((p, Reserved "*") : input, end) (action13 pos n) stk
; S16 ((p, Reserved "+") : input, end) (n :> stk@(_, pos, _)) ->
    gotoTerm ((p, Reserved "+") : input, end) (action13 pos n) stk
; S20 ((p, Reserved ")") : input, end) (t :> _ :? f :? stk@(_, pos, _)) ->
    gotoFactor ((p, Reserved ")") : input, end) (action10 pos f t) stk
; S20 ((p, Reserved "*") : input, end) (t :> _ :? f :? stk@(_, pos, _)) ->
    gotoFactor ((p, Reserved "*") : input, end) (action10 pos f t) stk
; S20 ((p, Reserved "+") : input, end) (t :> _ :? f :? stk@(_, pos, _)) ->
    gotoFactor ((p, Reserved "+") : input, end) (action10 pos f t) stk
; S21 ((p, Reserved ")") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoTerm ((p, Reserved ")") : input, end) (action14 pos e) stk
; S21 ((p, Reserved "*") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoTerm ((p, Reserved "*") : input, end) (action14 pos e) stk
; S21 ((p, Reserved "+") : input, end) (_ :> e :? _ :? stk@(_, pos, _)) ->
    gotoTerm ((p, Reserved "+") : input, end) (action14 pos e) stk
; S0 input _ -> Left  (currentPos input, ["(", "<num>"])
; S1 input _ -> Left  (currentPos input, ["$", "+"])
; S2 input _ -> Left  (currentPos input, ["$", "*", "+"])
; S3 input _ -> Left  (currentPos input, ["$", "*", "+"])
; S4 input _ -> Left  (currentPos input, ["$", "*", "+"])
; S5 input _ -> Left  (currentPos input, ["$", "*", "+"])
; S6 input _ -> Left  (currentPos input, ["(", "<num>"])
; S7 input _ -> Left  (currentPos input, ["(", "<num>"])
; S8 input _ -> Left  (currentPos input, ["(", "<num>"])
; S9 input _ -> Left  (currentPos input, [")", "+"])
; S10 input _ -> Left  (currentPos input, ["$", "*", "+"])
; S11 input _ -> Left  (currentPos input, ["$", "*", "+"])
; S12 input _ -> Left  (currentPos input, [")", "+"])
; S13 input _ -> Left  (currentPos input, [")", "*", "+"])
; S14 input _ -> Left  (currentPos input, [")", "*", "+"])
; S15 input _ -> Left  (currentPos input, [")", "*", "+"])
; S16 input _ -> Left  (currentPos input, [")", "*", "+"])
; S17 input _ -> Left  (currentPos input, ["(", "<num>"])
; S18 input _ -> Left  (currentPos input, ["(", "<num>"])
; S19 input _ -> Left  (currentPos input, ["(", "<num>"])
; S20 input _ -> Left  (currentPos input, [")", "*", "+"])
; S21 input _ -> Left  (currentPos input, [")", "*", "+"])
} where {
; action0 pos res =
{-# LINE  0 "<nowhere>" #-}
res
; action7 pos e f =
{-# LINE  7 "arith.grammar" #-}
                             Add pos e f
; action8 pos f =
{-# LINE  8 "arith.grammar" #-}
                             Factor pos f
; action10 pos f t =
{-# LINE  10 "arith.grammar" #-}
                               Mult pos f t
; action11 pos t =
{-# LINE  11 "arith.grammar" #-}
                               Term pos t
; action13 pos n =
{-# LINE  13 "arith.grammar" #-}
                        Number pos n
; action14 pos e =
{-# LINE  14 "arith.grammar" #-}
                        Group  pos e
}

currentPos :: ([Lexeme], Pos) -> Pos
currentPos = \case
  ([],           end) -> end
  ((pos, _) : _, _)   -> pos

parse :: FilePath -> IO (Either LexerError (Either (Pos, [String]) Expr))
parse filepath = do
  text <- Text.readFile filepath
  case lexText filepath text ["(", ")", "*", "+"] of
    Left  err   -> pure (Left err)
    Right input -> pure (Right (run S0 input Nil))
