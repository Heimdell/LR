{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Backend.Example1 where

import Backend.DefaultLexer
import Text.PrettyPrint.Annotated.HughesPJClass (Pretty(pPrint))
import Debug.Trace
import qualified Data.Text.IO as Text

---- DOMAIN -------------------------------------------------------------------

data Term
  = Number Integer
  | Group  Expr

instance Show Term where
  show = \case
    Number n -> show n
    Group  e -> "(" <> show e <> ")"

data Expr
  = Expr :+ Factor
  | Factor  Factor

instance Show Expr where
  show = \case
    e :+ f   -> show e <> " + " <> show f
    Factor f -> show f

data Factor
  = Factor :* Term
  | Term Term

instance Show Factor where
  show = \case
    f :* t -> show f <> " * " <> show t
    Term t -> show t

---- TOKENS -------------------------------------------------------------------

---- PARSE STACK --------------------------------------------------------------

{- |
  Is either empty or stores (value, prev-state, rest-stack).

  Prev-state and rest-stack are agreed over their signatures.
-}
data Stack' xs where
  Nil  ::                  Stack' '[]
  (:>) :: x -> Stack xs -> Stack' (x : xs)

type Stack a = (St a, Stack' a)

instance Show (Stack' '[]) where
  show Nil = "[]"

pattern (:?) :: x -> Stack xs -> Stack (x : xs)
pattern x :? xs <- (_, x :> xs)

infixr 5 :>, :?

-- instance (Show x, Show (Stack xs)) => Show (Stack' (x : xs)) where
--   show (Push x xs) = "Push(" <> show x <> "," <> show xs <> ")"

{- |
  Extract only head and tails of stack, ignore state.
-}

---- PARSING STATES -----------------------------------------------------------

{-
  0 => S = .E {$}
  1 =>
    S = E . {$}
    E = E .+ F {$ +}
  2 =>
    E = F . {$ +}
    F = F .* Lexeme {$ * +}
  3 =>
    E = E + F . {$ +}
    F = F .* Lexeme {$ * +}
  4 => F = Lexeme . {$ * +}
  5 => Lexeme = number . {$ * +}
  6 => Lexeme = ( .E ) {$ * +}
  7 => E = E + .F {$ +}
  8 => F = F * .Lexeme {$ * +}
  9 =>
    E = E .+ F {) +}
    Lexeme = ( E .) {$ * +}
  10 => F = F * Lexeme . {$ * +}
  11 => Lexeme = ( E ) . {$ * +}
  12 =>
    E = E .+ F {) +}
    Lexeme = ( E .) {) * +}
  13 =>
    E = F . {) +}
    F = F .* Lexeme {) * +}
  14 =>
    E = E + F . {) +}
    F = F .* Lexeme {) * +}
  15 => F = Lexeme . {) * +}
  16 => Lexeme = number . {) * +}
  17 => Lexeme = ( .E ) {) * +}
  18 => E = E + .F {) +}
  19 => F = F * .Lexeme {) * +}
  20 => F = F * Lexeme . {) * +}
  21 => Lexeme = ( E ) . {) * +}
-}
{- |
  The signatures of states denote the longest consumed part of the state.
-}
data St :: [*] -> * where
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

gotoExpr :: [Lexeme] -> Expr -> Stack a -> Expr
gotoExpr toks term stk@(state, _) = case state of
  S0 -> run S1 toks (term :> stk)
  S6 -> run S9 toks (term :> stk)
  S17 -> run S12 toks (term :> stk)
  _ -> error ""

gotoFactor :: [Lexeme] -> Factor -> Stack a -> Expr
gotoFactor toks term stk@(state, _) = case state of
  S0 -> run S2 toks (term :> stk)
  S6 -> run S13 toks (term :> stk)
  S7 -> run S3 toks (term :> stk)
  S17 -> run S13 toks (term :> stk)
  S18 -> run S14 toks (term :> stk)
  _ -> error ""

gotoTerm :: [Lexeme] -> Term -> Stack a -> Expr
gotoTerm toks term stk@(state, _) = case state of
  S0 -> run S4 toks (term :> stk)
  S6 -> run S15 toks (term :> stk)
  S7 -> run S4 toks (term :> stk)
  S8 -> run S10 toks (term :> stk)
  S17 -> run S15 toks (term :> stk)
  S18 -> run S15 toks (term :> stk)
  S19 -> run S20 toks (term :> stk)
  _ -> error ""

run :: St a -> [Lexeme] -> Stack' a -> Expr
run = \cases
  S0 ((a, Reserved "(") : input) stk ->
    run S6 input (() :> (S0, stk))
  S0 ((a, NumberLiteral n) : input) stk ->
    run S5 input (n :> (S0, stk))
  S1 ((a, Reserved "+") : input) stk ->
    run S7 input (() :> (S1, stk))
  S2 ((a, Reserved "*") : input) stk ->
    run S8 input (() :> (S2, stk))
  S3 ((a, Reserved "*") : input) stk ->
    run S8 input (() :> (S3, stk))
  S6 ((a, Reserved "(") : input) stk ->
    run S17 input (() :> (S6, stk))
  S6 ((a, NumberLiteral n) : input) stk ->
    run S16 input (n :> (S6, stk))
  S7 ((a, Reserved "(") : input) stk ->
    run S6 input (() :> (S7, stk))
  S7 ((a, NumberLiteral n) : input) stk ->
    run S5 input (n :> (S7, stk))
  S8 ((a, Reserved "(") : input) stk ->
    run S6 input (() :> (S8, stk))
  S8 ((a, NumberLiteral n) : input) stk ->
    run S5 input (n :> (S8, stk))
  S9 ((a, Reserved ")") : input) stk ->
    run S11 input (() :> (S9, stk))
  S9 ((a, Reserved "+") : input) stk ->
    run S18 input (() :> (S9, stk))
  S12 ((a, Reserved ")") : input) stk ->
    run S21 input (() :> (S12, stk))
  S12 ((a, Reserved "+") : input) stk ->
    run S18 input (() :> (S12, stk))
  S13 ((a, Reserved "*") : input) stk ->
    run S19 input (() :> (S13, stk))
  S14 ((a, Reserved "*") : input) stk ->
    run S19 input (() :> (S14, stk))
  S17 ((a, Reserved "(") : input) stk ->
    run S17 input (() :> (S17, stk))
  S17 ((a, NumberLiteral n) : input) stk ->
    run S16 input (n :> (S17, stk))
  S18 ((a, Reserved "(") : input) stk ->
    run S17 input (() :> (S18, stk))
  S18 ((a, NumberLiteral n) : input) stk ->
    run S16 input (n :> (S18, stk))
  S19 ((a, Reserved "(") : input) stk ->
    run S17 input (() :> (S19, stk))
  S19 ((a, NumberLiteral n) : input) stk ->
    run S16 input (n :> (S19, stk))
  S1 [] (res :> stk) -> res
  S2 [] (factor :> stk) -> gotoExpr [] (Factor factor) stk
  S2 ((a, Reserved "+") : input) (factor :> stk) ->
    gotoExpr ((a, Reserved "+") : input) (Factor factor) stk
  S3 [] (factor :> _ :? expr :? stk) ->
    gotoExpr [] (expr :+ factor) stk
  S3 ((a, Reserved "+") : input) (factor :> _ :? expr :? stk) ->
    gotoExpr ((a, Reserved "+") : input) (expr :+ factor) stk
  S4 [] (term :> stk) -> gotoFactor [] (Term term) stk
  S4 ((a, Reserved "*") : input) (term :> stk) ->
    gotoFactor ((a, Reserved "*") : input) (Term term) stk
  S4 ((a, Reserved "+") : input) (term :> stk) ->
    gotoFactor ((a, Reserved "+") : input) (Term term) stk
  S5 [] (n :> stk) -> gotoTerm [] (Number n) stk
  S5 ((a, Reserved "*") : input) (n :> stk) ->
    gotoTerm ((a, Reserved "*") : input) (Number n) stk
  S5 ((a, Reserved "+") : input) (n :> stk) ->
    gotoTerm ((a, Reserved "+") : input) (Number n) stk
  S10 [] (term :> _ :? factor :? stk) ->
    gotoFactor [] (factor :* term) stk
  S10 ((a, Reserved "*") : input) (term :> _ :? factor :? stk) ->
    gotoFactor ((a, Reserved "*") : input) (factor :* term) stk
  S10 ((a, Reserved "+") : input) (term :> _ :? factor :? stk) ->
    gotoFactor ((a, Reserved "+") : input) (factor :* term) stk
  S11 [] (_ :> expr :? _ :? stk) -> gotoTerm [] (Group expr) stk
  S11 ((a, Reserved "*") : input) (_ :> expr :? _ :? stk) ->
    gotoTerm ((a, Reserved "*") : input) (Group expr) stk
  S11 ((a, Reserved "+") : input) (_ :> expr :? _ :? stk) ->
    gotoTerm ((a, Reserved "+") : input) (Group expr) stk
  S13 ((a, Reserved ")") : input) (factor :> stk) ->
    gotoExpr ((a, Reserved ")") : input) (Factor factor) stk
  S13 ((a, Reserved "+") : input) (factor :> stk) ->
    gotoExpr ((a, Reserved "+") : input) (Factor factor) stk
  S14 ((a, Reserved ")") : input) (factor :> _ :? expr :? stk) ->
    gotoExpr ((a, Reserved ")") : input) (expr :+ factor) stk
  S14 ((a, Reserved "+") : input) (factor :> _ :? expr :? stk) ->
    gotoExpr ((a, Reserved "+") : input) (expr :+ factor) stk
  S15 ((a, Reserved ")") : input) (term :> stk) ->
    gotoFactor ((a, Reserved ")") : input) (Term term) stk
  S15 ((a, Reserved "*") : input) (term :> stk) ->
    gotoFactor ((a, Reserved "*") : input) (Term term) stk
  S15 ((a, Reserved "+") : input) (term :> stk) ->
    gotoFactor ((a, Reserved "+") : input) (Term term) stk
  S16 ((a, Reserved ")") : input) (n :> stk) ->
    gotoTerm ((a, Reserved ")") : input) (Number n) stk
  S16 ((a, Reserved "*") : input) (n :> stk) ->
    gotoTerm ((a, Reserved "*") : input) (Number n) stk
  S16 ((a, Reserved "+") : input) (n :> stk) ->
    gotoTerm ((a, Reserved "+") : input) (Number n) stk
  S20 ((a, Reserved ")") : input) (term :> _ :? factor :? stk) ->
    gotoFactor ((a, Reserved ")") : input) (factor :* term) stk
  S20 ((a, Reserved "*") : input) (term :> _ :? factor :? stk) ->
    gotoFactor ((a, Reserved "*") : input) (factor :* term) stk
  S20 ((a, Reserved "+") : input) (term :> _ :? factor :? stk) ->
    gotoFactor ((a, Reserved "+") : input) (factor :* term) stk
  S21 ((a, Reserved ")") : input) (_ :> expr :? _ :? stk) ->
    gotoTerm ((a, Reserved ")") : input) (Group expr) stk
  S21 ((a, Reserved "*") : input) (_ :> expr :? _ :? stk) ->
    gotoTerm ((a, Reserved "*") : input) (Group expr) stk
  S21 ((a, Reserved "+") : input) (_ :> expr :? _ :? stk) ->
    gotoTerm ((a, Reserved "+") : input) (Group expr) stk

---- TESTING ------------------------------------------------------------------

parse :: FilePath -> IO (Either LexerError Expr)
parse filepath = do
  text <- Text.readFile filepath
  case lexText filepath text (words "+ * ( )") of
    Left err -> pure (Left err)
    Right (input, _) -> pure (Right (run S0 input Nil))
