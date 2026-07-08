{-# LANGUAGE PatternSynonyms #-}

module Backend.Example2 where

---- DOMAIN -------------------------------------------------------------------

data Term
  = Number Int
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

data T
  = TOpen
  | TClose
  | TPlus
  | TMult
  | TNumber Int

instance Show T where
  show = \case
    TOpen      -> "("
    TClose     -> ")"
    TPlus      -> "+"
    TMult      -> "*"
    TNumber {} -> "number"

showToks :: [T] -> String
showToks = \case
  tok : _ -> show tok
  []      -> "<end-of-file>"

---- FILLER VALUES ------------------------------------------------------------

data Open  = Open  deriving stock (Show)
data Close = Close deriving stock (Show)
data Plus  = Plus  deriving stock (Show)
data Mult  = Mult  deriving stock (Show)

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
    F = F .* T {$ * +}
  3 =>
    E = E + F . {$ +}
    F = F .* T {$ * +}
  4 => F = T . {$ * +}
  5 => T = number . {$ * +}
  6 => T = ( .E ) {$ * +}
  7 => E = E + .F {$ +}
  8 => F = F * .T {$ * +}
  9 =>
    E = E .+ F {) +}
    T = ( E .) {$ * +}
  10 => F = F * T . {$ * +}
  11 => T = ( E ) . {$ * +}
  12 =>
    E = E .+ F {) +}
    T = ( E .) {) * +}
  13 =>
    E = F . {) +}
    F = F .* T {) * +}
  14 =>
    E = E + F . {) +}
    F = F .* T {) * +}
  15 => F = T . {) * +}
  16 => T = number . {) * +}
  17 => T = ( .E ) {) * +}
  18 => E = E + .F {) +}
  19 => F = F * .T {) * +}
  20 => F = F * T . {) * +}
  21 => T = ( E ) . {) * +}
-}
{- |
  The signatures of states denote the longest consumed part of the state.
-}
data St a where
  S0  :: forall a. St  a
  S1  :: forall a. St (Expr   : a)
  S2  :: forall a. St (Factor : a)
  S3  :: forall a. St (Factor : Plus  : Expr   : a)
  S4  :: forall a. St (Term   : a)
  S5  :: forall a. St (Int    : a)
  S6  :: forall a. St (Open   : a)
  S7  :: forall a. St (Plus   : Expr   : a)
  S8  :: forall a. St (Mult   : Factor : a)
  S9  :: forall a. St (Expr   : Open   : a)
  S10 :: forall a. St (Term   : Mult   : Factor : a)
  S11 :: forall a. St (Close  : Expr   : Open   : a)
  S12 :: forall a. St (Expr   : Open   : a)
  S13 :: forall a. St (Factor : a)
  S14 :: forall a. St (Factor : Plus   : Expr   : a)
  S15 :: forall a. St (Term   : a)
  S16 :: forall a. St (Int    : a)
  S17 :: forall a. St (Open   : a)
  S18 :: forall a. St (Plus   : Expr   : a)
  S19 :: forall a. St (Mult   : Factor : a)
  S20 :: forall a. St (Term   : Mult   : Factor : a)
  S21 :: forall a. St (Close  : Expr   : Open   : a)

deriving stock instance Show (St a)
{-
---- TABLES -------------------------------------------------------------------

{-
  0 =>
    E      GOTO  1
    F      GOTO  2
    T      GOTO  4

    (      Shift 6
    number Shift 5

  1 =>
    $ A-C-C-E-P-T
    + Shift 7

  2 =>
    $ Reduce E = F
    * Shift 8
    + Reduce E = F

  3 =>
    $ Reduce E = E + F
    * Shift 8
    + Reduce E = E + F

  4 =>
    $ Reduce F = T
    * Reduce F = T
    + Reduce F = T

  5 =>
    $ Reduce T = number
    * Reduce T = number
    + Reduce T = number

  6 =>
    E      GOTO  9
    F      GOTO  13
    T      GOTO  15
    (      Shift 17
    number Shift 16

  7 =>
    F      GOTO  3
    T      GOTO  4
    (      Shift 6
    number Shift 5

  8 =>
    T      GOTO  10
    (      Shift 6
    number Shift 5

  9 =>
    ) Shift 11
    + Shift 18

  10 =>
    $ Reduce F = F * T
    * Reduce F = F * T
    + Reduce F = F * T

  11 =>
    $ Reduce T = ( E )
    * Reduce T = ( E )
    + Reduce T = ( E )

  12 =>
    ) Shift 21
    + Shift 18

  13 =>
    ) Reduce E = F
    * Shift 19
    + Reduce E = F

  14 =>
    ) Reduce E = E + F
    * Shift 19
    + Reduce E = E + F

  15 =>
    ) Reduce F = T
    * Reduce F = T
    + Reduce F = T

  16 =>
    ) Reduce T = number
    * Reduce T = number
    + Reduce T = number

  17 =>
    E      GOTO  12
    F      GOTO  13
    T      GOTO  15
    (      Shift 17
    number Shift 16

  18 =>
    F      GOTO  14
    T      GOTO  15
    (      Shift 17
    number Shift 16

  19 =>
    T      GOTO  20
    (      Shift 17
    number Shift 16

  20 =>
    ) Reduce F = F * T
    * Reduce F = F * T
    + Reduce F = F * T

  21 =>
    ) Reduce T = ( E )
    * Reduce T = ( E )
    + Reduce T = ( E )
-}
-}

{- |
  GOTO table for entity Expr.
-}
gotoExpr :: [T] -> Expr -> Stack a -> Expr
gotoExpr toks expr stk@(state, _) = case state of
  S0  -> run (S1 , expr :> stk) toks
  S6  -> run (S9 , expr :> stk) toks
  S17 -> run (S12, expr :> stk) toks
  _   -> error ""

{- |
  GOTO table for entity Factor.
-}
gotoFactor :: [T] -> Factor -> Stack a -> Expr
gotoFactor toks factor stk@(state, _) = case state of
  S0  -> run (S2 , factor :> stk) toks
  S6  -> run (S13, factor :> stk) toks
  S7  -> run (S3 , factor :> stk) toks
  S17 -> run (S13, factor :> stk) toks
  S18 -> run (S14, factor :> stk) toks
  _   -> error ""

{- |
  GOTO table for entity Term.
-}
gotoTerm :: [T] -> Term -> Stack a -> Expr
gotoTerm toks term stk@(state, _) = case state of
  S0  -> run (S4 , term :> stk) toks
  S6  -> run (S15, term :> stk) toks
  S7  -> run (S4 , term :> stk) toks
  S8  -> run (S10, term :> stk) toks
  S17 -> run (S15, term :> stk) toks
  S18 -> run (S15, term :> stk) toks
  S19 -> run (S20, term :> stk) toks
  _   -> error ""

{- |
  Parsing procedure, contains table ACTION.
-}
run :: Stack a -> [T] -> Expr
run = \cases
  ---- SHIFT/REDUCE -----------------------------------------------------------

  (S1, e :> _) [] -> e

  {-
    Shift actions. They consume token and push its value onto stack with new state
    (s, x, xs) -> (s1, value, (s, x, xs))
  -}
  stk@(S0 , _) (TOpen     : toks) -> run (S6 , Open  :> stk) toks
  stk@(S0 , _) (TNumber n : toks) -> run (S5 , n     :> stk) toks
  stk@(S1 , _) (TPlus     : toks) -> run (S7 , Plus  :> stk) toks
  stk@(S2 , _) (TMult     : toks) -> run (S8 , Mult  :> stk) toks
  stk@(S3 , _) (TMult     : toks) -> run (S8 , Mult  :> stk) toks
  stk@(S6 , _) (TOpen     : toks) -> run (S17, Open  :> stk) toks
  stk@(S6 , _) (TNumber n : toks) -> run (S16, n     :> stk) toks
  stk@(S7 , _) (TOpen     : toks) -> run (S6 , Open  :> stk) toks
  stk@(S7 , _) (TNumber n : toks) -> run (S5 , n     :> stk) toks
  stk@(S8 , _) (TOpen     : toks) -> run (S6 , Open  :> stk) toks
  stk@(S8 , _) (TNumber n : toks) -> run (S5 , n     :> stk) toks
  stk@(S9 , _) (TPlus     : toks) -> run (S18, Plus  :> stk) toks
  stk@(S9 , _) (TClose    : toks) -> run (S11, Close :> stk) toks
  stk@(S12, _) (TPlus     : toks) -> run (S18, Plus  :> stk) toks
  stk@(S12, _) (TClose    : toks) -> run (S21, Close :> stk) toks
  stk@(S13, _) (TMult     : toks) -> run (S19, Mult  :> stk) toks
  stk@(S14, _) (TMult     : toks) -> run (S19, Mult  :> stk) toks
  stk@(S17, _) (TNumber n : toks) -> run (S16, n     :> stk) toks
  stk@(S17, _) (TOpen     : toks) -> run (S17, Open  :> stk) toks
  stk@(S18, _) (TNumber n : toks) -> run (S16, n     :> stk) toks
  stk@(S18, _) (TOpen     : toks) -> run (S17, Open  :> stk) toks
  stk@(S19, _) (TNumber n : toks) -> run (S16, n     :> stk) toks
  stk@(S19, _) (TOpen     : toks) -> run (S17, Open  :> stk) toks

  {-
    Reduce actions. They grab some information from stack and apply some reducer
    (_, f, (_, Plus, (_, e, (s, _, _)))) -> (s1, e :+ f, (s, _, _))
    where s1 is determined by GOTO table from state of s.
  -}
  (S2 ,                  f    :> stk) []                 -> gotoExpr    []              (Factor f) stk
  (S2 ,                  f    :> stk) (TPlus     : toks) -> gotoExpr    (TPlus  : toks) (Factor f) stk
  (S3 , f     :> Plus :? e    :? stk) []                 -> gotoExpr    []              (e :+ f)   stk
  (S3 , f     :> Plus :? e    :? stk) (TPlus     : toks) -> gotoExpr    toks            (e :+ f)   stk
  (S4 ,                  t    :> stk) []                 -> gotoFactor  []              (Term t)   stk
  (S4 ,                  t    :> stk) (TPlus     : toks) -> gotoFactor  (TPlus  : toks) (Term t)   stk
  (S4 ,                  t    :> stk) (TMult     : toks) -> gotoFactor  (TMult  : toks) (Term t)   stk
  (S5 ,                  i    :> stk) []                 -> gotoTerm    []              (Number i) stk
  (S5 ,                  i    :> stk) (TPlus     : toks) -> gotoTerm    (TPlus  : toks) (Number i) stk
  (S5 ,                  i    :> stk) (TMult     : toks) -> gotoTerm    (TMult  : toks) (Number i) stk
  (S10, t     :> Mult :? f    :? stk) []                 -> gotoFactor  []              (f :* t)   stk
  (S10, t     :> Mult :? f    :? stk) (TPlus     : toks) -> gotoFactor  (TPlus  : toks) (f :* t)   stk
  (S10, t     :> Mult :? f    :? stk) (TMult     : toks) -> gotoFactor  (TMult  : toks) (f :* t)   stk
  (S11, Close :> e    :? Open :? stk) []                 -> gotoTerm    []              (Group e)  stk
  (S11, Close :> e    :? Open :? stk) (TPlus     : toks) -> gotoTerm    (TPlus  : toks) (Group e)  stk
  (S11, Close :> e    :? Open :? stk) (TMult     : toks) -> gotoTerm    (TMult  : toks) (Group e)  stk
  (S13,                  f    :> stk) (TClose    : toks) -> gotoExpr    (TClose : toks) (Factor f) stk
  (S13,                  f    :> stk) (TPlus     : toks) -> gotoExpr    (TPlus  : toks) (Factor f) stk
  (S14, f     :> Plus :? e    :? stk) (TClose    : toks) -> gotoExpr    (TClose : toks) (e :+ f)   stk
  (S14, f     :> Plus :? e    :? stk) (TPlus     : toks) -> gotoExpr    (TPlus  : toks) (e :+ f)   stk
  (S15,                  t    :> stk) (TClose    : toks) -> gotoFactor  (TClose : toks) (Term t)   stk
  (S15,                  t    :> stk) (TPlus     : toks) -> gotoFactor  (TPlus  : toks) (Term t)   stk
  (S15,                  t    :> stk) (TMult     : toks) -> gotoFactor  (TMult  : toks) (Term t)   stk
  (S16,                  i    :> stk) (TClose    : toks) -> gotoTerm    (TClose : toks) (Number i) stk
  (S16,                  i    :> stk) (TPlus     : toks) -> gotoTerm    (TPlus  : toks) (Number i) stk
  (S16,                  i    :> stk) (TMult     : toks) -> gotoTerm    (TMult  : toks) (Number i) stk
  (S20, t     :> Mult :? f    :? stk) (TClose    : toks) -> gotoFactor  (TClose : toks) (f :* t)   stk
  (S20, t     :> Mult :? f    :? stk) (TPlus     : toks) -> gotoFactor  (TPlus  : toks) (f :* t)   stk
  (S20, t     :> Mult :? f    :? stk) (TMult     : toks) -> gotoFactor  (TMult  : toks) (f :* t)   stk
  (S21, Close :> e    :? Open :? stk) (TClose    : toks) -> gotoTerm    (TClose : toks) (Group e)  stk
  (S21, Close :> e    :? Open :? stk) (TPlus     : toks) -> gotoTerm    (TPlus  : toks) (Group e)  stk
  (S21, Close :> e    :? Open :? stk) (TMult     : toks) -> gotoTerm    (TMult  : toks) (Group e)  stk

  (S0 , _) toks -> error $ show S0  <> " expected '(' or number, got " <> showToks toks
  (S1 , _) toks -> error $ show S1  <> " expected <end-of-file> or '+', got " <> showToks toks
  (S2 , _) toks -> error $ show S2  <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
  (S3 , _) toks -> error $ show S3  <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
  (S4 , _) toks -> error $ show S4  <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
  (S5 , _) toks -> error $ show S5  <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
  (S6 , _) toks -> error $ show S6  <> " expected '(' or number, got " <> showToks toks
  (S7 , _) toks -> error $ show S7  <> " expected '(' or number, got " <> showToks toks
  (S8 , _) toks -> error $ show S8  <> " expected '(' or number, got " <> showToks toks
  (S9 , _) toks -> error $ show S9  <> " expected <end-of-file> or '+', got " <> showToks toks
  (S10, _) toks -> error $ show S10 <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
  (S11, _) toks -> error $ show S11 <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
  (S12, _) toks -> error $ show S12 <> " expected ')' or '+', got " <> showToks toks
  (S13, _) toks -> error $ show S13 <> " expected ')', '*' or '+', got " <> showToks toks
  (S14, _) toks -> error $ show S14 <> " expected ')', '*' or '+', got " <> showToks toks
  (S15, _) toks -> error $ show S15 <> " expected ')', '*' or '+', got " <> showToks toks
  (S16, _) toks -> error $ show S16 <> " expected ')', '*' or '+', got " <> showToks toks
  (S17, _) toks -> error $ show S17 <> " expected '(' or number, got " <> showToks toks
  (S18, _) toks -> error $ show S18 <> " expected '(' or number, got " <> showToks toks
  (S19, _) toks -> error $ show S19 <> " expected '(' or number, got " <> showToks toks
  (S20, _) toks -> error $ show S20 <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
  (S21, _) toks -> error $ show S21 <> " expected <end-of-file>, '*' or '+', got " <> showToks toks

---- TESTING ------------------------------------------------------------------

example :: Expr
example = run (S0, Nil) [TNumber 1, TMult, TOpen, TNumber 2, TPlus, TNumber 4, TClose, TMult, TNumber 3]
