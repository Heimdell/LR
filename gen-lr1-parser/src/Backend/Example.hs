{-# LANGUAGE PatternSynonyms #-}

module Backend.Example where

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
data Stack xs where
  Nil  ::               Stack '[]
  (:>) :: (St xs, x) -> Stack xs -> Stack (x : xs)

instance Show (Stack '[]) where
  show Nil = "[]"

instance (Show x, Show (Stack xs)) => Show (Stack (x : xs)) where
  show ((st, x) :> xs) = "(" <> show st <> ", " <> show x <> "," <> show xs <> ")"

{- |
  Extract only head and tails of stack, ignore state.
-}
pattern (:?) :: x -> Stack xs -> Stack (x : xs)
pattern x :? xs <- (_, x) :> xs

infixr 2 :>, :?

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
  S0  :: St  a
  S1  :: St (Expr   : a)
  S2  :: St (Factor : a)
  S3  :: St (Factor : Plus  : Expr   : a)
  S4  :: St (Term   : a)
  S5  :: St (Int    : a)
  S6  :: St (Open   : a)
  S7  :: St (Plus   : Expr   : a)
  S8  :: St (Mult   : Factor : a)
  S9  :: St (Expr   : Open  : a)
  S10 :: St (Term   : Mult   : Factor : a)
  S11 :: St (Close  : Expr   : Open   : a)
  S12 :: St (Expr   : Open   : a)
  S13 :: St (Factor : a)
  S14 :: St (Factor : Plus   : Expr   : a)
  S15 :: St (Term   : a)
  S16 :: St (Int    : a)
  S17 :: St (Open   : a)
  S18 :: St (Plus   : Expr   : a)
  S19 :: St (Mult   : Factor : a)
  S20 :: St (Term   : Mult   : Factor : a)
  S21 :: St (Close  : Expr   : Open   : a)

deriving stock instance Show (St a)

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

{- |
  GOTO table for entity Expr.
-}
gotoExpr :: St a -> Expr -> Stack a -> [T] -> Expr
gotoExpr = \cases
  S0  e stk -> run S1  ((S0,  e) :> stk)
  S6  e stk -> run S9  ((S6,  e) :> stk)
  S17 e stk -> run S12 ((S17, e) :> stk)
  _   _ _   -> error ""

{- |
  GOTO table for entity Factor.
-}
gotoFactor :: St a -> Factor -> Stack a -> [T] -> Expr
gotoFactor = \cases
  S0  f stk -> run S2  ((S0,  f) :> stk)
  S6  f stk -> run S13 ((S6,  f) :> stk)
  S7  f stk -> run S3  ((S7,  f) :> stk)
  S17 f stk -> run S13 ((S17, f) :> stk)
  S18 f stk -> run S14 ((S18, f) :> stk)
  _   _ _   -> error ""

{- |
  GOTO table for entity Term.
-}
gotoTerm :: St a -> Term -> Stack a -> [T] -> Expr
gotoTerm = \cases
  S0  t stk -> run S4  ((S0,  t) :> stk)
  S6  t stk -> run S15 ((S6,  t) :> stk)
  S7  t stk -> run S4  ((S7,  t) :> stk)
  S8  t stk -> run S10 ((S8,  t) :> stk)
  S17 t stk -> run S15 ((S17, t) :> stk)
  S18 t stk -> run S15 ((S18, t) :> stk)
  S19 t stk -> run S20 ((S19, t) :> stk)
  _ _ _ -> error ""

{- |
  Parsing procedure, contains table ACTION.
-}
run :: St a -> Stack a -> [T] -> Expr
run = \cases

  ---- SHIFT/REDUCE -----------------------------------------------------------

  S1 (ex :? _) [] -> ex

  S0  stk (TOpen     : toks) -> run S6  ((S0,  Open)  :> stk) toks
  S0  stk (TNumber n : toks) -> run S5  ((S0,  n)     :> stk) toks
  S1  stk (TPlus     : toks) -> run S7  ((S1,  Plus)  :> stk) toks
  S2  stk (TMult     : toks) -> run S8  ((S2,  Mult)  :> stk) toks
  S3  stk (TMult     : toks) -> run S8  ((S2,  Mult)  :> stk) toks
  S6  stk (TOpen     : toks) -> run S17 ((S6,  Open)  :> stk) toks
  S6  stk (TNumber n : toks) -> run S16 ((S6,  n)     :> stk) toks
  S7  stk (TOpen     : toks) -> run S6  ((S7,  Open)  :> stk) toks
  S7  stk (TNumber n : toks) -> run S5  ((S7,  n)     :> stk) toks
  S8  stk (TOpen     : toks) -> run S6  ((S8,  Open)  :> stk) toks
  S8  stk (TNumber n : toks) -> run S5  ((S8,  n)     :> stk) toks
  S9  stk (TPlus     : toks) -> run S18 ((S9,  Plus)  :> stk) toks
  S9  stk (TClose    : toks) -> run S11 ((S9,  Close) :> stk) toks
  S12 stk (TPlus     : toks) -> run S18 ((S12, Plus)  :> stk) toks
  S12 stk (TClose    : toks) -> run S21 ((S12, Close) :> stk) toks
  S13 stk (TMult     : toks) -> run S19 ((S13, Mult)  :> stk) toks
  S14 stk (TMult     : toks) -> run S19 ((S14, Mult)  :> stk) toks
  S17 stk (TNumber n : toks) -> run S16 ((S17, n)     :> stk) toks
  S17 stk (TOpen     : toks) -> run S17 ((S17, Open)  :> stk) toks
  S18 stk (TNumber n : toks) -> run S16 ((S18, n)     :> stk) toks
  S18 stk (TOpen     : toks) -> run S17 ((S18, Open)  :> stk) toks
  S19 stk (TNumber n : toks) -> run S16 ((S19, n)     :> stk) toks
  S19 stk (TOpen     : toks) -> run S17 ((S19, Open)  :> stk) toks

  S2  (                 (s, f)    :> stk) []                 -> gotoExpr   s (Factor f) stk []
  S2  (                 (s, f)    :> stk) (TPlus     : toks) -> gotoExpr   s (Factor f) stk (TPlus  : toks)
  S3  (f     :? Plus :? (s, e)    :> stk) []                 -> gotoExpr   s (e :+ f)   stk []
  S3  (f     :? Plus :? (s, e)    :> stk) (TPlus     : toks) -> gotoExpr   s (e :+ f)   stk (TPlus  : toks)
  S4  (                 (s, t)    :> stk) []                 -> gotoFactor s (Term t)   stk []
  S4  (                 (s, t)    :> stk) (TPlus     : toks) -> gotoFactor s (Term t)   stk (TPlus  : toks)
  S4  (                 (s, t)    :> stk) (TMult     : toks) -> gotoFactor s (Term t)   stk (TMult  : toks)
  S5  (                 (s, i)    :> stk) []                 -> gotoTerm   s (Number i) stk []
  S5  (                 (s, i)    :> stk) (TPlus     : toks) -> gotoTerm   s (Number i) stk (TPlus  : toks)
  S5  (                 (s, i)    :> stk) (TMult     : toks) -> gotoTerm   s (Number i) stk (TMult  : toks)
  S10 (t     :? Mult :? (s, f)    :> stk) []                 -> gotoFactor s (f :* t)   stk []
  S10 (t     :? Mult :? (s, f)    :> stk) (TPlus     : toks) -> gotoFactor s (f :* t)   stk (TPlus  : toks)
  S10 (t     :? Mult :? (s, f)    :> stk) (TMult     : toks) -> gotoFactor s (f :* t)   stk (TMult  : toks)
  S11 (Close :? e    :? (s, Open) :> stk) []                 -> gotoTerm   s (Group e)  stk []
  S11 (Close :? e    :? (s, Open) :> stk) (TPlus     : toks) -> gotoTerm   s (Group e)  stk (TPlus  : toks)
  S11 (Close :? e    :? (s, Open) :> stk) (TMult     : toks) -> gotoTerm   s (Group e)  stk (TMult  : toks)
  S13 (                 (s, f)    :> stk) (TClose    : toks) -> gotoExpr   s (Factor f) stk (TClose : toks)
  S13 (                 (s, f)    :> stk) (TPlus     : toks) -> gotoExpr   s (Factor f) stk (TPlus  : toks)
  S14 (    f :? Plus :? (s, e)    :> stk) (TClose    : toks) -> gotoExpr   s (e :+ f)   stk (TClose : toks)
  S14 (    f :? Plus :? (s, e)    :> stk) (TPlus     : toks) -> gotoExpr   s (e :+ f)   stk (TPlus  : toks)
  S15 (                 (s, t)    :> stk) (TClose    : toks) -> gotoFactor s (Term t)   stk (TClose : toks)
  S15 (                 (s, t)    :> stk) (TPlus     : toks) -> gotoFactor s (Term t)   stk (TPlus  : toks)
  S15 (                 (s, t)    :> stk) (TMult     : toks) -> gotoFactor s (Term t)   stk (TMult  : toks)
  S16 (                 (s, i)    :> stk) (TClose    : toks) -> gotoTerm   s (Number i) stk (TClose : toks)
  S16 (                 (s, i)    :> stk) (TPlus     : toks) -> gotoTerm   s (Number i) stk (TPlus  : toks)
  S16 (                 (s, i)    :> stk) (TMult     : toks) -> gotoTerm   s (Number i) stk (TMult  : toks)
  S20 (t     :? Mult :? (s, f)    :> stk) (TClose    : toks) -> gotoFactor s (f :* t)   stk (TClose : toks)
  S20 (t     :? Mult :? (s, f)    :> stk) (TPlus     : toks) -> gotoFactor s (f :* t)   stk (TPlus  : toks)
  S20 (t     :? Mult :? (s, f)    :> stk) (TMult     : toks) -> gotoFactor s (f :* t)   stk (TMult  : toks)
  S21 (Close :? e    :? (s, Open) :> stk) (TClose    : toks) -> gotoTerm   s (Group e)  stk (TClose : toks)
  S21 (Close :? e    :? (s, Open) :> stk) (TPlus     : toks) -> gotoTerm   s (Group e)  stk (TPlus  : toks)
  S21 (Close :? e    :? (s, Open) :> stk) (TMult     : toks) -> gotoTerm   s (Group e)  stk (TMult  : toks)

  ---- ERROR REPORTS ----------------------------------------------------------

  S0  _ toks -> error $ show S0  <> " expected '(' or number, got " <> showToks toks
  S1  _ toks -> error $ show S1  <> " expected <end-of-file> or '+', got " <> showToks toks
  S2  _ toks -> error $ show S2  <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
  S3  _ toks -> error $ show S3  <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
  S4  _ toks -> error $ show S4  <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
  S5  _ toks -> error $ show S5  <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
  S6  _ toks -> error $ show S6  <> " expected '(' or number, got " <> showToks toks
  S7  _ toks -> error $ show S7  <> " expected '(' or number, got " <> showToks toks
  S8  _ toks -> error $ show S8  <> " expected '(' or number, got " <> showToks toks
  S9  _ toks -> error $ show S9  <> " expected <end-of-file> or '+', got " <> showToks toks
  S10 _ toks -> error $ show S10 <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
  S11 _ toks -> error $ show S11 <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
  S12 _ toks -> error $ show S12 <> " expected ')' or '+', got " <> showToks toks
  S13 _ toks -> error $ show S13 <> " expected ')', '*' or '+', got " <> showToks toks
  S14 _ toks -> error $ show S14 <> " expected ')', '*' or '+', got " <> showToks toks
  S15 _ toks -> error $ show S15 <> " expected ')', '*' or '+', got " <> showToks toks
  S16 _ toks -> error $ show S16 <> " expected ')', '*' or '+', got " <> showToks toks
  S17 _ toks -> error $ show S17 <> " expected '(' or number, got " <> showToks toks
  S18 _ toks -> error $ show S18 <> " expected '(' or number, got " <> showToks toks
  S19 _ toks -> error $ show S19 <> " expected '(' or number, got " <> showToks toks
  S20 _ toks -> error $ show S20 <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
  S21 _ toks -> error $ show S21 <> " expected <end-of-file>, '*' or '+', got " <> showToks toks

---- TESTING ------------------------------------------------------------------

example :: Expr
example = run S0 Nil [TNumber 1, TMult, TOpen, TNumber 2, TPlus, TNumber 4, TClose, TMult, TNumber 3]
