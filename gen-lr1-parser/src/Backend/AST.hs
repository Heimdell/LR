module Backend.AST where

import Data.Text.Position (Pos)

data Term
  = Number Pos Integer
  | Group  Pos Expr

instance Show Term where
  show = \case
    Number _ n -> show n
    Group  _ e -> "(" <> show e <> ")"

data Expr
  = Add    Pos Expr   Factor
  | Factor Pos Factor

instance Show Expr where
  show = \case
    Add    _ e f -> show e <> " + " <> show f
    Factor _ f   -> show f

data Factor
  = Mult Pos Factor Term
  | Term Pos Term

instance Show Factor where
  show = \case
    Mult _ f t -> show f <> " * " <> show t
    Term _ t   -> show t
