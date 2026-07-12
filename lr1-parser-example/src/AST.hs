
module AST where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Position (Pos)
import Data.List (intercalate)

data Const
  = ConstNamed Pos Text
  | ConstInt   Pos Integer

instance Show Const where
  show = \case
    ConstNamed _ name -> Text.unpack name
    ConstInt   _ int  -> show int

data Op
  = Equals
  | Add
  | Mult

instance Show Op where
  show = \case
    Equals -> "="
    Add    -> "+"
    Mult   -> "*"

data Expr
  = ExprBinary Pos Expr Op Expr
  | ExprVar    Pos Text
  | ExprConst  Pos Const

instance Show Expr where
  show = \case
    ExprBinary _ left op right -> "(" <> show left <> " " <> show op <> " " <> show right <> ")"
    ExprVar    _ name          -> Text.unpack name
    ExprConst  _ con           -> show con

data Call = Call
  { pos       :: Pos
  , predicate :: Text
  , args      :: [Expr]
  }

instance Show Call where
  show Call {predicate, args} =
    Text.unpack predicate <> "("
      <> intercalate ", " (map show args)
      <> ")"

data Cond
  = CondAssert Pos Call
  | CondRefute Pos Call
  | CondGuard  Pos Expr

instance Show Cond where
  show = \case
    CondAssert _ c -> show c
    CondRefute _ c -> "~" <> show c
    CondGuard  _ c -> show c

data Clause = Clause
  { pos  :: Pos
  , pat  :: Call
  , body :: [Cond]
  }

instance Show Clause where
  show Clause {pat, body}
    | null body = show pat <> "."
    | otherwise = show pat <> " <- " <> intercalate ", " (map show body) <> "."

data Change
  = Assert Pos Call
  | Refute Pos Call

instance Show Change where
  show = \case
    Assert _ c -> "+" <> show c
    Refute _ c -> "-" <> show c

data Effect = Effect
  { pos  :: Pos
  , pat  :: Call
  , body :: [Cond]
  , changes :: [Change]
  }

instance Show Effect where
  show Effect {pat, body, changes}
    | null body = show pat <> " => " <> intercalate ", " (map show changes) <> "."
    | otherwise = show pat <> " -> " <> intercalate ", " (map show body) <> " => " <> intercalate ", " (map show changes) <> "."

data Stmt
  = StmtClause Pos Clause
  | StmtEffect Pos Effect

instance Show Stmt where
  show = \case
    StmtClause _ c -> show c
    StmtEffect _ e -> show e

data Program = Program {stmts :: [Stmt]}

instance Show Program where
  show Program {stmts} = unlines (map show stmts)

data Test
  = Expect Pos Call
  | Notify Pos Call
  | Guard  Pos Expr

instance Show Test where
  show = \case
    Expect _ c -> "expect " <> show c
    Notify _ c -> "notify " <> show c
    Guard  _ e -> "guard  " <> show e

data TestSuite = TestSuite {tests :: [Test]}

instance Show TestSuite where
  show TestSuite {tests} = unlines (map show tests)
