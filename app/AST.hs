module AST where

import Data.Text qualified as Text
import Data.Text (Text)
import Data.String (IsString (fromString))

data Prog
  = Var Name
  | Lam Name Prog
  | App Prog Prog
  | Con Constant
  | Let Name Prog Prog
  | Ann Prog Type
  deriving stock (Eq, Ord)

instance Show Prog where
  show = \case
    Var n     -> show n
    Lam n p   -> sexp "LAM" [show n, show p]
    App f x   -> sexp ""    [show f, show x]
    Con c     -> show c
    Let x e b -> sexp "LET" [show x, show e, show b]
    Ann p t   -> sexp "HAS-TYPE" [show p, show t]

sexp :: String -> [String] -> String
sexp h t = "(" ++ h ++ " " ++ unwords t ++ ")"

op :: String -> String -> String -> String
op l op' r = "(" ++ unwords [l, op', r] ++ ")"

data Constant
  = Unit
  deriving stock (Eq, Ord)

instance Show Constant where
  show Unit = "UNIT"

data Type
  = TVar Name
  | TArr Type Type
  | TFun Name Type
  | TCon Name
  deriving stock (Eq, Ord)

instance Show Type where
  show = \case
    TArr l r -> op (show l) "~>" (show r)
    TFun n t -> sexp "A" [show n, show t]
    TVar t   -> show t
    TCon t   -> show t

data Name = Name { index :: Int, raw :: Text }
  deriving stock (Eq, Ord)

fromText :: Text -> Name
fromText = Name 0

instance IsString Name where fromString = fromText . fromString
instance Show Name where
  show (Name 0 r) = Text.unpack r
  show (Name n r) = Text.unpack r <> "'" <> show n
