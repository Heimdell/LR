module AST where

import Data.Text qualified as Text
import Data.Text (Text)
import Data.Scientific

data Module = Module QName [Import] [Toplevel]
  deriving stock (Show, Eq, Ord)

data Import = Import
  { from   :: [Name]
  , rename :: Maybe [Name]
  , names  :: Maybe [Name]
  }
  deriving stock (Show, Eq, Ord)

data Toplevel
  = TopDecl Private Decl
  | NewType Private NewType
  deriving stock (Show, Eq, Ord)

data Private = Private | Public
  deriving stock (Show, Eq, Ord)

data NewType
  = Opaque Name [TArg] [Variant]
  deriving stock (Show, Eq, Ord)

data Variant
  = Variant Ctor [TField]
  deriving stock (Show, Eq, Ord)

data TField = TField Field Type
  deriving stock (Show, Eq, Ord)

data TArg = TArg Name (Maybe Kind)
  deriving stock (Show, Eq, Ord)

data Kind = Star | KArr Kind Kind
  deriving stock (Show, Eq, Ord)

data Prog
  = Var QName
  | App Prog Prog
  | Lam [Arg] Prog

  | Ann Prog Type

  | Get Prog Field
  | Upd Prog [RDecl]

  | Inj  Ctor [RDecl]
  | Mtc  Prog [Alt]

  | Let [Decl] Prog
  | List [ListElem]
  | Con Constant
  deriving stock (Show, Eq, Ord)

data ListElem
  = Elem   Prog
  | Spread Prog
  deriving stock (Show, Eq, Ord)

data RDecl
  = Decl Decl
  | Capt Name
  deriving stock (Show, Eq, Ord)

data Decl
  = Val Name (Maybe Type) Prog
  deriving stock (Show, Eq, Ord)

data Type
  = TCon QName
  | TVar Name
  | TApp Type Type
  | TArr Type Type
  deriving stock (Show, Eq, Ord)

data QName = QName [Name] Name
  deriving stock (Show, Eq, Ord)

data Name = Name Text Int
  deriving stock (Eq, Ord)

fromText :: Text -> Name
fromText = (`Name` 0)

instance Show Name where
  show (Name x 0) = Text.unpack x
  show (Name x i) = Text.unpack x <> "'" <> show i

data Ctor  = Ctor  QName deriving stock (Show, Eq, Ord)
data Field = Field Name  deriving stock (Show, Eq, Ord)

data Arg = Arg Name (Maybe Type) | Fix Name (Maybe Type)
  deriving stock (Show, Eq, Ord)

data Alt = Alt Pat (Maybe Prog) Prog
  deriving stock (Show, Eq, Ord)

data Pat
  = PVar Name
  | PPrj Ctor [PDecl]
  | PList [PListElem]
  | PCon Constant
  deriving stock (Show, Eq, Ord)

data PListElem
  = PElem Pat
  | PSpread Pat
  deriving stock (Show, Eq, Ord)

data PDecl
  = PDecl Name Pat
  | PCapt Name
  deriving stock (Show, Eq, Ord)

data Constant
  = Number Scientific
  | String Text
  deriving stock (Show, Eq, Ord)

data QType
  = Forall [Name] Type
  deriving stock (Show, Eq, Ord)
