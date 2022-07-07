
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-star-is-type #-}

module E where

import Control.Applicative hiding (some)
import Data.Monoid (First (..))
import Data.Char
import Data.Scientific
import Data.Text qualified as Text
import Data.Text (Text)
import Data.Typeable
import Prettyprinter
import Text.Megaparsec.Char.Lexer qualified as L

import Text.Brick.DSL
import Text.Brick.Lexer qualified as Lexer
import Text.Brick.Lexer (SourcePos, Parser, runLexer, stringLit, asToken, nameLike, kw)

import AST

import Debug.Trace

arith :: M (Entity Lexeme SourcePos Module)
-- arith :: M (Entity Lexeme SourcePos QuailifiedName)
arith = mdo

  module_ <- rule "Module"
    [ Pos !. "module" !* qCtor !. "where" !* imports !* toplevels :=> \_ _ p _ i t -> Module p i  t
    , Pos !. "module" !* qCtor !. "where" !* toplevels            :=> \_ _ p _   t -> Module p [] t
    , Pos !. "module" !* qCtor !. "where" !* imports              :=> \_ _ p _ i   -> Module p i  []
    , Pos !. "module" !* qCtor !. "where"                         :=> \_ _ p _     -> Module p [] []
    ]

  imports   <- some import_
  toplevels <- some toplevel

  import_ <- rule "Import"
    [ Pos !. "use" !* path !. "as" !* path !. "{" !* names !. "}" :=> \_ _ a _ b _ c _ -> Import a (Just b) (Just c)
    , Pos !. "use" !* path !. "as" !* path                        :=> \_ _ a _ b       -> Import a (Just b)  Nothing
    , Pos !. "use" !* path                 !. "{" !* names !. "}" :=> \_ _ a     _ c _ -> Import a  Nothing (Just c)
    , Pos !. "use" !* path                                        :=> \_ _ a           -> Import a  Nothing  Nothing
    ]

  path  <- ctor       `sepBy` dot
  names <- nameOrCtor `sepBy` sep

  nameOrCtor <- rule @Name "NameOrCtor"
    [ Pos !* name :=> pass
    , Pos !* ctor :=> pass
    ]

  toplevel <- rule "Toplevel"
    [ Pos !. "local" !. "def" !* decl' :=> \_ _ _ d -> TopDecl Private d
    , Pos            !. "def" !* decl' :=> \_   _ d -> TopDecl Public  d
    , Pos !. "local" !* newType        :=> \_ _ t   -> NewType Private t
    , Pos            !* newType        :=> \_   t   -> NewType Public  t
    ]

  prog <- rule "Prog" [ Pos !* letExpr :=> pass ]

  letExpr <- rule "LetExpr"
    [ Pos !. "let" !* decls !. "in" !* letExpr :=> \_ _ ds _ e -> Let ds e
    , Pos !* app                               :=> pass
    ]

  decls <- decl `sepBy` sep

  decl <- rule "Decl"
    [ Pos !* name !. ":" !* type_ !. "=" !* prog :=> \_ n _ t _ b -> Val n (Just t) b
    , Pos !* name                 !. "=" !* prog :=> \_ n     _ b -> Val n  Nothing b
    ]

  decl' <- rule "Decl"
    [ Pos !* name !. ":" !* type_ !. "=" !* prog :=> \_ n _ t _ b -> Val n (Just t) b
    ]

  app <- rule "App"
    [ Pos !* app !* term :=> \_ f x -> App f x
    , Pos !* term        :=> pass
    ]

  term <- rule "Term"
    [ Pos !. "(" !* prog !. ")"                     :=> \_ _ p _     -> p
    , Pos !. "(" !* prog !. ":" !* type_ !. ")"     :=> \_ _ p _ t _ -> Ann p t
    , Pos !* qName                                  :=> \_ n         -> Var n
    , Pos !. "\\" !* args !. "=>" !* prog           :=> \_ _ as _ b  -> Lam as b
    , Pos !* aCtor                                  :=> \_ c         -> Inj c []
    , Pos !* aCtor !. "{"           !. "}"          :=> \_ c _ _     -> Inj c []
    , Pos !* aCtor !. "{" !* rdecls !. "}"          :=> \_ c _ rd _  -> Inj c rd
    , Pos !* term !. "." !* field                   :=> \_ t _ f     -> Get t f
    , Pos !* term !. "with" !. "{" !* rdecls !. "}" :=> \_ t _ _ f _ -> Upd t f
    , Pos !* constant                               :=> \_ c         -> Con c
    , Pos !. "case" !* prog !. "of" !. "end"        :=> \_ _ p _ _   -> Mtc p []
    , Pos !. "case" !* prog !. "of" !* alts !. "end":=> \_ _ p _ as _-> Mtc p as
    , Pos !. "[" !. "]"                             :=> \_ _ _       -> List []
    , Pos !. "[" !* elems !. "]"                    :=> \_ _ es _    -> List es
    ]

  stmts <- stmt `sepBy` semi

  stmt <- rule "Stmt"
    [ Pos !* name !.                 "="  !* prog :=> \_ n     _ p -> SLet  n  Nothing p
    , Pos !* name !. ":" !* type_ !. "="  !* prog :=> \_ n _ t _ p -> SLet  n (Just t) p
    , Pos !* name !.                 "<-" !* prog :=> \_ n     _ p -> SBind n  Nothing p
    , Pos !* name !. ":" !* type_ !. "<-" !* prog :=> \_ n _ t _ p -> SBind n (Just t) p
    , Pos !* prog                                 :=> \_ p         -> SInvoke          p
    ]

  elems <- lelem `sepBy` sep

  lelem <- rule "LElem"
    [ Pos !. "..." !* prog :=> \_ _ p -> Spread p
    , Pos          !* prog :=> \_   p -> Elem p
    ]

  alts <- some alt

  alt <- rule "Alt"
    [ Pos !. "|" !* pat !. "when" !* prog !. "=>" !* prog :=> \_ _ p _ c _ b -> Alt p (Just c) b
    , Pos !. "|" !* pat                   !. "=>" !* prog :=> \_ _ p     _ b -> Alt p  Nothing b
    ]

  args <- arg `sepBy` sep

  arg <- rule "Arg"
    [ Pos !* name                 :=> \_ n     -> Arg n Nothing
    , Pos !* name !. ":" !* type_ :=> \_ n _ t -> Arg n (Just t)
    , Pos !. "@" !* name          :=> \_ _ n   -> Fix n Nothing
    , Pos !. "@" !* name !. ":" !* type_ :=> \_ _ n _ t  -> Fix n (Just t)
    ]

  rdecls <- rdecl `sepBy` sep

  rdecl <- rule "RDecl"
    [ Pos !* name                :=> \_ n     -> Capt n
    , Pos !* name !. "=" !* prog :=> \_ n _ p -> Decl (Val n Nothing p)
    ]

  pat <- rule "Pat"
    [ Pos !* aCtor                         :=> \_ c        -> PPrj c []
    , Pos !* aCtor !. "{"           !. "}" :=> \_ c _    _ -> PPrj c []
    , Pos !* aCtor !. "{" !* pdecls !. "}" :=> \_ c _ ds _ -> PPrj c ds
    , Pos !* constant                      :=> \_ c        -> PCon c
    , Pos !* name                          :=> \_ n        -> PVar n
    , Pos !. "[" !. "]"                    :=> \_ _ _       -> PList []
    , Pos !. "[" !* pelems !. "]"          :=> \_ _ es _    -> PList es
    ]

  pelems <- pelem `sepBy` sep

  pelem <- rule "PElem"
    [ Pos !. "..." !* pat :=> \_ _ p -> PSpread p
    , Pos          !* pat :=> \_   p -> PElem p
    ]

  pdecls <- pdecl `sepBy` sep

  pdecl <- rule "PDecl"
    [ Pos !* name               :=> \_ n     -> PCapt n
    , Pos !* name !. "=" !* pat :=> \_ n _ p -> PDecl n p
    ]

  constant <- rule "Constant"
    [ Pos !. "number" :=> \_ (LNumber s) -> Number s
    , Pos !. "string" :=> \_ (LString s) -> String s
    ]

  newType <- rule "NewType"
    [ Pos !. "type" !* ctor !* targs !. "=" !* variants :=> \_ _ c ns _ vs -> Opaque c ns vs
    , Pos !. "type" !* ctor          !. "=" !* variants :=> \_ _ c    _ vs -> Opaque c [] vs
    , Pos !. "type" !* ctor !* targs                    :=> \_ _ c ns      -> Opaque c ns []
    , Pos !. "type" !* ctor                             :=> \_ _ c         -> Opaque c [] []
    ]

  targs    <- some targ
  variants <- some variant

  targ <- rule "TArg"
    [ Pos        !* name                       :=> \_   n       -> TArg n Nothing
    , Pos !. "(" !* name !. ":" !* kind !. ")" :=> \_ _ n _ k _ -> TArg n (Just k)
    ]

  kind <- rule "Kind"
    [ Pos !* kindArrow :=> pass
    ]

  kindArrow <- rule "KArrow"
    [ Pos !* kindTerm !. "->" !* kindArrow :=> \_ d _ c -> KArr d c
    , Pos !* kindTerm                      :=> pass
    ]

  kindTerm <- rule "KTerm"
    [ Pos        !. "T"         :=> \_ _     -> Star
    , Pos !. "(" !* kind !. ")" :=> \_ _ k _ -> k
    ]

  variant <- rule "Variant"
    [ Pos !. "|" !* aCtor !. "{"           !. "}" :=> \_ _ c _ _ -> Variant c []
    , Pos !. "|" !* aCtor !. "{" !* fields !. "}" :=> \_ _ c _ fs _ -> Variant c fs
    ]

  fields <- tField `sepBy` sep

  tField <- rule "TField"
    [ Pos !* field !. ":" !* type_ :=> \_ n _ t -> TField n t
    ]

  field <- rule "Field" [ Pos !* name :=> \_ n -> Field n ]

  type_ <- rule "Type"
    [ Pos !* typeArrow :=> pass
    ]

  typeArrow <- rule "TArrow"
    [ Pos !* typeApp !. "->" !* typeArrow :=> \_ d _ c -> TArr d c
    , Pos !* typeApp                      :=> pass
    ]

  typeApp <- rule "TApp"
    [ Pos !* typeApp !* typeTerm :=> \_ f x -> TApp f x
    , Pos !* typeTerm            :=> pass
    ]

  typeTerm <- rule "TTerm"
    [ Pos        !* name         :=> \_ n     -> TVar n
    , Pos        !* qCtor        :=> \_ n     -> TCon n
    , Pos !. "(" !* type_ !. ")" :=> \_ _ t _ -> t
    ]

  qCtor <- rule "QCtor"
    [ Pos !* ctor                 :=> \_ c               -> QName [] c
    , Pos !* ctor !. "." !* qCtor :=> \_ c _ (QName p n) -> QName (c : p) n
    ]

  qName <- rule "QName"
    [ Pos !* name                 :=> \_ c               -> QName [] c
    , Pos !* ctor !. "." !* qName :=> \_ c _ (QName p n) -> QName (c : p) n
    ]

  aCtor <- rule "ACtor"
    [ Pos !* qCtor :=> \_ c -> Ctor c
    ]

  ctor <- rule @_ @SourcePos "Ctor" [ Pos !. "ctor" :=> \_ (LCtor c) -> fromText c ]
  name <- rule               "Name" [ Pos !. "name" :=> \_ (LVar  c) -> fromText c ]

  dot  <- rule "Dot"  [ Pos !. "." :=> \_ _ -> () ]
  semi <- rule "Semi" [ Pos !. ";" :=> \_ _ -> () ]

  sep <- rule "Separator"
    [ Pos !. "," :=> \_ _ -> ()
    , Pos !. ";" :=> \_ _ -> ()
    ]

  return module_

sepBy
  :: forall (t :: *) (p :: *) a (sep :: *)
  .  (Typeable p, Typeable sep, Typeable a, Typeable t)
  => Entity t p a
  -> Entity t p sep
  -> M (Entity t p [a])
sepBy a@(MkEntity aName) sep'@(MkEntity sepName) = mdo
  res <- rule (aName <> "/" <> sepName)
    [ Pos !* a                :=> \_ x      -> [x]
    , Pos !* a !* sep' !* res :=> \_ x _ xs -> x : xs
    ]
  return res

some
  :: forall (t :: *) (p :: *) a
  .  (Typeable p, Typeable a, Typeable t)
  => Entity t p a
  -> M (Entity t p [a])
some a@(MkEntity aName) = mdo
  res <- rule (aName <> "+")
    [ Pos !* a        :=> \_ x    -> [x]
    , Pos !* a !* res :=> \_ x xs -> x : xs
    ]
  return res

pass :: i -> a -> a
pass = const id

ignore :: i -> a -> ()
ignore _ _ = ()

data Lexeme
  = LIgnored Text
  | LNumber  Scientific
  | LString  Text
  | LVar     Text
  | LCtor    Text

instance Show Lexeme where
  show = \case
    LIgnored t -> show t
    LNumber  n -> show n
    LString  t -> show t
    LVar     n -> Text.unpack n
    LCtor    n -> Text.unpack n

lexer :: String -> Text -> Either String [(Term, SourcePos, Lexeme)]
lexer = runLexer "$" (LIgnored "$")
  [ nameLike  "name"   startName restName LVar    reserved
  , nameLike  "ctor"   startCtor restCtor LCtor   reserved
  , stringLit "string"                    LString
  , asToken   "number" L.scientific       LNumber
  , terms LIgnored reserved
  ]
  where
    reserved = "-> ( ) : , ; { } | type * let in \\ => = with case of module use as where def local T when [ ] ... . @ end"
    startName c = isAlpha c && isLower c
    startCtor c = isAlpha c && isUpper c
    restName  c = isAlpha c && isLower c || isDigit c || c `elem` ("-_?!'" :: String)
    restCtor  c = isAlphaNum c || c `elem` ("-?!_'" :: String)

terms :: (Text -> l) -> Text -> Parser (Term, SourcePos, l)
terms l txt = foldl (<|>) empty $ map (kw' l) do Text.words txt

kw' :: (Text -> l) -> Text -> Parser (Term, SourcePos, l)
kw' l t = kw t (MkTerm t) (l t)

input :: String -> Text -> [(Term, SourcePos, Lexeme)]
input f src = either error id $ lexer f src

-- test :: String -> Text -> (Either Component Ent)
test f src = runParser arith src $ input f src

pp :: (Functor f, Pretty (f Hide)) => f i -> IO ()
pp = print . pretty . fmap (const Hide)

data Hide = Hide

instance Show Hide where show _ = ""

angled, braced :: [Doc ann] -> Doc ann
braced = encloseSep "{" "}" ","
angled = encloseSep "<" ">" ","

located :: (Show i, Pretty (f i), Foldable f) => f i -> String
located fi = case getFirst (foldMap (First . Just) fi) of
  Just info -> show (pretty fi) <> " at " <> show info
  Nothing   -> error $ "structure with no info field: " <> show (pretty fi)
