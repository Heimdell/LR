
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-star-is-type #-}

module E where

import Control.Applicative
import Data.Monoid (First (..))
import Data.Char
import Data.Scientific
import Data.Text qualified as Text
import Data.Text (Text)
import Data.Typeable
import Prettyprinter

import Text.Brick.DSL
import Text.Brick.Lexer qualified as Lexer
import Text.Brick.Lexer (SourcePos, Parser, runLexer, stringLit, asToken, nameLike, kw)

import AST

arith :: M (Entity Lexeme SourcePos Prog)
arith = mdo
  expr <- rule "Expression"
    [ Pos !* letExpr :=> pass
    ]

  letExpr <- rule "Let-expression"
    [ Pos !. "let" !* name !. "=" !* expr !. "in" !* expr :=> \_ _ n _ e _ k -> Let n e k
    , Pos !* app                                          :=> pass
    ]

  app <- rule "Application"
    [ Pos !* app  !* term     :=> \_ f x -> App f x
    , Pos !* term             :=> pass
    ]

  term <- rule "Terminal-expression"
    [ Pos !. "(" !* expr !. ":" !* type_ !. ")" :=> \_ _ e _ t _  -> Ann e t
    , Pos !. "(" !* expr !. ")"                 :=> \_ _ e     _  -> e
    , Pos !. "\\" !* arg !. "=>" !* expr        :=> \_ _ as _ e   -> Lam as e
    , Pos !* name                               :=> \_ n          -> Var n
    , Pos !* con                                :=> pass
    ]

  con <- rule "Constant"
    [ Pos !. "(" !. ")" :=> \_ _ _ -> Con Unit
    ]

  type_ <- rule "Type"
    [ Pos !* typeArrow :=> pass
    ]

  typeArrow <- rule "Function-type"
    [ Pos !* typeTerm !. "->" !* typeArrow :=> \_ d _ c -> TArr d c
    , Pos !* typeTerm                      :=> pass
    ]

  typeTerm <- rule "Small-type"
    [ Pos !. "(" !* type_ !. ")" :=> \_ _ t _    -> t
    , Pos !*  name               :=> \_ n        -> TVar n
    ]

  arg <- rule "Argument"
    [ Pos !* name :=> \_ n     -> n
    ]

  name <- rule "Name"
    [ Pos !. "name" :=> \_ (LVar n) -> fromText n
    ]

  -- comma <- rule "','"
  --   [ Pos !. "," :=> \_ _ -> ()
  --   ]

  -- bar <- rule "'|'"
  --   [ Pos !. "|" :=> \_ _ -> ()
  --   ]

  -- semi <- rule "';'"
  --   [ Pos !. ";" :=> \_ _ -> ()
  --   ]

  return expr

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
  [ stringLit "string"                    LString
  , asToken   "number" Lexer.scientific   LNumber
  , nameLike  "name"   startName restName LVar    reserved
  , nameLike  "ctor"   startCtor restName LCtor   reserved
  , terms LIgnored reserved
  ]
  where
    reserved = "( ) { } : , . ; | @ let in with case of record union as when \\ -> => = type mu /\\ forall"
    startName c = isAlpha c && isLower c
    startCtor c = isAlpha c && isUpper c
    restName  c = isAlphaNum c || c `elem` ("-?!_" :: String)

terms :: (Text -> l) -> Text -> Parser (Term, SourcePos, l)
terms l txt = foldl (<|>) empty $ map (kw' l) do Text.words txt

kw' :: (Text -> l) -> Text -> Parser (Term, SourcePos, l)
kw' l t = kw t (MkTerm t) (l t)

input :: String -> Text -> [(Term, SourcePos, Lexeme)]
input f src = either error id $ lexer f src

test :: String -> Text -> Prog
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