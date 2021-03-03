
module Exts where

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.List

import Data.Char        (isUpper)
import Data.List        (intercalate)
import Data.Traversable (for)
import Data.String      (IsString (..))

import Pretty
import Name
import Table
import Set qualified
import Set (Set)
import Rule
import Point
import Term

import S
import Tree

data ETable term = ETable
  { etRules :: [ERule term]
  }
  deriving Show via PP (ETable term)

instance Pretty term => Pretty (ETable term) where
  pretty = vcat . map pretty . etRules

data ERule term = ERule
  { erName   ::  Name
  , erPoints :: [EPoint term]
  , erAction :: Name
  }
  deriving Show via PP (ERule term)

data EPRod term = EPRod
  { epName    ::  Name
  , epBefore  :: [EPoint term]
  , epCurrent ::  EPoint term
  , epAfter   :: [EPoint term]
  , epAction  ::  Name
  }

instance Pretty term => Pretty (ERule term) where
  pretty (ERule name points _) =
    pretty name <+> "=" `indent` fsep (map pretty points)

data EPoint term
  = ETerm term
  | EName Name
  | EPlus (EPoint term)
  | EMult (EPoint term)
  | EOpt  (EPoint term)
  | EOr   [EPoint term]
  | ESeq  [EPoint term]
  deriving Show via PP (EPoint term)

instance Pretty term => Pretty (EPoint term) where
  pretty = \case
    ETerm t -> pretty t
    EName n -> pretty n
    EPlus p -> parens (pretty p) <.> "+"
    EMult p -> parens (pretty p) <.> "*"
    EOpt  p -> parens (pretty p) <.> "?"
    EOr  ps -> parens (fsep $ punctuate " |" $ map pretty ps)
    ESeq ps -> fsep $ map pretty ps

instance IsString term => IsString (EPoint term) where
  fromString str@(fstChar : _)
    | isUpper fstChar = EName (Name       str)
    | otherwise       = ETerm (fromString str)

  fromString [] = error "Point.fromString: empty string is not allowed"

asString :: Pretty term => String -> [Point term] -> Name
asString s pts = fromString $ filter (not . (`elem` ("\n " :: String))) $ intercalate "-" (s : map show pts)

instance MonadWriter w m => MonadWriter w (ListT m) where
  tell   = lift . tell
  listen = error "ListT.listen"
  pass   = error "ListT.pass"

compileEPoint :: (Pretty term, Ord term) => EPoint term -> ListT (Writer (Set (Rule term))) [Point term]
compileEPoint = loop
  where
    loop = \case
      ETerm r -> return [Term    r]
      EName n -> return [NonTerm n]
      EPlus p -> do
        pts <- loop p
        let ptsName = asString "One"  pts
        let newName = asString "Plus" pts
        tell $ Set.fromList
          [ Rule newName [NonTerm ptsName, NonTerm newName] "Cons"
          , Rule newName [NonTerm ptsName]                  "One"
          , Rule ptsName pts                                "Pts"
          ]
        return [NonTerm newName]

      EMult p -> do
        pts <- loop (EPlus p)
        return pts <|> return []

      EOpt p -> do
        pts <- loop p
        return pts <|> return []

      EOr ps -> do
        ptss <- for ps loop
        list return ptss

      ESeq ps -> do
        ptss <- for ps loop
        return (join ptss)

compileERule :: (Ord term, Pretty term) => ERule term -> ListT (Writer (Set (Rule term))) (Rule term)
compileERule (ERule name epoints reduce) = do
  ptss <- for epoints compileEPoint
  return $ Rule name (join ptss) reduce

compileETable :: (Ord term, Pretty term) => ETable term -> Table term
compileETable (ETable erules) = do
  let action = list compileERule erules
  let (rules, additional) = runWriter $ runListT action
  Table $ Set.toList $ Set.fromList rules <> additional

list :: Monad m => (a -> ListT m b) -> [a] -> ListT m b
list f xs = ListT do
  ys <- for xs (runListT . f)
  return (join ys)

-- test :: ETable S
-- test = ETable
--   [ ERule  Start   ["Value"]                                                      "S"
--   , ERule "Value"  [EOr ["Str", "Array", "Object"]]                              "-Value"
--   , ERule "Str"    ["str"]                                                       "-Value"
--   , ERule "Array"  ["[", EOpt (ESeq ["Value", EMult (ESeq [",", "Value"])]), "]"] "Array"
--   , ERule "Object" ["{", EOpt (ESeq ["Pair",  EMult (ESeq [",", "Pair"])]),  "}"] "Object"
--   , ERule "Pair"   ["Str", ":", "Value"]                                          "Pair"
--   ]