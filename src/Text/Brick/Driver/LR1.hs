{-# OPTIONS_GHC -Wno-orphans #-}
{-# language Strict #-}

module Text.Brick.Driver.LR1 where

import Control.Arrow (second)
import Control.Monad (foldM, unless)
import Data.Array as Array
import Data.Function
import Data.Hashable (Hashable (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Map
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Data.List (sortBy, groupBy, transpose)
import Data.Maybe
import Data.Ord
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics
import GHC.Stack
import Numeric

import Debug.Trace

{- | Name for lexeme.
-}
newtype Term = MkTerm { raw :: Text }
  deriving stock   (Eq, Ord, Generic)
  deriving newtype (Hashable, IsString)

instance Show Term where
  show t = show t.raw

{- | Name for complex enitity.
-}
newtype Entity = MkEntity { raw :: Text }
  deriving stock   (Eq, Ord, Generic)
  deriving newtype (Hashable, IsString)

instance Show Entity where
  show e = Text.unpack e.raw

{- | Name to be unserted into `ParseTree` node.
-}
newtype Mark = MkMark { raw :: Text }
  deriving stock   (Eq, Ord, Generic)
  deriving newtype (Hashable, IsString)

instance Show Mark where
  show e = Text.unpack e.raw

{- | Lexeme or entity.
-}
data Point
  = Term   Term
  | Entity Entity
  deriving stock    (Eq, Ord, Generic)
  deriving anyclass (Hashable)

instance Show Point where
  show = \case
    Term   t -> show t
    Entity e -> show e

{- | Make a name out of piece of text. If uppercase, return entity;
     lexeme otherwise.
-}
mkPoint :: Text -> Point
mkPoint s@(Text.uncons -> Just (h, _))
  | h `Set.member` Set.fromList ['A'.. 'Z'] = Entity (MkEntity s)
  | otherwise                               = Term   (MkTerm s)
mkPoint _ = error "empty point"

{- | Grammar rule.
-}
data Rule = MkRule
  { entity :: Entity           -- ^ What does it parse?
  , points :: Array Int Point  -- ^ What does the entity consist of?
  , mark   :: Mark             -- ^ Node name in `ParseTree`.
  , hash   :: Int              -- ^ Cached `hash`.
  }
  deriving stock (Generic)

instance Eq Rule where
  a == b = a.hash == b.hash
    && checkCollision "eq" (==) True a.entity b.entity
    && checkCollision "eq" (==) True a.points b.points
    && checkCollision "eq" (==) True a.mark   b.mark

-- | Only the @(.hash)@ is compared to speed all up.
instance Ord Rule where
  compare a b = compare a.hash b.hash
    <> checkCollision "compare" compare EQ a.entity b.entity
    <> checkCollision "compare" compare EQ a.points b.points
    <> checkCollision "compare" compare EQ a.mark   b.mark

-- -- | Only the @(.hash)@ is compared to speed all up.
-- instance Eq  Rule where (==)    = (==)    `on` (.hash)

-- -- | Only the @(.hash)@ is compared to speed all up.
-- instance Ord Rule where compare = compare `on` (.hash)

-- | Precomputed @(.hash)@ is used instead of recalculating.
instance Hashable Rule where
  hashWithSalt salt rule = hashWithSalt salt rule.hash

{- | Make rule from entity name, list of points and a `ParseTree` mark.

     Precalculate `hash` into @(.hash)@
-}
mkRule :: Entity -> [Point] -> Mark -> Rule
mkRule e pts ctor = MkRule
  { entity = e
  , mark   = ctor
  , points = listArray (0, length pts - 1) pts
  , hash   = hashWithSalt (hash e) (hashWithSalt (hash pts) (hash ctor))
  }

instance Show Rule where
  show r =
    rightPad 8 (show r.mark)
      <> " . "
      <> rightPad 8 (show r.entity)
      <> " = " <> unwords (map show $ Array.elems r.points)

ruleShowTable :: Rule -> [String]
ruleShowTable rule =
  [ show rule.mark
  , "."
  , show rule.entity
  , "="
  , unwords (map show $ Array.elems rule.points)
  ]

{- | Pad string with spaces at right.
-}
rightPad :: Int -> String -> String
rightPad len s = s <> replicate (len - length s) ' '

{- | Grammar.
-}
data Grammar = MkGrammar
  { rules :: HashMap Entity (HashSet Rule)  -- ^ Ruleset
  , order :: [Rule]                         -- ^ Ruleset in order of declaration
  }

{- | Make a grammar from a list of rules.
-}
makeGrammar :: [Rule] -> Grammar
makeGrammar = foldr push (MkGrammar Map.empty [])
  where
    push r g = g
      { rules = Map.unionWith (<>) (Map.singleton r.entity (Set.singleton r)) g.rules
      , order = r : g.order
      }

{- | Monoidal `lookup` into `HashMap`.
-}
find :: (Hashable a, Monoid v) => a -> HashMap a v -> v
find k m = maybe mempty id (Map.lookup k m)

instance Show Grammar where
  show g =
    g.order
      & groupEntities
      & map (unlines . untable . map ruleShowTable)
      & unlines
    where
      groupEntities =
        map (map snd) .
        groupBy ((==) `on` fst) .
        map \r -> (r.entity, r)

      untable :: [[String]] -> [String]
      untable tbl = do
        let
          transp = transpose tbl

          colLens = transp
            & map (maximum . map length)

          padded = zipWith (map . rightPad) colLens transp
        map unwords (transpose padded)


{- | Relation (`Entity` -> set of first `Term`-s).
-}
data First = MkFirst
  { terms :: HashMap Entity (HashSet Term)
  }

instance Show First where
  show f =
    f.terms
      & Map.map do \ts -> unwords (map show (Set.toList ts))
      & Map.toList
      & map do \(e, ts) -> show e <> " = {" <> ts <> "}"
      & unlines

{- | Generate FIRST table from grammar.
-}
firstsFromGrammar :: Grammar -> First
firstsFromGrammar (MkGrammar _ rs) = MkFirst $ loop mempty
  where
    loop f = do
      let new = foldr firstInRule f rs
      if f == new  -- is run until result stops changing
      then f
      else loop new

    firstInRule r acc =
      Map.insertWith (<>) r.entity delta acc
      where
        delta =
          case r.points Array.! 0 of
            Entity e -> find e acc
            Term   t -> Set.singleton t

{- | Make FIRST also recognise `Term`-s (they are returned as `Set.singleton`-s).
-}
firstTerms :: Point -> First -> HashSet Term
firstTerms (Term   t) _ = Set.singleton t
firstTerms (Entity e) f = find e f.terms

{- | Position of the parser in the `Rule`.
-}
data Position = MkPosition
  { rule      :: Rule  -- ^ `Rule` this `Position` points to.
  , pos       :: Int   -- ^ Count of parsed `Point`-s.
  , lookahead :: Term  -- ^ The expected `Term` in reducing situation.
  , hash      :: Int   -- ^ Precomputed `hash`.
  }
  deriving stock (Generic)

-- | Only the @(.hash)@ is compared to speed all up.
instance Eq Position where
  a == b = a.hash == b.hash
    && checkCollision "eq" (==) True a.rule      b.rule
    && checkCollision "eq" (==) True a.pos       b.pos
    && checkCollision "eq" (==) True a.lookahead b.lookahead

-- | Only the @(.hash)@ is compared to speed all up.
instance Ord Position where
  compare a b = compare a.hash b.hash
    <> checkCollision "compare" compare EQ a.rule      b.rule
    <> checkCollision "compare" compare EQ a.pos       b.pos
    <> checkCollision "compare" compare EQ a.lookahead b.lookahead
-- instance Eq St where
--   a == b = a.hash == b.hash && checkCollision "==" (==) True a.positions b.positions

-- -- | Only the @(.hash)@ is compared to speed all up.
-- instance Ord St where
--   compare a b = compare a.hash b.hash <> checkCollision "compare" compare EQ a.positions b.positions

-- | Precomputed @(.hash)@ is used instead of recalculating.
instance Hashable Position where
  hashWithSalt salt pos = hashWithSalt salt pos.hash

instance Show Position where
  show i =
    rightPad 8 (show i.rule.mark)
      <> " . "
      <> rightPad 8 (show i.rule.entity <> " " <> show i.lookahead)
      <> " = " <> unwords (map show before)
      <> " . " <> unwords (map show after)
    where
      (before, after) = splitAt i.pos $ Array.elems i.rule.points

-- | Create starting position of the rule with given lookahead term.
startingPosition
  :: Rule
  -> Term  -- ^ lookahead term
  -> Position
startingPosition rule lookahead = MkPosition
  { rule
  , lookahead
  , pos = 0
  , hash = hashWithSalt rule.hash (hash lookahead)
  }

{- | Uncons a rule, generating current `Point` to be parsed and next position.

     The hash is recalculated as @hash1 = hash (hash0 + 1)@.
-}
locusAndNext :: Position -> Maybe (Point, Position)
locusAndNext pos = do
  if snd (Array.bounds pos.rule.points) >= pos.pos
  then Just (pos.rule.points Array.! pos.pos, pos { pos = pos.pos + 1, hash = hash (pos.hash + 1) })
  else Nothing

{- | Return next point to be parsed, if any.
-}
locus :: Position -> Maybe Point
locus pos = fst <$> locusAndNext pos

{- | Return next position, if any.
-}
next :: Position -> Maybe Position
next pos = snd <$> locusAndNext pos

{- | Return two next points to be parsed, if any.
-}
twoLoci :: Position -> (Maybe Point, Maybe Point)
twoLoci pos =
  ( locus pos
  , locus =<< next pos
  )

{- | Parser state. Is a set of positions.

     Parser is in all of those positions /simultaneously/.

     This is because `Entity`-es are transparent, and you can shove in their
     definition in their place.

     Therefore, if you have a position

         T = ( . E )

     then you are in all starting positions of @E@ at the same time.
-}
data St = MkSt
  { kernel    :: HashSet Position  -- ^ The initial set of positions to gen state from.
  , positions :: HashSet Position  -- ^ The full `closure` of kernel.
  , hash      :: Int               -- ^ Precalculated `hash`
  }


-- | Only the @(.hash)@ is compared to speed all up.
instance Eq St where
  a == b = a.hash == b.hash && checkCollision "==" (==) True a.positions b.positions

-- | Only the @(.hash)@ is compared to speed all up.
instance Ord St where
  compare a b = compare a.hash b.hash <> checkCollision "compare" compare EQ a.positions b.positions

-- | Precomputed @(.hash)@ is used instead of recalculating.
instance Hashable St where
  hashWithSalt salt st = hashWithSalt salt st.hash

instance Show St where
  show st =
    showHex (fromIntegral st.hash :: Word) "\n" <> do
      st.kernel
        & Set.toList
        & map (("  " <>) . show)
        & unlines

{- Uncomment this for 10% slowdown.
-}
-- checkCollision :: (Show a, Show b, Eq b, HasCallStack) => Text -> (a -> a -> b) -> b -> a -> a -> b
-- checkCollision name cmp mustBe a b = do
--   let res = cmp a b
--   if res /= mustBe
--   then error $ show ("hash collision" :: Text, name, a, b, "=" :: Text, res)
--   else res

{- | Turns out hash collisions on Int64 `hash`-es are very rare. Does not check anything.
-}
checkCollision :: (Show a, Show b, Eq b) => Text -> (a -> a -> b) -> b -> a -> a -> b
checkCollision _name _cmp mustBe _a _b = mustBe

{- | Make full state from the kernel using grammar and FIRST.
-}
closure :: Grammar -> First -> HashSet Position -> St
closure grammar first kernel = do
  let positions = loop kernel kernel
  MkSt
    { kernel
    , positions
    , hash = hash positions
    }
  where
    loop pool acc = do
      let new   = foldMap findPositions pool
      let delta = Set.filter (not . (`Set.member` acc)) new
      if Set.null delta
      then acc
      else loop delta (delta <> acc)

    findPositions :: Position -> HashSet Position
    findPositions i =
      case twoLoci i of
        (Just (Entity e), Just p) ->
          flip foldMap (find e grammar.rules) \rule ->
          flip Set.map (firstTerms p first) \lookahead ->
            startingPosition rule lookahead

        (Just (Entity e), Nothing) ->
          flip Set.map (find e grammar.rules) \rule ->
            startingPosition rule i.lookahead

        _ -> Set.empty

{- | Make first state of the parser.

     Assumes that entity @"S"@ has exactly one rule.

     Will use "$" as lookahad term name, with end-of-text semantic.
-}
firstState :: Grammar -> First -> St
firstState g f = closure g f $ Set.map (`startingPosition` "$") $ find "S" g.rules

{- | GOTO table.

     Answers a question: in the state S with point P on top of the parse stack,
     which state to go in?
-}
data Goto = MkGoto
  { moves :: HashMap St (HashMap Point St)
  }

{- | Make GOTO transition table.

     Works on state stack in a cycle. Initially, stack only contains first state.

     For each non-yet-visited state in the stack,
     group all items in the state by their locus `Point`.

     Then for each group with unique locus `Point`, apply `nextPosition` to them,
     and push closures of the groups into state stack.

     Add transition @(state -> point -> closure(group))@ for each @point x group@
     pair.

     Example:

     If you have a position

         T = ( . E )
         P = . E foo Bar
         K = . T e
         F = a . K

     in state S1 then GOTO table will contain, for @(S1, E)@

         (S1, E) -> closure
           [ T = ( E . )
           , P = E . foo Bar
           ]

-}
mkGotos :: Grammar -> First -> Goto
mkGotos grammar first = loop [firstState grammar first] Set.empty (MkGoto Map.empty)
  where
    loop :: [St] -> HashSet St -> Goto -> Goto
    loop []          _       goto = goto
    loop (st : rest) visited goto | Set.member st visited = loop rest visited goto
    loop (st : rest) visited (MkGoto goto) = do
      let
        visited' = Set.insert st visited
        grouped = st.positions
          & Set.toList
          & sortBy  do comparing locus
          & groupBy do (==) `on` locus
          & map     do mapMaybe locusAndNext
          & filter  do not . null
          & map     do regroup
          & map     do second (closure grammar first)

        regroup grp@((pt, _) : _) = (pt, Set.fromList $ map snd grp)
        regroup _                 = error "groupBy is broken"

        nexts = map snd grouped
        goto' = Map.insert st (Map.fromList grouped) goto

      loop (nexts ++ rest) visited' (MkGoto goto')

instance Show Goto where
  show goto =
    goto.moves
      & Map.toList
      & map (second Map.toList)
      & map do \(st, movs) -> show st <> "\n" <> (showMove =<< movs)
      & unlines
    where
      showMove (point, st) =
        "  " <> show point <> "\n"
          <> indent 4 (show st)

{- | Add some spaces to the left.
-}
indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' <>) . lines

{- | Possible actions of the parser.
-}
data Action
  = Shift     St               -- ^ Push current token into stack, goto given state
  | Reduce    Rule             -- ^ Perform reduction with that rule
  | Conflict (HashSet Action)  -- ^ Resolve conflict (LR(1) driver can't and will crash).
  | Accept                     -- ^ Return top parsed value as result.
  deriving stock    (Eq, Ord, Generic)
  deriving anyclass (Hashable)

{- | If two actions are the same, return left.

     If they are different, pack them into `Conflict`.

     If any is already a `Conflict`, merge conflicts.
-}
instance Semigroup Action where
  a <> b | a == b = a
  Conflict as <> Conflict bs = Conflict (as <> bs)
  Conflict as <> b = Conflict (as <> Set.singleton b)
  a <> Conflict bs = Conflict (Set.singleton a <> bs)
  a <> b = Conflict $ Set.fromList [a, b]

instance Show Action where
  show = \case
    Shift st ->
      "Shift\n" <> indent 2 (show st)

    Reduce rule ->
      "Reduce\n" <> indent 2 (show rule)

    Accept -> "Accept"

    Conflict set ->
      "Conflict\n" <> indent 2 (unlines (map show (Set.toList set)))

{- | ACTION table.

     Answers the question: in the state S and /term/ T what `Action` should be
     performed?
-}
data Actions = MkActions
  { act :: HashMap St (HashMap Term Action)
  }

{- | We generate ACTIONs from GOTOs.

     For each source state S1 in GOTO, for each `Position` in S1,

     1) if locus is empty (whole ruls is parsed) and entity is "S", accept;

     2) if locus is empty, reduce with rule from the position;

     3) if locus is `Term`, shift that term.

     Conflicts cat emerge from the fact that we `(<>)` all the @(term -> action)`@-s
     from each `Position` in the state S1.
-}
mkActions :: Goto -> Actions
mkActions goto = MkActions $ foldr addState Map.empty (Map.keys goto.moves)
  where
    addState :: St -> HashMap St (HashMap Term Action) -> HashMap St (HashMap Term Action)
    addState st acc = do
      foldr (addPosition st) acc st.positions

    addPosition :: St -> Position -> HashMap St (HashMap Term Action) -> HashMap St (HashMap Term Action)
    addPosition st pos acc = do
      let update act = Map.insertWith (<>) st act acc
      case locus pos of
        Nothing
          | pos.rule.entity == "S" -> update $ Map.singleton pos.lookahead  Accept
          | otherwise              -> update $ Map.singleton pos.lookahead (Reduce pos.rule)

        Just (Term t) -> update $ Map.singleton t do Shift (goto.moves Map.! st Map.! Term t)
        _             -> acc

instance Show Actions where
  show goto =
    goto.act
      & Map.toList
      & map (second Map.toList)
      & map do \(st, movs) -> show st <> "\n" <> (showMove =<< movs)
      & unlines
    where
      showMove (point, st) =
        "  " <> show point <> "\n"
          <> indent 4 (show st)

{- | A structure to pack result of the parse in.
-}
data ParseTree pos a
  = Atom pos a
  | Branch Mark pos [ParseTree pos a]

{- | Does not show `Mark`-s; instead, dumps all tokens an arranges parentheses
     around them.

     Parentheses are omitted for one-item nodes for clarity.
-}
instance Show a => Show (ParseTree pos a) where
  show = \case
    Atom _ a -> show a
    Branch _ _ [x] -> show x
    Branch _ _ xs -> "(" <> unwords (map show xs) <> ")"

data ParseError pos = ParseError
  { unexpected :: Term
  , position   :: pos
  , expected   :: HashSet Term
  , reductions :: HashSet Entity
  }

{- | Run a parser using ACTION and GOTO table from a starting state.

     Is a left fold with `consume`.

     The error
-}
parse
  :: Actions
  -> Goto
  -> St
  -> [(Term, pos, lexeme)]
  -> Either (ParseError pos) (ParseTree pos lexeme)
parse actions goto initial input = do
  let allConflicts = conflicts actions
  unless (null allConflicts.act) do
    traceM "Conflicts:"
    traceShowM allConflicts
  (_, [(_, result)]) <- foldM (consume actions goto) ([initial], []) input
  return result

consume
  :: Actions
  -> Goto
  -> ([St], [(pos, ParseTree pos lexeme)])
  -> (Term, pos, lexeme)
  -> Either (ParseError pos) ([St], [(pos, ParseTree pos lexeme)])
consume actions goto (top : states, stack) (term, pos, lexeme) = do
  let positionsibleActions = actions.act Map.! top

  unless (term `Map.member` positionsibleActions) do
    let
      positionsibleProductions =
        Map.keysSet (goto.moves Map.! top)
          & foldMap \case
              Entity e -> Set.singleton e
              _        -> Set.empty
    traceShowM top
    Left (ParseError term pos (Map.keysSet positionsibleActions) positionsibleProductions)

  case positionsibleActions Map.! term of
    Shift st      -> return (st : top : states, (pos, Atom pos lexeme) : stack)
    Accept        -> return (top : states, stack)
    Conflict set  -> error $ "LR(1) driver cannot resolve conflicts\n" <> indent 2 (show set)
    Reduce   rule -> do
      let size             = length rule.points
      case drop size (top : states) of
        top' : states' -> do
          let (rtaken, stack') = splitAt size stack
          let taken            = reverse rtaken
          let top''            = goto.moves Map.! top' Map.! Entity rule.entity
          let states''         = top'' : top' : states'
          let subtrees         = map snd taken
          let start            = fst (head taken)
          let subtree          = Branch rule.mark start subtrees
          let stack''          = (start, subtree) : stack'
          consume actions goto (states'', stack'') (term, pos, lexeme)

        _ -> error "parser state is corrupted"

consume _ _ _ _ = error "parser state is corrupted"

instance MonadFail (Either e) where
  fail = error

{- | Simple endpoint. Generate tables and run parser.

     Will generate tables as soon as first argument is received.
-}
runLR1
  :: Grammar
  -> [(Term, pos, lexeme)]
  -> Either (ParseError pos) (ParseTree pos lexeme)
runLR1 grammar = do
  let first  = firstsFromGrammar grammar
  let start  = firstState grammar first
  let goto   = mkGotos grammar first
  let action = mkActions goto
  traceShow grammar $
   traceShow (length goto.moves) $
    parse action goto start

gotoStateCount :: Goto -> Int
gotoStateCount goto =
  length $ Map.keysSet goto.moves <> (foldMap . foldMap) Set.singleton goto.moves

{- | Make parse error pretty.
-}
report
  :: IsPosition pos
  => Text            -- ^ Parsed source.
  -> ParseError pos
  -> String
report src (ParseError term sp terms entities) = do
  let prefixLength = length (posName sp) + 1 + posColumn sp - 1
  let line = Text.lines src !! (posLine sp - 1)
  let lineNo = show (posLine sp)
  "\n" <> posName sp <> ":" <> lineNo <> ":" <> Text.unpack line <> "\n"
    <> replicate prefixLength ' ' <> map (const ' ') lineNo <> " ^\n"
    <> "expected " <> makeList (map show (Set.toList terms))
    <> " but got " <> show term
    <> case Set.toList entities of
         [] -> "."
         list -> ",\nas beginning of " <> makeList (map show list) <> ".\n"

{- | Class to abstract out a position.
-}
class IsPosition p where
  posName   :: p -> String
  posLine   :: p -> Int
  posColumn :: p -> Int

makeList :: [String] -> String
makeList [] = "nothing (the grammar is incorrect, probably)"
makeList [x] = x
makeList [x, y] = x <> " or " <> y
makeList (x : xs) = x <> ", " <> makeList xs

conflicts :: Actions -> Actions
conflicts action = action
  { act = action.act & Map.filter (any isConflict) & Map.map (Map.filter isConflict)
  }

isConflict :: Action -> Bool
isConflict Conflict {} = True
isConflict _           = False
