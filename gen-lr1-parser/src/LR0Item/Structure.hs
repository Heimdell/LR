{- |
  Internal state of parsing a rule.
-}
module LR0Item.Structure where

import GHC.Records       (HasField(..))

import Data.Array qualified as Array

import Rule
import Symbol
import Data.Foldable
import Data.Text (Text)

{- |
  LR0Item in a rule during parsing process.
-}
data LR0Item = LR0Item
  { offset    :: Int      -- ^ mark of the rule
  , clause    :: Clause   -- ^ reference to the rule
  , type_     :: Maybe Text
  , entity    :: NonTerminal
  }
  deriving stock (Eq, Ord)

{- |
  > E = E + T
  >       ^
  Current point of the position.
-}
instance HasField "locus" LR0Item (Maybe Symbol) where
  getField LR0Item {clause, offset} =
    if offset >= length clause.points
    then Nothing
    else Just (clause.points Array.! offset)

instance HasField "reducer" LR0Item Text where
  getField LR0Item {clause} = clause.reducer

{- |
  > E = E + T
  >       ^

  > E = E + T
  >         ^

  Next position.
-}
instance HasField "next" LR0Item (Maybe LR0Item) where
  getField pos@LR0Item{clause, offset} =
    if offset >= length clause.points
    then Nothing
    else Just (pos :: LR0Item)
      { offset = offset + 1
      }

instance HasField "parsed" LR0Item [Symbol] where
  getField LR0Item {offset, clause} = take offset $ toList clause.points

{- |
  Start parsing a rule, expecting given `lookahead` term.
-}
startRule :: NonTerminal -> Maybe Text -> Clause  -> LR0Item
startRule entity type_ clause = LR0Item
  { offset = 0
  , clause
  , entity
  , type_
  }

-- examplePosSet :: Set LR0Item
-- examplePosSet = Set.fromList
--   [ fromJust $ (mkRule "T" [T "(", E "E", T ")"] "" `start` "$").next
--   ,             mkRule "E" [E "E", T "+", E "F"] "" `start` "+"
--   ]
