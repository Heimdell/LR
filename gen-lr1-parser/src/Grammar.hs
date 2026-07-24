module Grammar where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Function ( (&) )
import Data.Map.Monoidal (type (==>), (==>), (!?))
import Data.Text (Text)
import Data.Map (Map)
import Data.Map.Monoidal qualified as Monoidal
import Data.List.NonEmpty qualified as NonEmpty

import Rule
import Symbol

data Grammar = Grammar
  { imports :: [Text]
  , rules   :: Set Rule
  , types   :: Map NonTerminal Text
  , targets :: Set NonTerminal
  }

data CachedGrammar = CachedGrammar
  { leadingTerminals :: Entity ==> Set Terminal
  , rules            :: Entity ==> Set Rule
  , types            :: Map NonTerminal Text
  , terminals        :: Set Terminal
  }
  -- deriving stock (Show)

{- |
  Memoise calculation of leading terminals for each entity.

  Build index for rules based on the entity they parse.
-}
grammarToCachedGrammar :: Grammar -> CachedGrammar
grammarToCachedGrammar grammar = CachedGrammar
  { leadingTerminals = memoiseLeadingTerminals grammar
  , rules            = grammar.rules & foldMap \rule -> rule.entity ==> [rule]
  , types            = grammar.types
  , terminals        = grammarTerminals grammar
  }

grammarTerminals :: Grammar -> Set Terminal
grammarTerminals grammar =
  grammar.rules & foldMap \rule ->
    rule.symbols & foldMap \case
      _ :@ Term term -> [term]
      _              -> []

{- |
  Calculate sets of leading terminals for each entity.

  We need them for the situation like

  > [E ← E • Op F, ⊥]

  Where we need to start all rules for @Op@. For that we need lookahead set,
  which will be @LEADING-TERMS(F)@ in this case, because after @Op@ in this situation
  we expect only leading terminals of @F@.
-}
memoiseLeadingTerminals :: Grammar -> Entity ==> Set Terminal
memoiseLeadingTerminals grammar = go Monoidal.empty
  where
    go :: Entity ==> Set Terminal -> Entity ==> Set Terminal
    go cache = do
        let cache' = renew <> cache
        if cache == cache'
        then cache
        else go cache'
      where
        renew :: Entity ==> Set Terminal
        renew = grammar.rules & foldMap \rule -> do
          rule.entity ==> case (NonEmpty.head rule.symbols).symbol of
            NonTerm entity -> cache !? Named entity
            Term    term   -> [term]

{- |
  Grab the set of leading `Terminal`s for a `Symbol`.

  > LEADING-TERMS("+") = {"+"}
  > LEADING-TERMS("E") = {"(", "n"}
-}
leadingTerminalsOf :: CachedGrammar -> NamedSymbol -> Set Lookahead
leadingTerminalsOf cache = Set.map LookForTerm . \case
  _ :@ Term    term   -> [term]
  _ :@ NonTerm entity -> cache.leadingTerminals !? Named entity

rulesFor :: CachedGrammar -> Entity -> Set Rule
rulesFor cache entity = cache.rules !? entity
