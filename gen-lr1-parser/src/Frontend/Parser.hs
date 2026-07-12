{-# language PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
module Frontend.Parser (
    parseGrammar
) where
  
import Data.Text.IO.Utf8 qualified as Text
import Data.Kind qualified as Kind
import Rule
import Term
import Text.Lexer.Default
import Data.Text.Position
import Data.Text (Text)
import Data.Lexeme
import Data.Array
import Data.Set qualified as Set
import Data.Set (Set)
listToArray :: [a] -> Array Int a
listToArray as = Data.Array.listArray (0, length as - 1) as
  
data StackGrammar' xs where
  NilGrammar  ::      StackGrammar' '[]
  PushGrammar :: x -> StackGrammar xs -> StackGrammar' (x : xs)
  
type StackGrammar a = (StGrammar a, Pos, StackGrammar' a)
  
pattern PushGrammar' :: a -> StackGrammar xs -> StackGrammar (a : xs)
pattern PushGrammar' a xs <- (_, _, PushGrammar a xs)
  
data StGrammar :: [Kind.Type] -> Kind.Type where
  SGrammar0 :: StGrammar (a)
  SGrammar1 :: StGrammar (([Text], Set Entity, [Rule]) : a)
  SGrammar2 :: StGrammar ([Point] : a)
  SGrammar3 :: StGrammar (Clause : a)
  SGrammar4 :: StGrammar (Entity : a)
  SGrammar5 :: StGrammar (Rule : a)
  SGrammar6 :: StGrammar ([Text] : a)
  SGrammar7 :: StGrammar (() : [Point] : a)
  SGrammar8 :: StGrammar (() : Clause : a)
  SGrammar9 :: StGrammar (() : Entity : a)
  SGrammar10 :: StGrammar (() : Entity : a)
  SGrammar11 :: StGrammar ([Rule] : Rule : a)
  SGrammar12 :: StGrammar ([Entity] : [Text] : a)
  SGrammar13 :: StGrammar (Text : () : [Point] : a)
  SGrammar14 :: StGrammar ([Clause] : () : Clause : a)
  SGrammar15 :: StGrammar (Text : () : Entity : a)
  SGrammar16 :: StGrammar ([Clause] : () : Entity : a)
  SGrammar17 :: StGrammar ([Rule] : [Entity] : [Text] : a)
  SGrammar18 :: StGrammar (() : Text : () : Entity : a)
  SGrammar19 :: StGrammar ([Clause] : () : Text : () : Entity : a)
  SGrammar20 :: StGrammar (Text : a)
  SGrammar21 :: StGrammar (Text : a)
  SGrammar22 :: StGrammar (() : a)
  SGrammar23 :: StGrammar (Entity : a)
  SGrammar24 :: StGrammar ([Entity] : () : a)
  SGrammar25 :: StGrammar (() : Entity : a)
  SGrammar26 :: StGrammar ([Entity] : () : Entity : a)
  SGrammar27 :: StGrammar (Text : a)
  SGrammar28 :: StGrammar (Text : a)
  SGrammar29 :: StGrammar (Text : a)
  SGrammar30 :: StGrammar (Term : a)
  SGrammar31 :: StGrammar (() : Text : a)
  SGrammar32 :: StGrammar (Term : () : Text : a)
  SGrammar33 :: StGrammar (Entity : () : Text : a)
  SGrammar34 :: StGrammar (Point : a)
  SGrammar35 :: StGrammar ([Point] : Point : a)
  SGrammar36 :: StGrammar (() : a)
  SGrammar37 :: StGrammar (Text : a)
  SGrammar38 :: StGrammar ([Text] : () : a)
  SGrammar39 :: StGrammar ([Text] : Text : a)
  
__gotoAdditionsForGrammar :: ([Lexeme], Pos) -> [Text] -> StackGrammar a -> Either (Pos, [String]) ([Text], Set Entity, [Rule])
__gotoAdditionsForGrammar toks term stk@(state, _, _) = case state of
  SGrammar0 -> __runGrammar SGrammar6 toks (PushGrammar term stk)
  _ -> error ""

__gotoClauseForGrammar :: ([Lexeme], Pos) -> Clause -> StackGrammar a -> Either (Pos, [String]) ([Text], Set Entity, [Rule])
__gotoClauseForGrammar toks term stk@(state, _, _) = case state of
  SGrammar8 -> __runGrammar SGrammar3 toks (PushGrammar term stk)
  SGrammar10 -> __runGrammar SGrammar3 toks (PushGrammar term stk)
  SGrammar18 -> __runGrammar SGrammar3 toks (PushGrammar term stk)
  _ -> error ""

__gotoClausesForGrammar :: ([Lexeme], Pos) -> [Clause] -> StackGrammar a -> Either (Pos, [String]) ([Text], Set Entity, [Rule])
__gotoClausesForGrammar toks term stk@(state, _, _) = case state of
  SGrammar8 -> __runGrammar SGrammar14 toks (PushGrammar term stk)
  SGrammar10 -> __runGrammar SGrammar16 toks (PushGrammar term stk)
  SGrammar18 -> __runGrammar SGrammar19 toks (PushGrammar term stk)
  _ -> error ""

__gotoEntitiesForGrammar :: ([Lexeme], Pos) -> [Entity] -> StackGrammar a -> Either (Pos, [String]) ([Text], Set Entity, [Rule])
__gotoEntitiesForGrammar toks term stk@(state, _, _) = case state of
  SGrammar22 -> __runGrammar SGrammar24 toks (PushGrammar term stk)
  SGrammar25 -> __runGrammar SGrammar26 toks (PushGrammar term stk)
  _ -> error ""

__gotoEntityForGrammar :: ([Lexeme], Pos) -> Entity -> StackGrammar a -> Either (Pos, [String]) ([Text], Set Entity, [Rule])
__gotoEntityForGrammar toks term stk@(state, _, _) = case state of
  SGrammar5 -> __runGrammar SGrammar4 toks (PushGrammar term stk)
  SGrammar12 -> __runGrammar SGrammar4 toks (PushGrammar term stk)
  SGrammar22 -> __runGrammar SGrammar23 toks (PushGrammar term stk)
  SGrammar25 -> __runGrammar SGrammar23 toks (PushGrammar term stk)
  SGrammar31 -> __runGrammar SGrammar33 toks (PushGrammar term stk)
  _ -> error ""

__gotoGrammarForGrammar :: ([Lexeme], Pos) -> ([Text], Set Entity, [Rule]) -> StackGrammar a -> Either (Pos, [String]) ([Text], Set Entity, [Rule])
__gotoGrammarForGrammar toks term stk@(state, _, _) = case state of
  SGrammar0 -> __runGrammar SGrammar1 toks (PushGrammar term stk)
  _ -> error ""

__gotoLinesForGrammar :: ([Lexeme], Pos) -> [Text] -> StackGrammar a -> Either (Pos, [String]) ([Text], Set Entity, [Rule])
__gotoLinesForGrammar toks term stk@(state, _, _) = case state of
  SGrammar36 -> __runGrammar SGrammar38 toks (PushGrammar term stk)
  SGrammar37 -> __runGrammar SGrammar39 toks (PushGrammar term stk)
  _ -> error ""

__gotoPointForGrammar :: ([Lexeme], Pos) -> Point -> StackGrammar a -> Either (Pos, [String]) ([Text], Set Entity, [Rule])
__gotoPointForGrammar toks term stk@(state, _, _) = case state of
  SGrammar8 -> __runGrammar SGrammar34 toks (PushGrammar term stk)
  SGrammar10 -> __runGrammar SGrammar34 toks (PushGrammar term stk)
  SGrammar18 -> __runGrammar SGrammar34 toks (PushGrammar term stk)
  SGrammar34 -> __runGrammar SGrammar34 toks (PushGrammar term stk)
  _ -> error ""

__gotoPointsForGrammar :: ([Lexeme], Pos) -> [Point] -> StackGrammar a -> Either (Pos, [String]) ([Text], Set Entity, [Rule])
__gotoPointsForGrammar toks term stk@(state, _, _) = case state of
  SGrammar8 -> __runGrammar SGrammar2 toks (PushGrammar term stk)
  SGrammar10 -> __runGrammar SGrammar2 toks (PushGrammar term stk)
  SGrammar18 -> __runGrammar SGrammar2 toks (PushGrammar term stk)
  SGrammar34 -> __runGrammar SGrammar35 toks (PushGrammar term stk)
  _ -> error ""

__gotoRuleForGrammar :: ([Lexeme], Pos) -> Rule -> StackGrammar a -> Either (Pos, [String]) ([Text], Set Entity, [Rule])
__gotoRuleForGrammar toks term stk@(state, _, _) = case state of
  SGrammar5 -> __runGrammar SGrammar5 toks (PushGrammar term stk)
  SGrammar12 -> __runGrammar SGrammar5 toks (PushGrammar term stk)
  _ -> error ""

__gotoRulesForGrammar :: ([Lexeme], Pos) -> [Rule] -> StackGrammar a -> Either (Pos, [String]) ([Text], Set Entity, [Rule])
__gotoRulesForGrammar toks term stk@(state, _, _) = case state of
  SGrammar5 -> __runGrammar SGrammar11 toks (PushGrammar term stk)
  SGrammar12 -> __runGrammar SGrammar17 toks (PushGrammar term stk)
  _ -> error ""

__gotoStartersForGrammar :: ([Lexeme], Pos) -> [Entity] -> StackGrammar a -> Either (Pos, [String]) ([Text], Set Entity, [Rule])
__gotoStartersForGrammar toks term stk@(state, _, _) = case state of
  SGrammar6 -> __runGrammar SGrammar12 toks (PushGrammar term stk)
  _ -> error ""

__gotoTermForGrammar :: ([Lexeme], Pos) -> Term -> StackGrammar a -> Either (Pos, [String]) ([Text], Set Entity, [Rule])
__gotoTermForGrammar toks term stk@(state, _, _) = case state of
  SGrammar8 -> __runGrammar SGrammar30 toks (PushGrammar term stk)
  SGrammar10 -> __runGrammar SGrammar30 toks (PushGrammar term stk)
  SGrammar18 -> __runGrammar SGrammar30 toks (PushGrammar term stk)
  SGrammar31 -> __runGrammar SGrammar32 toks (PushGrammar term stk)
  SGrammar34 -> __runGrammar SGrammar30 toks (PushGrammar term stk)
  _ -> error ""
  
__runGrammar :: StGrammar a -> ([Lexeme], Pos) -> StackGrammar' a -> Either (Pos, [String]) ([Text], Set Entity, [Rule])
__runGrammar = \cases {
; SGrammar0 ((__p,  "add") : __input, __end) __stk ->
    __runGrammar SGrammar36 (__input, __end) (PushGrammar () (SGrammar0, __p, __stk))
; SGrammar2 ((__p,  "=>") : __input, __end) __stk ->
    __runGrammar SGrammar7 (__input, __end) (PushGrammar () (SGrammar2, __p, __stk))
; SGrammar3 ((__p,  "|") : __input, __end) __stk ->
    __runGrammar SGrammar8 (__input, __end) (PushGrammar () (SGrammar3, __p, __stk))
; SGrammar4 ((__p,  ":") : __input, __end) __stk ->
    __runGrammar SGrammar9 (__input, __end) (PushGrammar () (SGrammar4, __p, __stk))
; SGrammar4 ((__p,  "=") : __input, __end) __stk ->
    __runGrammar SGrammar10 (__input, __end) (PushGrammar () (SGrammar4, __p, __stk))
; SGrammar5 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar21 (__input, __end) (PushGrammar n (SGrammar5, __p, __stk))
; SGrammar6 ((__p,  "start") : __input, __end) __stk ->
    __runGrammar SGrammar22 (__input, __end) (PushGrammar () (SGrammar6, __p, __stk))
; SGrammar7 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar13 (__input, __end) (PushGrammar n (SGrammar7, __p, __stk))
; SGrammar8 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar29 (__input, __end) (PushGrammar n (SGrammar8, __p, __stk))
; SGrammar8 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar27 (__input, __end) (PushGrammar n (SGrammar8, __p, __stk))
; SGrammar9 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar15 (__input, __end) (PushGrammar n (SGrammar9, __p, __stk))
; SGrammar10 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar29 (__input, __end) (PushGrammar n (SGrammar10, __p, __stk))
; SGrammar10 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar27 (__input, __end) (PushGrammar n (SGrammar10, __p, __stk))
; SGrammar12 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar21 (__input, __end) (PushGrammar n (SGrammar12, __p, __stk))
; SGrammar15 ((__p,  "=") : __input, __end) __stk ->
    __runGrammar SGrammar18 (__input, __end) (PushGrammar () (SGrammar15, __p, __stk))
; SGrammar18 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar29 (__input, __end) (PushGrammar n (SGrammar18, __p, __stk))
; SGrammar18 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar27 (__input, __end) (PushGrammar n (SGrammar18, __p, __stk))
; SGrammar22 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar20 (__input, __end) (PushGrammar n (SGrammar22, __p, __stk))
; SGrammar23 ((__p,  ",") : __input, __end) __stk ->
    __runGrammar SGrammar25 (__input, __end) (PushGrammar () (SGrammar23, __p, __stk))
; SGrammar25 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar20 (__input, __end) (PushGrammar n (SGrammar25, __p, __stk))
; SGrammar29 ((__p,  ":") : __input, __end) __stk ->
    __runGrammar SGrammar31 (__input, __end) (PushGrammar () (SGrammar29, __p, __stk))
; SGrammar31 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar28 (__input, __end) (PushGrammar n (SGrammar31, __p, __stk))
; SGrammar31 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar27 (__input, __end) (PushGrammar n (SGrammar31, __p, __stk))
; SGrammar34 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar29 (__input, __end) (PushGrammar n (SGrammar34, __p, __stk))
; SGrammar34 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar27 (__input, __end) (PushGrammar n (SGrammar34, __p, __stk))
; SGrammar36 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar37 (__input, __end) (PushGrammar n (SGrammar36, __p, __stk))
; SGrammar37 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar37 (__input, __end) (PushGrammar n (SGrammar37, __p, __stk))
-- lookahead Nothing, entity Grammar
; SGrammar1 ([], __end) ((PushGrammar res __stk@(_, __pos, _))) ->
    pure res
-- lookahead Nothing, entity Clauses
; SGrammar3 ([], __end) ((PushGrammar c __stk@(_, __pos, _))) ->
    __gotoClausesForGrammar ([], __end) (action34 __pos c) __stk
-- lookahead Just <Name>, entity Clauses
; SGrammar3 ((__p, UppercaseName tok) : __input, __end) ((PushGrammar c __stk@(_, __pos, _))) ->
    __gotoClausesForGrammar ((__p, UppercaseName tok) : __input, __end) (action34 __pos c) __stk
-- lookahead Nothing, entity Rules
; SGrammar5 ([], __end) ((PushGrammar r __stk@(_, __pos, _))) ->
    __gotoRulesForGrammar ([], __end) (action42 __pos r) __stk
-- lookahead Nothing, entity Rules
; SGrammar11 ([], __end) ((PushGrammar rs (PushGrammar' r __stk@(_, __pos, _)))) ->
    __gotoRulesForGrammar ([], __end) (action43 __pos r rs) __stk
-- lookahead Nothing, entity Clause
; SGrammar13 ([], __end) ((PushGrammar reducer (PushGrammar' _ (PushGrammar' points __stk@(_, __pos, _))))) ->
    __gotoClauseForGrammar ([], __end) (action30 __pos points
                                                       reducer) __stk
-- lookahead Just <Name>, entity Clause
; SGrammar13 ((__p, UppercaseName tok) : __input, __end) ((PushGrammar reducer (PushGrammar' _ (PushGrammar' points __stk@(_, __pos, _))))) ->
    __gotoClauseForGrammar ((__p, UppercaseName tok) : __input, __end) (action30 __pos points
                                                                                       reducer) __stk
-- lookahead Just |, entity Clause
; SGrammar13 ((__p,  "|") : __input, __end) ((PushGrammar reducer (PushGrammar' _ (PushGrammar' points __stk@(_, __pos, _))))) ->
    __gotoClauseForGrammar ((__p,  "|") : __input, __end) (action30 __pos points
                                                                          reducer) __stk
-- lookahead Nothing, entity Clauses
; SGrammar14 ([], __end) ((PushGrammar cs (PushGrammar' _ (PushGrammar' c __stk@(_, __pos, _))))) ->
    __gotoClausesForGrammar ([], __end) (action35 __pos c cs) __stk
-- lookahead Just <Name>, entity Clauses
; SGrammar14 ((__p, UppercaseName tok) : __input, __end) ((PushGrammar cs (PushGrammar' _ (PushGrammar' c __stk@(_, __pos, _))))) ->
    __gotoClausesForGrammar ((__p, UppercaseName tok) : __input, __end) (action35 __pos c
                                                                                        cs) __stk
-- lookahead Nothing, entity Rule
; SGrammar16 ([], __end) ((PushGrammar clauses (PushGrammar' _ (PushGrammar' entity __stk@(_, __pos, _))))) ->
    __gotoRuleForGrammar ([], __end) (action39 __pos entity
                                                     clauses) __stk
-- lookahead Just <Name>, entity Rule
; SGrammar16 ((__p, UppercaseName tok) : __input, __end) ((PushGrammar clauses (PushGrammar' _ (PushGrammar' entity __stk@(_, __pos, _))))) ->
    __gotoRuleForGrammar ((__p, UppercaseName tok) : __input, __end) (action39 __pos entity
                                                                                     clauses) __stk
-- lookahead Nothing, entity Grammar
; SGrammar17 ([], __end) ((PushGrammar r (PushGrammar' s (PushGrammar' a __stk@(_, __pos, _))))) ->
    __gotoGrammarForGrammar ([], __end) (action62 __pos a s r) __stk
-- lookahead Nothing, entity Rule
; SGrammar19 ([], __end) ((PushGrammar clauses (PushGrammar' _ (PushGrammar' type_ (PushGrammar' _ (PushGrammar' entity __stk@(_, __pos, _))))))) ->
    __gotoRuleForGrammar ([], __end) (action38 __pos entity type_
                                                     clauses) __stk
-- lookahead Just <Name>, entity Rule
; SGrammar19 ((__p, UppercaseName tok) : __input, __end) ((PushGrammar clauses (PushGrammar' _ (PushGrammar' type_ (PushGrammar' _ (PushGrammar' entity __stk@(_, __pos, _))))))) ->
    __gotoRuleForGrammar ((__p, UppercaseName tok) : __input, __end) (action38 __pos entity
                                                                                     type_
                                                                                     clauses) __stk
-- lookahead Just ,, entity Entity
; SGrammar20 ((__p,  ",") : __input, __end) ((PushGrammar e __stk@(_, __pos, _))) ->
    __gotoEntityForGrammar ((__p,  ",") : __input, __end) (action18 __pos e) __stk
-- lookahead Just <Name>, entity Entity
; SGrammar20 ((__p, UppercaseName tok) : __input, __end) ((PushGrammar e __stk@(_, __pos, _))) ->
    __gotoEntityForGrammar ((__p, UppercaseName tok) : __input, __end) (action18 __pos e) __stk
-- lookahead Just :, entity Entity
; SGrammar21 ((__p,  ":") : __input, __end) ((PushGrammar e __stk@(_, __pos, _))) ->
    __gotoEntityForGrammar ((__p,  ":") : __input, __end) (action18 __pos e) __stk
-- lookahead Just =, entity Entity
; SGrammar21 ((__p,  "=") : __input, __end) ((PushGrammar e __stk@(_, __pos, _))) ->
    __gotoEntityForGrammar ((__p,  "=") : __input, __end) (action18 __pos e) __stk
-- lookahead Just <Name>, entity Entities
; SGrammar23 ((__p, UppercaseName tok) : __input, __end) ((PushGrammar e __stk@(_, __pos, _))) ->
    __gotoEntitiesForGrammar ((__p, UppercaseName tok) : __input, __end) (action57 __pos e) __stk
-- lookahead Just <Name>, entity Starters
; SGrammar24 ((__p, UppercaseName tok) : __input, __end) ((PushGrammar es (PushGrammar' _ __stk@(_, __pos, _)))) ->
    __gotoStartersForGrammar ((__p, UppercaseName tok) : __input, __end) (action54 __pos es) __stk
-- lookahead Just <Name>, entity Entities
; SGrammar26 ((__p, UppercaseName tok) : __input, __end) ((PushGrammar es (PushGrammar' _ (PushGrammar' e __stk@(_, __pos, _))))) ->
    __gotoEntitiesForGrammar ((__p, UppercaseName tok) : __input, __end) (action58 __pos e
                                                                                         es) __stk
-- lookahead Just <name>, entity Term
; SGrammar27 ((__p, LowercaseName tok) : __input, __end) ((PushGrammar t __stk@(_, __pos, _))) ->
    __gotoTermForGrammar ((__p, LowercaseName tok) : __input, __end) (action17 __pos t) __stk
-- lookahead Just <str>, entity Term
; SGrammar27 ((__p, StringLiteral tok) : __input, __end) ((PushGrammar t __stk@(_, __pos, _))) ->
    __gotoTermForGrammar ((__p, StringLiteral tok) : __input, __end) (action17 __pos t) __stk
-- lookahead Just =>, entity Term
; SGrammar27 ((__p,  "=>") : __input, __end) ((PushGrammar t __stk@(_, __pos, _))) ->
    __gotoTermForGrammar ((__p,  "=>") : __input, __end) (action17 __pos t) __stk
-- lookahead Just <name>, entity Entity
; SGrammar28 ((__p, LowercaseName tok) : __input, __end) ((PushGrammar e __stk@(_, __pos, _))) ->
    __gotoEntityForGrammar ((__p, LowercaseName tok) : __input, __end) (action18 __pos e) __stk
-- lookahead Just <str>, entity Entity
; SGrammar28 ((__p, StringLiteral tok) : __input, __end) ((PushGrammar e __stk@(_, __pos, _))) ->
    __gotoEntityForGrammar ((__p, StringLiteral tok) : __input, __end) (action18 __pos e) __stk
-- lookahead Just =>, entity Entity
; SGrammar28 ((__p,  "=>") : __input, __end) ((PushGrammar e __stk@(_, __pos, _))) ->
    __gotoEntityForGrammar ((__p,  "=>") : __input, __end) (action18 __pos e) __stk
-- lookahead Just <name>, entity Point
; SGrammar30 ((__p, LowercaseName tok) : __input, __end) ((PushGrammar t __stk@(_, __pos, _))) ->
    __gotoPointForGrammar ((__p, LowercaseName tok) : __input, __end) (action23 __pos t) __stk
-- lookahead Just <str>, entity Point
; SGrammar30 ((__p, StringLiteral tok) : __input, __end) ((PushGrammar t __stk@(_, __pos, _))) ->
    __gotoPointForGrammar ((__p, StringLiteral tok) : __input, __end) (action23 __pos t) __stk
-- lookahead Just =>, entity Point
; SGrammar30 ((__p,  "=>") : __input, __end) ((PushGrammar t __stk@(_, __pos, _))) ->
    __gotoPointForGrammar ((__p,  "=>") : __input, __end) (action23 __pos t) __stk
-- lookahead Just <name>, entity Point
; SGrammar32 ((__p, LowercaseName tok) : __input, __end) ((PushGrammar t (PushGrammar' _ (PushGrammar' n __stk@(_, __pos, _))))) ->
    __gotoPointForGrammar ((__p, LowercaseName tok) : __input, __end) (action21 __pos n
                                                                                      t) __stk
-- lookahead Just <str>, entity Point
; SGrammar32 ((__p, StringLiteral tok) : __input, __end) ((PushGrammar t (PushGrammar' _ (PushGrammar' n __stk@(_, __pos, _))))) ->
    __gotoPointForGrammar ((__p, StringLiteral tok) : __input, __end) (action21 __pos n
                                                                                      t) __stk
-- lookahead Just =>, entity Point
; SGrammar32 ((__p,  "=>") : __input, __end) ((PushGrammar t (PushGrammar' _ (PushGrammar' n __stk@(_, __pos, _))))) ->
    __gotoPointForGrammar ((__p,  "=>") : __input, __end) (action21 __pos n
                                                                          t) __stk
-- lookahead Just <name>, entity Point
; SGrammar33 ((__p, LowercaseName tok) : __input, __end) ((PushGrammar e (PushGrammar' _ (PushGrammar' n __stk@(_, __pos, _))))) ->
    __gotoPointForGrammar ((__p, LowercaseName tok) : __input, __end) (action22 __pos n
                                                                                      e) __stk
-- lookahead Just <str>, entity Point
; SGrammar33 ((__p, StringLiteral tok) : __input, __end) ((PushGrammar e (PushGrammar' _ (PushGrammar' n __stk@(_, __pos, _))))) ->
    __gotoPointForGrammar ((__p, StringLiteral tok) : __input, __end) (action22 __pos n
                                                                                      e) __stk
-- lookahead Just =>, entity Point
; SGrammar33 ((__p,  "=>") : __input, __end) ((PushGrammar e (PushGrammar' _ (PushGrammar' n __stk@(_, __pos, _))))) ->
    __gotoPointForGrammar ((__p,  "=>") : __input, __end) (action22 __pos n
                                                                          e) __stk
-- lookahead Just =>, entity Points
; SGrammar34 ((__p,  "=>") : __input, __end) ((PushGrammar p __stk@(_, __pos, _))) ->
    __gotoPointsForGrammar ((__p,  "=>") : __input, __end) (action26 __pos p) __stk
-- lookahead Just =>, entity Points
; SGrammar35 ((__p,  "=>") : __input, __end) ((PushGrammar ps (PushGrammar' p __stk@(_, __pos, _)))) ->
    __gotoPointsForGrammar ((__p,  "=>") : __input, __end) (action27 __pos p
                                                                           ps) __stk
-- lookahead Just start, entity Additions
; SGrammar36 ((__p,  "start") : __input, __end) ((PushGrammar _ __stk@(_, __pos, _))) ->
    __gotoAdditionsForGrammar ((__p,  "start") : __input, __end) (action46 __pos ) __stk
-- lookahead Just start, entity Lines
; SGrammar37 ((__p,  "start") : __input, __end) ((PushGrammar t __stk@(_, __pos, _))) ->
    __gotoLinesForGrammar ((__p,  "start") : __input, __end) (action50 __pos t) __stk
-- lookahead Just start, entity Additions
; SGrammar38 ((__p,  "start") : __input, __end) ((PushGrammar ls (PushGrammar' _ __stk@(_, __pos, _)))) ->
    __gotoAdditionsForGrammar ((__p,  "start") : __input, __end) (action47 __pos ls) __stk
-- lookahead Just start, entity Lines
; SGrammar39 ((__p,  "start") : __input, __end) ((PushGrammar ts (PushGrammar' t __stk@(_, __pos, _)))) ->
    __gotoLinesForGrammar ((__p,  "start") : __input, __end) (action51 __pos t
                                                                             ts) __stk
; SGrammar0 __input _ -> Left  (currentPos __input, ["add"])
; SGrammar1 __input _ -> Left  (currentPos __input, ["<eof>"])
; SGrammar2 __input _ -> Left  (currentPos __input, ["=>"])
; SGrammar3 __input _ ->
    Left  (currentPos __input, ["<eof>", "<Name>", "|"])
; SGrammar4 __input _ -> Left  (currentPos __input, [":", "="])
; SGrammar5 __input _ ->
    Left  (currentPos __input, ["<eof>", "<Name>"])
; SGrammar6 __input _ -> Left  (currentPos __input, ["start"])
; SGrammar7 __input _ -> Left  (currentPos __input, ["<str>"])
; SGrammar8 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>"])
; SGrammar9 __input _ -> Left  (currentPos __input, ["<str>"])
; SGrammar10 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>"])
; SGrammar11 __input _ -> Left  (currentPos __input, ["<eof>"])
; SGrammar12 __input _ -> Left  (currentPos __input, ["<Name>"])
; SGrammar13 __input _ ->
    Left  (currentPos __input, ["<eof>", "<Name>", "|"])
; SGrammar14 __input _ ->
    Left  (currentPos __input, ["<eof>", "<Name>"])
; SGrammar15 __input _ -> Left  (currentPos __input, ["="])
; SGrammar16 __input _ ->
    Left  (currentPos __input, ["<eof>", "<Name>"])
; SGrammar17 __input _ -> Left  (currentPos __input, ["<eof>"])
; SGrammar18 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>"])
; SGrammar19 __input _ ->
    Left  (currentPos __input, ["<eof>", "<Name>"])
; SGrammar20 __input _ ->
    Left  (currentPos __input, [",", "<Name>"])
; SGrammar21 __input _ -> Left  (currentPos __input, [":", "="])
; SGrammar22 __input _ -> Left  (currentPos __input, ["<Name>"])
; SGrammar23 __input _ ->
    Left  (currentPos __input, [",", "<Name>"])
; SGrammar24 __input _ -> Left  (currentPos __input, ["<Name>"])
; SGrammar25 __input _ -> Left  (currentPos __input, ["<Name>"])
; SGrammar26 __input _ -> Left  (currentPos __input, ["<Name>"])
; SGrammar27 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>", "=>"])
; SGrammar28 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>", "=>"])
; SGrammar29 __input _ -> Left  (currentPos __input, [":"])
; SGrammar30 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>", "=>"])
; SGrammar31 __input _ ->
    Left  (currentPos __input, ["<Name>", "<str>"])
; SGrammar32 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>", "=>"])
; SGrammar33 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>", "=>"])
; SGrammar34 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>", "=>"])
; SGrammar35 __input _ -> Left  (currentPos __input, ["=>"])
; SGrammar36 __input _ ->
    Left  (currentPos __input, ["<str>", "start"])
; SGrammar37 __input _ ->
    Left  (currentPos __input, ["<str>", "start"])
; SGrammar38 __input _ -> Left  (currentPos __input, ["start"])
; SGrammar39 __input _ -> Left  (currentPos __input, ["start"])
} where {
; action17 pos t =
         Term   t
; action18 pos e =
         Entity e
; action21 pos n t =
    T (Just n) t
; action22 pos n e =
    E (Just n) e
; action23 pos t =
                   T  Nothing t
; action26 pos p =
    [p]
; action27 pos p ps =
     p : ps
; action30 pos points reducer =
    Clause {mark = 0, pos, points = listToArray points, reducer}
; action34 pos c =
    [c]
; action35 pos c cs =
     c : cs
; action38 pos entity type_ clauses =
    Rule {entity, type_ = Just type_, clauses}
; action39 pos entity clauses =
    Rule {entity, type_ = Nothing,    clauses}
; action42 pos r =
    [r]
; action43 pos r rs =
     r : rs
; action46 pos =
    []
; action47 pos ls =
    ls
; action50 pos t =
    [t]
; action51 pos t ts =
     t : ts
; action54 pos es =
    es
; action57 pos e =
    [e]
; action58 pos e es =
     e : es
; action62 pos a s r =
    (a, Set.fromList s, r)
}
parseGrammar :: FilePath -> IO (Either LexerError (Either (Pos, [String]) ([Text], Set Entity, [Rule])))
parseGrammar filepath = do
  text <- Text.readFile filepath
  case lexText filepath text [",", ":", "=", "=>", "add", "start",
                              "|"] of
    Left  err   -> pure (Left err)
    Right input -> pure (Right (__runGrammar SGrammar0 input NilGrammar))
  
currentPos :: ([Lexeme], Pos) -> Pos
currentPos = \case
  ([],           end) -> end
  ((pos, _) : _, _)   -> pos
  