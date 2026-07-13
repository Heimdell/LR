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
import Symbol
import Text.Lexer.Default
import Data.Text.Position
import Data.Text (Text)
import Data.Lexeme
import Data.Array
import Data.Set qualified as Set
import Data.Set (Set)
listToArray :: [a] -> Array Int a
listToArray as = Data.Array.listArray (0, length as - 1) as

type Stack  st xs = (st xs, Pos, Stack' st xs)
data Stack' st xs where
  Nil  ::                     Stack' st '[]
  (:>) :: x -> Stack st xs -> Stack' st (x : xs)


data StGrammar :: [Kind.Type] -> Kind.Type where
  SGrammar0 :: StGrammar (() : a)
  SGrammar1 :: StGrammar (() : NonTerminal : a)
  SGrammar2 :: StGrammar (Text : a)
  SGrammar3 :: StGrammar (Rule : a)
  SGrammar4 :: StGrammar ([NonTerminal] : [Text] : a)
  SGrammar5 :: StGrammar (Text : a)
  SGrammar6 :: StGrammar (() : Clause : a)
  SGrammar7 :: StGrammar (() : NonTerminal : a)
  SGrammar8 :: StGrammar (() : Text : () : NonTerminal : a)
  SGrammar9 :: StGrammar ([Text] : a)
  SGrammar10 :: StGrammar ([Symbol] : a)
  SGrammar11 :: StGrammar (Clause : a)
  SGrammar12 :: StGrammar (NonTerminal : a)
  SGrammar13 :: StGrammar (NonTerminal : a)
  SGrammar14 :: StGrammar (() : [Symbol] : a)
  SGrammar15 :: StGrammar (() : NonTerminal : a)
  SGrammar16 :: StGrammar ([NonTerminal] : () : a)
  SGrammar17 :: StGrammar (Text : () : [Symbol] : a)
  SGrammar18 :: StGrammar ([Clause] : () : Clause : a)
  SGrammar19 :: StGrammar (Text : () : NonTerminal : a)
  SGrammar20 :: StGrammar ([Clause] : () : NonTerminal : a)
  SGrammar21 :: StGrammar ([NonTerminal] : () : NonTerminal : a)
  SGrammar22 :: StGrammar ([Clause] : () : Text : () : NonTerminal :
                           a)
  SGrammar23 :: StGrammar (() : Text : a)
  SGrammar24 :: StGrammar (Symbol : a)
  SGrammar25 :: StGrammar (Text : a)
  SGrammar26 :: StGrammar (Text : a)
  SGrammar27 :: StGrammar (Text : a)
  SGrammar28 :: StGrammar (Terminal : a)
  SGrammar29 :: StGrammar (Terminal : () : Text : a)
  SGrammar30 :: StGrammar (NonTerminal : () : Text : a)
  SGrammar31 :: StGrammar ([Symbol] : Symbol : a)
  SGrammar32 :: StGrammar (a)
  SGrammar33 :: StGrammar (() : a)
  SGrammar34 :: StGrammar (Text : a)
  SGrammar35 :: StGrammar ([Text] : () : a)
  SGrammar36 :: StGrammar ([Text] : Text : a)
  SGrammar37 :: StGrammar (([Text], Set NonTerminal, [Rule]) : a)
  SGrammar38 :: StGrammar ([Rule] : Rule : a)
  SGrammar39 :: StGrammar ([Rule] : [NonTerminal] : [Text] : a)

__gotoAdditionsForGrammar :: ([Lexeme], Pos) -> [Text] -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoAdditionsForGrammar toks term stk@(state, _, _) = case state of
  SGrammar32 -> __runGrammar SGrammar9 toks (term :> stk)
  _ -> error ""

__gotoClauseForGrammar :: ([Lexeme], Pos) -> Clause -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoClauseForGrammar toks term stk@(state, _, _) = case state of
  SGrammar6 -> __runGrammar SGrammar11 toks (term :> stk)
  SGrammar7 -> __runGrammar SGrammar11 toks (term :> stk)
  SGrammar8 -> __runGrammar SGrammar11 toks (term :> stk)
  _ -> error ""

__gotoClausesForGrammar :: ([Lexeme], Pos) -> [Clause] -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoClausesForGrammar toks term stk@(state, _, _) = case state of
  SGrammar6 -> __runGrammar SGrammar18 toks (term :> stk)
  SGrammar7 -> __runGrammar SGrammar20 toks (term :> stk)
  SGrammar8 -> __runGrammar SGrammar22 toks (term :> stk)
  _ -> error ""

__gotoGrammarForGrammar :: ([Lexeme], Pos) -> ([Text], Set NonTerminal, [Rule]) -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoGrammarForGrammar toks term stk@(state, _, _) = case state of
  SGrammar32 -> __runGrammar SGrammar37 toks (term :> stk)
  _ -> error ""

__gotoLinesForGrammar :: ([Lexeme], Pos) -> [Text] -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoLinesForGrammar toks term stk@(state, _, _) = case state of
  SGrammar33 -> __runGrammar SGrammar35 toks (term :> stk)
  SGrammar34 -> __runGrammar SGrammar36 toks (term :> stk)
  _ -> error ""

__gotoNonTerminalForGrammar :: ([Lexeme], Pos) -> NonTerminal -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoNonTerminalForGrammar toks term stk@(state, _, _) = case state of
  SGrammar0 -> __runGrammar SGrammar13 toks (term :> stk)
  SGrammar1 -> __runGrammar SGrammar13 toks (term :> stk)
  SGrammar3 -> __runGrammar SGrammar12 toks (term :> stk)
  SGrammar4 -> __runGrammar SGrammar12 toks (term :> stk)
  SGrammar23 -> __runGrammar SGrammar30 toks (term :> stk)
  _ -> error ""

__gotoNonTerminalsForGrammar :: ([Lexeme], Pos) -> [NonTerminal] -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoNonTerminalsForGrammar toks term stk@(state, _, _) = case state of
  SGrammar0 -> __runGrammar SGrammar16 toks (term :> stk)
  SGrammar1 -> __runGrammar SGrammar21 toks (term :> stk)
  _ -> error ""

__gotoRuleForGrammar :: ([Lexeme], Pos) -> Rule -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoRuleForGrammar toks term stk@(state, _, _) = case state of
  SGrammar3 -> __runGrammar SGrammar3 toks (term :> stk)
  SGrammar4 -> __runGrammar SGrammar3 toks (term :> stk)
  _ -> error ""

__gotoRulesForGrammar :: ([Lexeme], Pos) -> [Rule] -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoRulesForGrammar toks term stk@(state, _, _) = case state of
  SGrammar3 -> __runGrammar SGrammar38 toks (term :> stk)
  SGrammar4 -> __runGrammar SGrammar39 toks (term :> stk)
  _ -> error ""

__gotoStartersForGrammar :: ([Lexeme], Pos) -> [NonTerminal] -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoStartersForGrammar toks term stk@(state, _, _) = case state of
  SGrammar9 -> __runGrammar SGrammar4 toks (term :> stk)
  _ -> error ""

__gotoSymbolForGrammar :: ([Lexeme], Pos) -> Symbol -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoSymbolForGrammar toks term stk@(state, _, _) = case state of
  SGrammar6 -> __runGrammar SGrammar24 toks (term :> stk)
  SGrammar7 -> __runGrammar SGrammar24 toks (term :> stk)
  SGrammar8 -> __runGrammar SGrammar24 toks (term :> stk)
  SGrammar24 -> __runGrammar SGrammar24 toks (term :> stk)
  _ -> error ""

__gotoSymbolsForGrammar :: ([Lexeme], Pos) -> [Symbol] -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoSymbolsForGrammar toks term stk@(state, _, _) = case state of
  SGrammar6 -> __runGrammar SGrammar10 toks (term :> stk)
  SGrammar7 -> __runGrammar SGrammar10 toks (term :> stk)
  SGrammar8 -> __runGrammar SGrammar10 toks (term :> stk)
  SGrammar24 -> __runGrammar SGrammar31 toks (term :> stk)
  _ -> error ""

__gotoTerminalForGrammar :: ([Lexeme], Pos) -> Terminal -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoTerminalForGrammar toks term stk@(state, _, _) = case state of
  SGrammar6 -> __runGrammar SGrammar28 toks (term :> stk)
  SGrammar7 -> __runGrammar SGrammar28 toks (term :> stk)
  SGrammar8 -> __runGrammar SGrammar28 toks (term :> stk)
  SGrammar23 -> __runGrammar SGrammar29 toks (term :> stk)
  SGrammar24 -> __runGrammar SGrammar28 toks (term :> stk)
  _ -> error ""

__runGrammar :: StGrammar a -> ([Lexeme], Pos) -> Stack' StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__runGrammar = \cases {
; SGrammar0 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar2 (__input, __end) (n :> (SGrammar0, __p, __stk))
; SGrammar1 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar2 (__input, __end) (n :> (SGrammar1, __p, __stk))
; SGrammar3 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar5 (__input, __end) (n :> (SGrammar3, __p, __stk))
; SGrammar4 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar5 (__input, __end) (n :> (SGrammar4, __p, __stk))
; SGrammar6 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar27 (__input, __end) (n :> (SGrammar6, __p, __stk))
; SGrammar6 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar25 (__input, __end) (n :> (SGrammar6, __p, __stk))
; SGrammar7 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar27 (__input, __end) (n :> (SGrammar7, __p, __stk))
; SGrammar7 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar25 (__input, __end) (n :> (SGrammar7, __p, __stk))
; SGrammar8 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar27 (__input, __end) (n :> (SGrammar8, __p, __stk))
; SGrammar8 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar25 (__input, __end) (n :> (SGrammar8, __p, __stk))
; SGrammar9 ((__p,  "start") : __input, __end) __stk ->
    __runGrammar SGrammar0 (__input, __end) (() :> (SGrammar9, __p, __stk))
; SGrammar10 ((__p,  "=>") : __input, __end) __stk ->
    __runGrammar SGrammar14 (__input, __end) (() :> (SGrammar10, __p, __stk))
; SGrammar11 ((__p,  "|") : __input, __end) __stk ->
    __runGrammar SGrammar6 (__input, __end) (() :> (SGrammar11, __p, __stk))
; SGrammar12 ((__p,  ":") : __input, __end) __stk ->
    __runGrammar SGrammar15 (__input, __end) (() :> (SGrammar12, __p, __stk))
; SGrammar12 ((__p,  "=") : __input, __end) __stk ->
    __runGrammar SGrammar7 (__input, __end) (() :> (SGrammar12, __p, __stk))
; SGrammar13 ((__p,  ",") : __input, __end) __stk ->
    __runGrammar SGrammar1 (__input, __end) (() :> (SGrammar13, __p, __stk))
; SGrammar14 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar17 (__input, __end) (n :> (SGrammar14, __p, __stk))
; SGrammar15 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar19 (__input, __end) (n :> (SGrammar15, __p, __stk))
; SGrammar19 ((__p,  "=") : __input, __end) __stk ->
    __runGrammar SGrammar8 (__input, __end) (() :> (SGrammar19, __p, __stk))
; SGrammar23 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar26 (__input, __end) (n :> (SGrammar23, __p, __stk))
; SGrammar23 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar25 (__input, __end) (n :> (SGrammar23, __p, __stk))
; SGrammar24 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar27 (__input, __end) (n :> (SGrammar24, __p, __stk))
; SGrammar24 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar25 (__input, __end) (n :> (SGrammar24, __p, __stk))
; SGrammar27 ((__p,  ":") : __input, __end) __stk ->
    __runGrammar SGrammar23 (__input, __end) (() :> (SGrammar27, __p, __stk))
; SGrammar32 ((__p,  "add") : __input, __end) __stk ->
    __runGrammar SGrammar33 (__input, __end) (() :> (SGrammar32, __p, __stk))
; SGrammar33 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar34 (__input, __end) (n :> (SGrammar33, __p, __stk))
; SGrammar34 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar34 (__input, __end) (n :> (SGrammar34, __p, __stk))
-- lookahead ,, entity NonTerminal
; SGrammar2 ((__p,  ",") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoNonTerminalForGrammar ((__p,  ",") : __input, __end) (action18 __pos e) __stk
-- lookahead <Name>, entity NonTerminal
; SGrammar2 ((__p, UppercaseName tok) : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoNonTerminalForGrammar ((__p, UppercaseName tok) : __input, __end) (action18 __pos e) __stk
-- lookahead <eof>, entity Rules
; SGrammar3 ([], __end) ((r :> __stk@(_, __pos, _))) ->
    __gotoRulesForGrammar ([], __end) (action42 __pos r) __stk
-- lookahead :, entity NonTerminal
; SGrammar5 ((__p,  ":") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoNonTerminalForGrammar ((__p,  ":") : __input, __end) (action18 __pos e) __stk
-- lookahead =, entity NonTerminal
; SGrammar5 ((__p,  "=") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoNonTerminalForGrammar ((__p,  "=") : __input, __end) (action18 __pos e) __stk
-- lookahead <Name>, entity Clauses
; SGrammar11 ((__p, UppercaseName tok) : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoClausesForGrammar ((__p, UppercaseName tok) : __input, __end) (action34 __pos c) __stk
-- lookahead <eof>, entity Clauses
; SGrammar11 ([], __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoClausesForGrammar ([], __end) (action34 __pos c) __stk
-- lookahead <Name>, entity NonTerminals
; SGrammar13 ((__p, UppercaseName tok) : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoNonTerminalsForGrammar ((__p, UppercaseName tok) : __input, __end) (action57 __pos e) __stk
-- lookahead <Name>, entity Starters
; SGrammar16 ((__p, UppercaseName tok) : __input, __end) ((es :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoStartersForGrammar ((__p, UppercaseName tok) : __input, __end) (action54 __pos es) __stk
-- lookahead <Name>, entity Clause
; SGrammar17 ((__p, UppercaseName tok) : __input, __end) ((reducer :> (_, _, _ :> (_, _, points :> __stk@(_, __pos, _))))) ->
    __gotoClauseForGrammar ((__p, UppercaseName tok) : __input, __end) (action30 __pos points
                                                                                       reducer) __stk
-- lookahead |, entity Clause
; SGrammar17 ((__p,  "|") : __input, __end) ((reducer :> (_, _, _ :> (_, _, points :> __stk@(_, __pos, _))))) ->
    __gotoClauseForGrammar ((__p,  "|") : __input, __end) (action30 __pos points
                                                                          reducer) __stk
-- lookahead <eof>, entity Clause
; SGrammar17 ([], __end) ((reducer :> (_, _, _ :> (_, _, points :> __stk@(_, __pos, _))))) ->
    __gotoClauseForGrammar ([], __end) (action30 __pos points
                                                       reducer) __stk
-- lookahead <Name>, entity Clauses
; SGrammar18 ((__p, UppercaseName tok) : __input, __end) ((cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _))))) ->
    __gotoClausesForGrammar ((__p, UppercaseName tok) : __input, __end) (action35 __pos c
                                                                                        cs) __stk
-- lookahead <eof>, entity Clauses
; SGrammar18 ([], __end) ((cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _))))) ->
    __gotoClausesForGrammar ([], __end) (action35 __pos c cs) __stk
-- lookahead <Name>, entity Rule
; SGrammar20 ((__p, UppercaseName tok) : __input, __end) ((clauses :> (_, _, _ :> (_, _, entity :> __stk@(_, __pos, _))))) ->
    __gotoRuleForGrammar ((__p, UppercaseName tok) : __input, __end) (action39 __pos entity
                                                                                     clauses) __stk
-- lookahead <eof>, entity Rule
; SGrammar20 ([], __end) ((clauses :> (_, _, _ :> (_, _, entity :> __stk@(_, __pos, _))))) ->
    __gotoRuleForGrammar ([], __end) (action39 __pos entity
                                                     clauses) __stk
-- lookahead <Name>, entity NonTerminals
; SGrammar21 ((__p, UppercaseName tok) : __input, __end) ((es :> (_, _, _ :> (_, _, e :> __stk@(_, __pos, _))))) ->
    __gotoNonTerminalsForGrammar ((__p, UppercaseName tok) : __input, __end) (action58 __pos e
                                                                                             es) __stk
-- lookahead <Name>, entity Rule
; SGrammar22 ((__p, UppercaseName tok) : __input, __end) ((clauses :> (_, _, _ :> (_, _, type_ :> (_, _, _ :> (_, _, entity :> __stk@(_, __pos, _))))))) ->
    __gotoRuleForGrammar ((__p, UppercaseName tok) : __input, __end) (action38 __pos entity
                                                                                     type_
                                                                                     clauses) __stk
-- lookahead <eof>, entity Rule
; SGrammar22 ([], __end) ((clauses :> (_, _, _ :> (_, _, type_ :> (_, _, _ :> (_, _, entity :> __stk@(_, __pos, _))))))) ->
    __gotoRuleForGrammar ([], __end) (action38 __pos entity type_
                                                     clauses) __stk
-- lookahead =>, entity Symbols
; SGrammar24 ((__p,  "=>") : __input, __end) ((p :> __stk@(_, __pos, _))) ->
    __gotoSymbolsForGrammar ((__p,  "=>") : __input, __end) (action26 __pos p) __stk
-- lookahead <name>, entity Terminal
; SGrammar25 ((__p, LowercaseName tok) : __input, __end) ((t :> __stk@(_, __pos, _))) ->
    __gotoTerminalForGrammar ((__p, LowercaseName tok) : __input, __end) (action17 __pos t) __stk
-- lookahead <str>, entity Terminal
; SGrammar25 ((__p, StringLiteral tok) : __input, __end) ((t :> __stk@(_, __pos, _))) ->
    __gotoTerminalForGrammar ((__p, StringLiteral tok) : __input, __end) (action17 __pos t) __stk
-- lookahead =>, entity Terminal
; SGrammar25 ((__p,  "=>") : __input, __end) ((t :> __stk@(_, __pos, _))) ->
    __gotoTerminalForGrammar ((__p,  "=>") : __input, __end) (action17 __pos t) __stk
-- lookahead <name>, entity NonTerminal
; SGrammar26 ((__p, LowercaseName tok) : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoNonTerminalForGrammar ((__p, LowercaseName tok) : __input, __end) (action18 __pos e) __stk
-- lookahead <str>, entity NonTerminal
; SGrammar26 ((__p, StringLiteral tok) : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoNonTerminalForGrammar ((__p, StringLiteral tok) : __input, __end) (action18 __pos e) __stk
-- lookahead =>, entity NonTerminal
; SGrammar26 ((__p,  "=>") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoNonTerminalForGrammar ((__p,  "=>") : __input, __end) (action18 __pos e) __stk
-- lookahead <name>, entity Symbol
; SGrammar28 ((__p, LowercaseName tok) : __input, __end) ((t :> __stk@(_, __pos, _))) ->
    __gotoSymbolForGrammar ((__p, LowercaseName tok) : __input, __end) (action23 __pos t) __stk
-- lookahead <str>, entity Symbol
; SGrammar28 ((__p, StringLiteral tok) : __input, __end) ((t :> __stk@(_, __pos, _))) ->
    __gotoSymbolForGrammar ((__p, StringLiteral tok) : __input, __end) (action23 __pos t) __stk
-- lookahead =>, entity Symbol
; SGrammar28 ((__p,  "=>") : __input, __end) ((t :> __stk@(_, __pos, _))) ->
    __gotoSymbolForGrammar ((__p,  "=>") : __input, __end) (action23 __pos t) __stk
-- lookahead <name>, entity Symbol
; SGrammar29 ((__p, LowercaseName tok) : __input, __end) ((t :> (_, _, _ :> (_, _, n :> __stk@(_, __pos, _))))) ->
    __gotoSymbolForGrammar ((__p, LowercaseName tok) : __input, __end) (action21 __pos n
                                                                                       t) __stk
-- lookahead <str>, entity Symbol
; SGrammar29 ((__p, StringLiteral tok) : __input, __end) ((t :> (_, _, _ :> (_, _, n :> __stk@(_, __pos, _))))) ->
    __gotoSymbolForGrammar ((__p, StringLiteral tok) : __input, __end) (action21 __pos n
                                                                                       t) __stk
-- lookahead =>, entity Symbol
; SGrammar29 ((__p,  "=>") : __input, __end) ((t :> (_, _, _ :> (_, _, n :> __stk@(_, __pos, _))))) ->
    __gotoSymbolForGrammar ((__p,  "=>") : __input, __end) (action21 __pos n
                                                                           t) __stk
-- lookahead <name>, entity Symbol
; SGrammar30 ((__p, LowercaseName tok) : __input, __end) ((e :> (_, _, _ :> (_, _, n :> __stk@(_, __pos, _))))) ->
    __gotoSymbolForGrammar ((__p, LowercaseName tok) : __input, __end) (action22 __pos n
                                                                                       e) __stk
-- lookahead <str>, entity Symbol
; SGrammar30 ((__p, StringLiteral tok) : __input, __end) ((e :> (_, _, _ :> (_, _, n :> __stk@(_, __pos, _))))) ->
    __gotoSymbolForGrammar ((__p, StringLiteral tok) : __input, __end) (action22 __pos n
                                                                                       e) __stk
-- lookahead =>, entity Symbol
; SGrammar30 ((__p,  "=>") : __input, __end) ((e :> (_, _, _ :> (_, _, n :> __stk@(_, __pos, _))))) ->
    __gotoSymbolForGrammar ((__p,  "=>") : __input, __end) (action22 __pos n
                                                                           e) __stk
-- lookahead =>, entity Symbols
; SGrammar31 ((__p,  "=>") : __input, __end) ((ps :> (_, _, p :> __stk@(_, __pos, _)))) ->
    __gotoSymbolsForGrammar ((__p,  "=>") : __input, __end) (action27 __pos p
                                                                            ps) __stk
-- lookahead start, entity Additions
; SGrammar33 ((__p,  "start") : __input, __end) ((_ :> __stk@(_, __pos, _))) ->
    __gotoAdditionsForGrammar ((__p,  "start") : __input, __end) (action46 __pos ) __stk
-- lookahead start, entity Lines
; SGrammar34 ((__p,  "start") : __input, __end) ((t :> __stk@(_, __pos, _))) ->
    __gotoLinesForGrammar ((__p,  "start") : __input, __end) (action50 __pos t) __stk
-- lookahead start, entity Additions
; SGrammar35 ((__p,  "start") : __input, __end) ((ls :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoAdditionsForGrammar ((__p,  "start") : __input, __end) (action47 __pos ls) __stk
-- lookahead start, entity Lines
; SGrammar36 ((__p,  "start") : __input, __end) ((ts :> (_, _, t :> __stk@(_, __pos, _)))) ->
    __gotoLinesForGrammar ((__p,  "start") : __input, __end) (action51 __pos t
                                                                             ts) __stk
-- lookahead <eof>, entity Grammar
; SGrammar37 ([], __end) ((res :> __stk@(_, __pos, _))) -> pure res
-- lookahead <eof>, entity Rules
; SGrammar38 ([], __end) ((rs :> (_, _, r :> __stk@(_, __pos, _)))) ->
    __gotoRulesForGrammar ([], __end) (action43 __pos r rs) __stk
-- lookahead <eof>, entity Grammar
; SGrammar39 ([], __end) ((r :> (_, _, s :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoGrammarForGrammar ([], __end) (action62 __pos a s r) __stk
; SGrammar0 __input _ -> Left  (currentPos __input, ["<Name>"])
; SGrammar1 __input _ -> Left  (currentPos __input, ["<Name>"])
; SGrammar2 __input _ ->
    Left  (currentPos __input, [",", "<Name>"])
; SGrammar3 __input _ ->
    Left  (currentPos __input, ["<Name>", "<eof>"])
; SGrammar4 __input _ -> Left  (currentPos __input, ["<Name>"])
; SGrammar5 __input _ -> Left  (currentPos __input, [":", "="])
; SGrammar6 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>"])
; SGrammar7 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>"])
; SGrammar8 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>"])
; SGrammar9 __input _ -> Left  (currentPos __input, ["start"])
; SGrammar10 __input _ -> Left  (currentPos __input, ["=>"])
; SGrammar11 __input _ ->
    Left  (currentPos __input, ["<Name>", "|", "<eof>"])
; SGrammar12 __input _ -> Left  (currentPos __input, [":", "="])
; SGrammar13 __input _ ->
    Left  (currentPos __input, [",", "<Name>"])
; SGrammar14 __input _ -> Left  (currentPos __input, ["<str>"])
; SGrammar15 __input _ -> Left  (currentPos __input, ["<str>"])
; SGrammar16 __input _ -> Left  (currentPos __input, ["<Name>"])
; SGrammar17 __input _ ->
    Left  (currentPos __input, ["<Name>", "|", "<eof>"])
; SGrammar18 __input _ ->
    Left  (currentPos __input, ["<Name>", "<eof>"])
; SGrammar19 __input _ -> Left  (currentPos __input, ["="])
; SGrammar20 __input _ ->
    Left  (currentPos __input, ["<Name>", "<eof>"])
; SGrammar21 __input _ -> Left  (currentPos __input, ["<Name>"])
; SGrammar22 __input _ ->
    Left  (currentPos __input, ["<Name>", "<eof>"])
; SGrammar23 __input _ ->
    Left  (currentPos __input, ["<Name>", "<str>"])
; SGrammar24 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>", "=>"])
; SGrammar25 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>", "=>"])
; SGrammar26 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>", "=>"])
; SGrammar27 __input _ -> Left  (currentPos __input, [":"])
; SGrammar28 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>", "=>"])
; SGrammar29 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>", "=>"])
; SGrammar30 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>", "=>"])
; SGrammar31 __input _ -> Left  (currentPos __input, ["=>"])
; SGrammar32 __input _ -> Left  (currentPos __input, ["add"])
; SGrammar33 __input _ ->
    Left  (currentPos __input, ["<str>", "start"])
; SGrammar34 __input _ ->
    Left  (currentPos __input, ["<str>", "start"])
; SGrammar35 __input _ -> Left  (currentPos __input, ["start"])
; SGrammar36 __input _ -> Left  (currentPos __input, ["start"])
; SGrammar37 __input _ -> Left  (currentPos __input, ["<eof>"])
; SGrammar38 __input _ -> Left  (currentPos __input, ["<eof>"])
; SGrammar39 __input _ -> Left  (currentPos __input, ["<eof>"])
} where {
; action17 pos t =
              Terminal    t
; action18 pos e =
              NonTerminal e
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

parseGrammar :: FilePath -> IO (Either LexerError (Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])))
parseGrammar filepath = do
  text <- Text.readFile filepath
  case lexText filepath text [",", ":", "=", "=>", "add", "start",
                              "|"] of
    Left  err   -> pure (Left err)
    Right input -> pure (Right (__runGrammar SGrammar32 input Nil))

currentPos :: ([Lexeme], Pos) -> Pos
currentPos = \case
  ([],           end) -> end
  ((pos, _) : _, _)   -> pos
