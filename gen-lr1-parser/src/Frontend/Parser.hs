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
  SGrammar0 :: StGrammar (Text : a)
  SGrammar1 :: StGrammar (Text : a)
  SGrammar2 :: StGrammar ([Symbol] : a)
  SGrammar3 :: StGrammar (Clause : a)
  SGrammar4 :: StGrammar (NonTerminal : a)
  SGrammar5 :: StGrammar (() : a)
  SGrammar6 :: StGrammar (NonTerminal : a)
  SGrammar7 :: StGrammar (() : [Symbol] : a)
  SGrammar8 :: StGrammar (() : Clause : a)
  SGrammar9 :: StGrammar (() : NonTerminal : a)
  SGrammar10 :: StGrammar (() : NonTerminal : a)
  SGrammar11 :: StGrammar ([NonTerminal] : () : a)
  SGrammar12 :: StGrammar (() : NonTerminal : a)
  SGrammar13 :: StGrammar (Text : () : [Symbol] : a)
  SGrammar14 :: StGrammar ([Clause] : () : Clause : a)
  SGrammar15 :: StGrammar (Text : () : NonTerminal : a)
  SGrammar16 :: StGrammar ([Clause] : () : NonTerminal : a)
  SGrammar17 :: StGrammar ([NonTerminal] : () : NonTerminal : a)
  SGrammar18 :: StGrammar (() : Text : () : NonTerminal : a)
  SGrammar19 :: StGrammar ([Clause] : () : Text : () : NonTerminal :
                           a)
  SGrammar20 :: StGrammar (Text : a)
  SGrammar21 :: StGrammar (Text : a)
  SGrammar22 :: StGrammar (Text : a)
  SGrammar23 :: StGrammar (Terminal : a)
  SGrammar24 :: StGrammar (() : Text : a)
  SGrammar25 :: StGrammar (Terminal : () : Text : a)
  SGrammar26 :: StGrammar (NonTerminal : () : Text : a)
  SGrammar27 :: StGrammar (Symbol : a)
  SGrammar28 :: StGrammar ([Symbol] : Symbol : a)
  SGrammar29 :: StGrammar (() : a)
  SGrammar30 :: StGrammar (Text : a)
  SGrammar31 :: StGrammar ([Text] : () : a)
  SGrammar32 :: StGrammar ([Text] : Text : a)
  SGrammar33 :: StGrammar (a)
  SGrammar34 :: StGrammar (([Text], Set NonTerminal, [Rule]) : a)
  SGrammar35 :: StGrammar (Rule : a)
  SGrammar36 :: StGrammar ([Text] : a)
  SGrammar37 :: StGrammar ([Rule] : Rule : a)
  SGrammar38 :: StGrammar ([NonTerminal] : [Text] : a)
  SGrammar39 :: StGrammar ([Rule] : [NonTerminal] : [Text] : a)

__gotoAdditionsForGrammar :: ([Lexeme], Pos) -> [Text] -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoAdditionsForGrammar toks term stk@(state, _, _) = case state of
  SGrammar33 -> __runGrammar SGrammar36 toks (term :> stk)
  _ -> error ""

__gotoClauseForGrammar :: ([Lexeme], Pos) -> Clause -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoClauseForGrammar toks term stk@(state, _, _) = case state of
  SGrammar8 -> __runGrammar SGrammar3 toks (term :> stk)
  SGrammar10 -> __runGrammar SGrammar3 toks (term :> stk)
  SGrammar18 -> __runGrammar SGrammar3 toks (term :> stk)
  _ -> error ""

__gotoClausesForGrammar :: ([Lexeme], Pos) -> [Clause] -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoClausesForGrammar toks term stk@(state, _, _) = case state of
  SGrammar8 -> __runGrammar SGrammar14 toks (term :> stk)
  SGrammar10 -> __runGrammar SGrammar16 toks (term :> stk)
  SGrammar18 -> __runGrammar SGrammar19 toks (term :> stk)
  _ -> error ""

__gotoGrammarForGrammar :: ([Lexeme], Pos) -> ([Text], Set NonTerminal, [Rule]) -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoGrammarForGrammar toks term stk@(state, _, _) = case state of
  SGrammar33 -> __runGrammar SGrammar34 toks (term :> stk)
  _ -> error ""

__gotoLinesForGrammar :: ([Lexeme], Pos) -> [Text] -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoLinesForGrammar toks term stk@(state, _, _) = case state of
  SGrammar29 -> __runGrammar SGrammar31 toks (term :> stk)
  SGrammar30 -> __runGrammar SGrammar32 toks (term :> stk)
  _ -> error ""

__gotoNonTerminalForGrammar :: ([Lexeme], Pos) -> NonTerminal -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoNonTerminalForGrammar toks term stk@(state, _, _) = case state of
  SGrammar5 -> __runGrammar SGrammar6 toks (term :> stk)
  SGrammar12 -> __runGrammar SGrammar6 toks (term :> stk)
  SGrammar24 -> __runGrammar SGrammar26 toks (term :> stk)
  SGrammar35 -> __runGrammar SGrammar4 toks (term :> stk)
  SGrammar38 -> __runGrammar SGrammar4 toks (term :> stk)
  _ -> error ""

__gotoNonTerminalsForGrammar :: ([Lexeme], Pos) -> [NonTerminal] -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoNonTerminalsForGrammar toks term stk@(state, _, _) = case state of
  SGrammar5 -> __runGrammar SGrammar11 toks (term :> stk)
  SGrammar12 -> __runGrammar SGrammar17 toks (term :> stk)
  _ -> error ""

__gotoRuleForGrammar :: ([Lexeme], Pos) -> Rule -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoRuleForGrammar toks term stk@(state, _, _) = case state of
  SGrammar35 -> __runGrammar SGrammar35 toks (term :> stk)
  SGrammar38 -> __runGrammar SGrammar35 toks (term :> stk)
  _ -> error ""

__gotoRulesForGrammar :: ([Lexeme], Pos) -> [Rule] -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoRulesForGrammar toks term stk@(state, _, _) = case state of
  SGrammar35 -> __runGrammar SGrammar37 toks (term :> stk)
  SGrammar38 -> __runGrammar SGrammar39 toks (term :> stk)
  _ -> error ""

__gotoStartersForGrammar :: ([Lexeme], Pos) -> [NonTerminal] -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoStartersForGrammar toks term stk@(state, _, _) = case state of
  SGrammar36 -> __runGrammar SGrammar38 toks (term :> stk)
  _ -> error ""

__gotoSymbolForGrammar :: ([Lexeme], Pos) -> Symbol -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoSymbolForGrammar toks term stk@(state, _, _) = case state of
  SGrammar8 -> __runGrammar SGrammar27 toks (term :> stk)
  SGrammar10 -> __runGrammar SGrammar27 toks (term :> stk)
  SGrammar18 -> __runGrammar SGrammar27 toks (term :> stk)
  SGrammar27 -> __runGrammar SGrammar27 toks (term :> stk)
  _ -> error ""

__gotoSymbolsForGrammar :: ([Lexeme], Pos) -> [Symbol] -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoSymbolsForGrammar toks term stk@(state, _, _) = case state of
  SGrammar8 -> __runGrammar SGrammar2 toks (term :> stk)
  SGrammar10 -> __runGrammar SGrammar2 toks (term :> stk)
  SGrammar18 -> __runGrammar SGrammar2 toks (term :> stk)
  SGrammar27 -> __runGrammar SGrammar28 toks (term :> stk)
  _ -> error ""

__gotoTerminalForGrammar :: ([Lexeme], Pos) -> Terminal -> Stack StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__gotoTerminalForGrammar toks term stk@(state, _, _) = case state of
  SGrammar8 -> __runGrammar SGrammar23 toks (term :> stk)
  SGrammar10 -> __runGrammar SGrammar23 toks (term :> stk)
  SGrammar18 -> __runGrammar SGrammar23 toks (term :> stk)
  SGrammar24 -> __runGrammar SGrammar25 toks (term :> stk)
  SGrammar27 -> __runGrammar SGrammar23 toks (term :> stk)
  _ -> error ""

__runGrammar :: StGrammar a -> ([Lexeme], Pos) -> Stack' StGrammar a -> Either (Pos, [String]) ([Text], Set NonTerminal, [Rule])
__runGrammar = \cases {
; SGrammar2 ((__p,  "=>") : __input, __end) __stk ->
    __runGrammar SGrammar7 (__input, __end) (() :> (SGrammar2, __p, __stk))
; SGrammar3 ((__p,  "|") : __input, __end) __stk ->
    __runGrammar SGrammar8 (__input, __end) (() :> (SGrammar3, __p, __stk))
; SGrammar4 ((__p,  ":") : __input, __end) __stk ->
    __runGrammar SGrammar9 (__input, __end) (() :> (SGrammar4, __p, __stk))
; SGrammar4 ((__p,  "=") : __input, __end) __stk ->
    __runGrammar SGrammar10 (__input, __end) (() :> (SGrammar4, __p, __stk))
; SGrammar5 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar0 (__input, __end) (n :> (SGrammar5, __p, __stk))
; SGrammar6 ((__p,  ",") : __input, __end) __stk ->
    __runGrammar SGrammar12 (__input, __end) (() :> (SGrammar6, __p, __stk))
; SGrammar7 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar13 (__input, __end) (n :> (SGrammar7, __p, __stk))
; SGrammar8 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar22 (__input, __end) (n :> (SGrammar8, __p, __stk))
; SGrammar8 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar20 (__input, __end) (n :> (SGrammar8, __p, __stk))
; SGrammar9 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar15 (__input, __end) (n :> (SGrammar9, __p, __stk))
; SGrammar10 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar22 (__input, __end) (n :> (SGrammar10, __p, __stk))
; SGrammar10 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar20 (__input, __end) (n :> (SGrammar10, __p, __stk))
; SGrammar12 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar0 (__input, __end) (n :> (SGrammar12, __p, __stk))
; SGrammar15 ((__p,  "=") : __input, __end) __stk ->
    __runGrammar SGrammar18 (__input, __end) (() :> (SGrammar15, __p, __stk))
; SGrammar18 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar22 (__input, __end) (n :> (SGrammar18, __p, __stk))
; SGrammar18 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar20 (__input, __end) (n :> (SGrammar18, __p, __stk))
; SGrammar22 ((__p,  ":") : __input, __end) __stk ->
    __runGrammar SGrammar24 (__input, __end) (() :> (SGrammar22, __p, __stk))
; SGrammar24 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar21 (__input, __end) (n :> (SGrammar24, __p, __stk))
; SGrammar24 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar20 (__input, __end) (n :> (SGrammar24, __p, __stk))
; SGrammar27 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar22 (__input, __end) (n :> (SGrammar27, __p, __stk))
; SGrammar27 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar20 (__input, __end) (n :> (SGrammar27, __p, __stk))
; SGrammar29 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar30 (__input, __end) (n :> (SGrammar29, __p, __stk))
; SGrammar30 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar SGrammar30 (__input, __end) (n :> (SGrammar30, __p, __stk))
; SGrammar33 ((__p,  "add") : __input, __end) __stk ->
    __runGrammar SGrammar29 (__input, __end) (() :> (SGrammar33, __p, __stk))
; SGrammar35 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar1 (__input, __end) (n :> (SGrammar35, __p, __stk))
; SGrammar36 ((__p,  "start") : __input, __end) __stk ->
    __runGrammar SGrammar5 (__input, __end) (() :> (SGrammar36, __p, __stk))
; SGrammar38 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar SGrammar1 (__input, __end) (n :> (SGrammar38, __p, __stk))
-- lookahead ,, entity NonTerminal
; SGrammar0 ((__p,  ",") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoNonTerminalForGrammar ((__p,  ",") : __input, __end) (action18 __pos e) __stk
-- lookahead <Name>, entity NonTerminal
; SGrammar0 ((__p, UppercaseName tok) : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoNonTerminalForGrammar ((__p, UppercaseName tok) : __input, __end) (action18 __pos e) __stk
-- lookahead :, entity NonTerminal
; SGrammar1 ((__p,  ":") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoNonTerminalForGrammar ((__p,  ":") : __input, __end) (action18 __pos e) __stk
-- lookahead =, entity NonTerminal
; SGrammar1 ((__p,  "=") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoNonTerminalForGrammar ((__p,  "=") : __input, __end) (action18 __pos e) __stk
-- lookahead <Name>, entity Clauses
; SGrammar3 ((__p, UppercaseName tok) : __input, __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoClausesForGrammar ((__p, UppercaseName tok) : __input, __end) (action34 __pos c) __stk
-- lookahead <eof>, entity Clauses
; SGrammar3 ([], __end) ((c :> __stk@(_, __pos, _))) ->
    __gotoClausesForGrammar ([], __end) (action34 __pos c) __stk
-- lookahead <Name>, entity NonTerminals
; SGrammar6 ((__p, UppercaseName tok) : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoNonTerminalsForGrammar ((__p, UppercaseName tok) : __input, __end) (action57 __pos e) __stk
-- lookahead <Name>, entity Starters
; SGrammar11 ((__p, UppercaseName tok) : __input, __end) ((es :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoStartersForGrammar ((__p, UppercaseName tok) : __input, __end) (action54 __pos es) __stk
-- lookahead <Name>, entity Clause
; SGrammar13 ((__p, UppercaseName tok) : __input, __end) ((reducer :> (_, _, _ :> (_, _, points :> __stk@(_, __pos, _))))) ->
    __gotoClauseForGrammar ((__p, UppercaseName tok) : __input, __end) (action30 __pos points
                                                                                       reducer) __stk
-- lookahead |, entity Clause
; SGrammar13 ((__p,  "|") : __input, __end) ((reducer :> (_, _, _ :> (_, _, points :> __stk@(_, __pos, _))))) ->
    __gotoClauseForGrammar ((__p,  "|") : __input, __end) (action30 __pos points
                                                                          reducer) __stk
-- lookahead <eof>, entity Clause
; SGrammar13 ([], __end) ((reducer :> (_, _, _ :> (_, _, points :> __stk@(_, __pos, _))))) ->
    __gotoClauseForGrammar ([], __end) (action30 __pos points
                                                       reducer) __stk
-- lookahead <Name>, entity Clauses
; SGrammar14 ((__p, UppercaseName tok) : __input, __end) ((cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _))))) ->
    __gotoClausesForGrammar ((__p, UppercaseName tok) : __input, __end) (action35 __pos c
                                                                                        cs) __stk
-- lookahead <eof>, entity Clauses
; SGrammar14 ([], __end) ((cs :> (_, _, _ :> (_, _, c :> __stk@(_, __pos, _))))) ->
    __gotoClausesForGrammar ([], __end) (action35 __pos c cs) __stk
-- lookahead <Name>, entity Rule
; SGrammar16 ((__p, UppercaseName tok) : __input, __end) ((clauses :> (_, _, _ :> (_, _, entity :> __stk@(_, __pos, _))))) ->
    __gotoRuleForGrammar ((__p, UppercaseName tok) : __input, __end) (action39 __pos entity
                                                                                     clauses) __stk
-- lookahead <eof>, entity Rule
; SGrammar16 ([], __end) ((clauses :> (_, _, _ :> (_, _, entity :> __stk@(_, __pos, _))))) ->
    __gotoRuleForGrammar ([], __end) (action39 __pos entity
                                                     clauses) __stk
-- lookahead <Name>, entity NonTerminals
; SGrammar17 ((__p, UppercaseName tok) : __input, __end) ((es :> (_, _, _ :> (_, _, e :> __stk@(_, __pos, _))))) ->
    __gotoNonTerminalsForGrammar ((__p, UppercaseName tok) : __input, __end) (action58 __pos e
                                                                                             es) __stk
-- lookahead <Name>, entity Rule
; SGrammar19 ((__p, UppercaseName tok) : __input, __end) ((clauses :> (_, _, _ :> (_, _, type_ :> (_, _, _ :> (_, _, entity :> __stk@(_, __pos, _))))))) ->
    __gotoRuleForGrammar ((__p, UppercaseName tok) : __input, __end) (action38 __pos entity
                                                                                     type_
                                                                                     clauses) __stk
-- lookahead <eof>, entity Rule
; SGrammar19 ([], __end) ((clauses :> (_, _, _ :> (_, _, type_ :> (_, _, _ :> (_, _, entity :> __stk@(_, __pos, _))))))) ->
    __gotoRuleForGrammar ([], __end) (action38 __pos entity type_
                                                     clauses) __stk
-- lookahead <name>, entity Terminal
; SGrammar20 ((__p, LowercaseName tok) : __input, __end) ((t :> __stk@(_, __pos, _))) ->
    __gotoTerminalForGrammar ((__p, LowercaseName tok) : __input, __end) (action17 __pos t) __stk
-- lookahead <str>, entity Terminal
; SGrammar20 ((__p, StringLiteral tok) : __input, __end) ((t :> __stk@(_, __pos, _))) ->
    __gotoTerminalForGrammar ((__p, StringLiteral tok) : __input, __end) (action17 __pos t) __stk
-- lookahead =>, entity Terminal
; SGrammar20 ((__p,  "=>") : __input, __end) ((t :> __stk@(_, __pos, _))) ->
    __gotoTerminalForGrammar ((__p,  "=>") : __input, __end) (action17 __pos t) __stk
-- lookahead <name>, entity NonTerminal
; SGrammar21 ((__p, LowercaseName tok) : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoNonTerminalForGrammar ((__p, LowercaseName tok) : __input, __end) (action18 __pos e) __stk
-- lookahead <str>, entity NonTerminal
; SGrammar21 ((__p, StringLiteral tok) : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoNonTerminalForGrammar ((__p, StringLiteral tok) : __input, __end) (action18 __pos e) __stk
-- lookahead =>, entity NonTerminal
; SGrammar21 ((__p,  "=>") : __input, __end) ((e :> __stk@(_, __pos, _))) ->
    __gotoNonTerminalForGrammar ((__p,  "=>") : __input, __end) (action18 __pos e) __stk
-- lookahead <name>, entity Symbol
; SGrammar23 ((__p, LowercaseName tok) : __input, __end) ((t :> __stk@(_, __pos, _))) ->
    __gotoSymbolForGrammar ((__p, LowercaseName tok) : __input, __end) (action23 __pos t) __stk
-- lookahead <str>, entity Symbol
; SGrammar23 ((__p, StringLiteral tok) : __input, __end) ((t :> __stk@(_, __pos, _))) ->
    __gotoSymbolForGrammar ((__p, StringLiteral tok) : __input, __end) (action23 __pos t) __stk
-- lookahead =>, entity Symbol
; SGrammar23 ((__p,  "=>") : __input, __end) ((t :> __stk@(_, __pos, _))) ->
    __gotoSymbolForGrammar ((__p,  "=>") : __input, __end) (action23 __pos t) __stk
-- lookahead <name>, entity Symbol
; SGrammar25 ((__p, LowercaseName tok) : __input, __end) ((t :> (_, _, _ :> (_, _, n :> __stk@(_, __pos, _))))) ->
    __gotoSymbolForGrammar ((__p, LowercaseName tok) : __input, __end) (action21 __pos n
                                                                                       t) __stk
-- lookahead <str>, entity Symbol
; SGrammar25 ((__p, StringLiteral tok) : __input, __end) ((t :> (_, _, _ :> (_, _, n :> __stk@(_, __pos, _))))) ->
    __gotoSymbolForGrammar ((__p, StringLiteral tok) : __input, __end) (action21 __pos n
                                                                                       t) __stk
-- lookahead =>, entity Symbol
; SGrammar25 ((__p,  "=>") : __input, __end) ((t :> (_, _, _ :> (_, _, n :> __stk@(_, __pos, _))))) ->
    __gotoSymbolForGrammar ((__p,  "=>") : __input, __end) (action21 __pos n
                                                                           t) __stk
-- lookahead <name>, entity Symbol
; SGrammar26 ((__p, LowercaseName tok) : __input, __end) ((e :> (_, _, _ :> (_, _, n :> __stk@(_, __pos, _))))) ->
    __gotoSymbolForGrammar ((__p, LowercaseName tok) : __input, __end) (action22 __pos n
                                                                                       e) __stk
-- lookahead <str>, entity Symbol
; SGrammar26 ((__p, StringLiteral tok) : __input, __end) ((e :> (_, _, _ :> (_, _, n :> __stk@(_, __pos, _))))) ->
    __gotoSymbolForGrammar ((__p, StringLiteral tok) : __input, __end) (action22 __pos n
                                                                                       e) __stk
-- lookahead =>, entity Symbol
; SGrammar26 ((__p,  "=>") : __input, __end) ((e :> (_, _, _ :> (_, _, n :> __stk@(_, __pos, _))))) ->
    __gotoSymbolForGrammar ((__p,  "=>") : __input, __end) (action22 __pos n
                                                                           e) __stk
-- lookahead =>, entity Symbols
; SGrammar27 ((__p,  "=>") : __input, __end) ((p :> __stk@(_, __pos, _))) ->
    __gotoSymbolsForGrammar ((__p,  "=>") : __input, __end) (action26 __pos p) __stk
-- lookahead =>, entity Symbols
; SGrammar28 ((__p,  "=>") : __input, __end) ((ps :> (_, _, p :> __stk@(_, __pos, _)))) ->
    __gotoSymbolsForGrammar ((__p,  "=>") : __input, __end) (action27 __pos p
                                                                            ps) __stk
-- lookahead start, entity Additions
; SGrammar29 ((__p,  "start") : __input, __end) ((_ :> __stk@(_, __pos, _))) ->
    __gotoAdditionsForGrammar ((__p,  "start") : __input, __end) (action46 __pos ) __stk
-- lookahead start, entity Lines
; SGrammar30 ((__p,  "start") : __input, __end) ((t :> __stk@(_, __pos, _))) ->
    __gotoLinesForGrammar ((__p,  "start") : __input, __end) (action50 __pos t) __stk
-- lookahead start, entity Additions
; SGrammar31 ((__p,  "start") : __input, __end) ((ls :> (_, _, _ :> __stk@(_, __pos, _)))) ->
    __gotoAdditionsForGrammar ((__p,  "start") : __input, __end) (action47 __pos ls) __stk
-- lookahead start, entity Lines
; SGrammar32 ((__p,  "start") : __input, __end) ((ts :> (_, _, t :> __stk@(_, __pos, _)))) ->
    __gotoLinesForGrammar ((__p,  "start") : __input, __end) (action51 __pos t
                                                                             ts) __stk
-- lookahead <eof>, entity Grammar
; SGrammar34 ([], __end) ((res :> __stk@(_, __pos, _))) -> pure res
-- lookahead <eof>, entity Rules
; SGrammar35 ([], __end) ((r :> __stk@(_, __pos, _))) ->
    __gotoRulesForGrammar ([], __end) (action42 __pos r) __stk
-- lookahead <eof>, entity Rules
; SGrammar37 ([], __end) ((rs :> (_, _, r :> __stk@(_, __pos, _)))) ->
    __gotoRulesForGrammar ([], __end) (action43 __pos r rs) __stk
-- lookahead <eof>, entity Grammar
; SGrammar39 ([], __end) ((r :> (_, _, s :> (_, _, a :> __stk@(_, __pos, _))))) ->
    __gotoGrammarForGrammar ([], __end) (action62 __pos a s r) __stk
; SGrammar0 __input _ ->
    Left  (currentPos __input, [",", "<Name>"])
; SGrammar1 __input _ -> Left  (currentPos __input, [":", "="])
; SGrammar2 __input _ -> Left  (currentPos __input, ["=>"])
; SGrammar3 __input _ ->
    Left  (currentPos __input, ["<Name>", "|", "<eof>"])
; SGrammar4 __input _ -> Left  (currentPos __input, [":", "="])
; SGrammar5 __input _ -> Left  (currentPos __input, ["<Name>"])
; SGrammar6 __input _ ->
    Left  (currentPos __input, [",", "<Name>"])
; SGrammar7 __input _ -> Left  (currentPos __input, ["<str>"])
; SGrammar8 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>"])
; SGrammar9 __input _ -> Left  (currentPos __input, ["<str>"])
; SGrammar10 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>"])
; SGrammar11 __input _ -> Left  (currentPos __input, ["<Name>"])
; SGrammar12 __input _ -> Left  (currentPos __input, ["<Name>"])
; SGrammar13 __input _ ->
    Left  (currentPos __input, ["<Name>", "|", "<eof>"])
; SGrammar14 __input _ ->
    Left  (currentPos __input, ["<Name>", "<eof>"])
; SGrammar15 __input _ -> Left  (currentPos __input, ["="])
; SGrammar16 __input _ ->
    Left  (currentPos __input, ["<Name>", "<eof>"])
; SGrammar17 __input _ -> Left  (currentPos __input, ["<Name>"])
; SGrammar18 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>"])
; SGrammar19 __input _ ->
    Left  (currentPos __input, ["<Name>", "<eof>"])
; SGrammar20 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>", "=>"])
; SGrammar21 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>", "=>"])
; SGrammar22 __input _ -> Left  (currentPos __input, [":"])
; SGrammar23 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>", "=>"])
; SGrammar24 __input _ ->
    Left  (currentPos __input, ["<Name>", "<str>"])
; SGrammar25 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>", "=>"])
; SGrammar26 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>", "=>"])
; SGrammar27 __input _ ->
    Left  (currentPos __input, ["<name>", "<str>", "=>"])
; SGrammar28 __input _ -> Left  (currentPos __input, ["=>"])
; SGrammar29 __input _ ->
    Left  (currentPos __input, ["<str>", "start"])
; SGrammar30 __input _ ->
    Left  (currentPos __input, ["<str>", "start"])
; SGrammar31 __input _ -> Left  (currentPos __input, ["start"])
; SGrammar32 __input _ -> Left  (currentPos __input, ["start"])
; SGrammar33 __input _ -> Left  (currentPos __input, ["add"])
; SGrammar34 __input _ -> Left  (currentPos __input, ["<eof>"])
; SGrammar35 __input _ ->
    Left  (currentPos __input, ["<Name>", "<eof>"])
; SGrammar36 __input _ -> Left  (currentPos __input, ["start"])
; SGrammar37 __input _ -> Left  (currentPos __input, ["<eof>"])
; SGrammar38 __input _ -> Left  (currentPos __input, ["<Name>"])
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
    Right input -> pure (Right (__runGrammar SGrammar33 input Nil))

currentPos :: ([Lexeme], Pos) -> Pos
currentPos = \case
  ([],           end) -> end
  ((pos, _) : _, _)   -> pos
