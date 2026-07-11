{-# language PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Frontend.Parser (parseGrammar) where
  
import Data.Text.IO.Utf8 qualified as Text
import Data.Kind qualified as Kind
import Rule
import Term
import Text.Lexer.Default
import Data.Text.Position
import Data.Text (Text)
import Data.Lexeme
import Data.Array
listToArray :: [a] -> Array Int a
listToArray as = Data.Array.listArray (0, length as - 1) as
  
data Stack' xs where
  Nil  ::      Stack' '[]
  (:>) :: x -> Stack xs -> Stack' (x : xs)
  
type Stack a = (St a, Pos, Stack' a)
  
pattern (:?) :: a -> Stack xs -> Stack (a : xs)
pattern a :? xs <- (_, _, a :> xs)
  
infixr 2 :>, :?
  
data St :: [Kind.Type] -> Kind.Type where
  S0 :: forall a. St (a)
  S1 :: forall a. St (([Text], Entity, [Rule]) : a)
  S2 :: forall a. St ([Point] : a)
  S3 :: forall a. St (Clause : a)
  S4 :: forall a. St (Entity : a)
  S5 :: forall a. St (Rule : a)
  S6 :: forall a. St ([Text] : a)
  S7 :: forall a. St (() : [Point] : a)
  S8 :: forall a. St (() : Clause : a)
  S9 :: forall a. St (() : Entity : a)
  S10 :: forall a. St (() : Entity : a)
  S11 :: forall a. St ([Rule] : Rule : a)
  S12 :: forall a. St (Entity : [Text] : a)
  S13 :: forall a. St (Text : () : [Point] : a)
  S14 :: forall a. St ([Clause] : () : Clause : a)
  S15 :: forall a. St (Text : () : Entity : a)
  S16 :: forall a. St ([Clause] : () : Entity : a)
  S17 :: forall a. St ([Rule] : Entity : [Text] : a)
  S18 :: forall a. St (() : Text : () : Entity : a)
  S19 :: forall a. St ([Clause] : () : Text : () : Entity : a)
  S20 :: forall a. St (Text : a)
  S21 :: forall a. St (Text : a)
  S22 :: forall a. St (Text : a)
  S23 :: forall a. St (Text : a)
  S24 :: forall a. St (Text : a)
  S25 :: forall a. St (Term : a)
  S26 :: forall a. St (Entity : a)
  S27 :: forall a. St (() : a)
  S28 :: forall a. St (() : Text : a)
  S29 :: forall a. St (Entity : () : a)
  S30 :: forall a. St (Term : () : Text : a)
  S31 :: forall a. St (Entity : () : Text : a)
  S32 :: forall a. St (Point : a)
  S33 :: forall a. St ([Point] : Point : a)
  S34 :: forall a. St (() : a)
  S35 :: forall a. St (Text : a)
  S36 :: forall a. St ([Text] : () : a)
  S37 :: forall a. St ([Text] : Text : a)
  
__gotoAdditions :: ([Lexeme], Pos) -> [Text] -> Stack a -> Either (Pos, [String]) ([Text], Entity, [Rule])
__gotoAdditions toks term stk@(state, _, _) = case state of
  S0 -> __runGrammar S6 toks (term :> stk)
  _ -> error ""

__gotoClause :: ([Lexeme], Pos) -> Clause -> Stack a -> Either (Pos, [String]) ([Text], Entity, [Rule])
__gotoClause toks term stk@(state, _, _) = case state of
  S8 -> __runGrammar S3 toks (term :> stk)
  S10 -> __runGrammar S3 toks (term :> stk)
  S18 -> __runGrammar S3 toks (term :> stk)
  _ -> error ""

__gotoClauses :: ([Lexeme], Pos) -> [Clause] -> Stack a -> Either (Pos, [String]) ([Text], Entity, [Rule])
__gotoClauses toks term stk@(state, _, _) = case state of
  S8 -> __runGrammar S14 toks (term :> stk)
  S10 -> __runGrammar S16 toks (term :> stk)
  S18 -> __runGrammar S19 toks (term :> stk)
  _ -> error ""

__gotoEntity :: ([Lexeme], Pos) -> Entity -> Stack a -> Either (Pos, [String]) ([Text], Entity, [Rule])
__gotoEntity toks term stk@(state, _, _) = case state of
  S5 -> __runGrammar S4 toks (term :> stk)
  S8 -> __runGrammar S26 toks (term :> stk)
  S10 -> __runGrammar S26 toks (term :> stk)
  S12 -> __runGrammar S4 toks (term :> stk)
  S18 -> __runGrammar S26 toks (term :> stk)
  S27 -> __runGrammar S29 toks (term :> stk)
  S28 -> __runGrammar S31 toks (term :> stk)
  S32 -> __runGrammar S26 toks (term :> stk)
  _ -> error ""

__gotoGrammar :: ([Lexeme], Pos) -> ([Text], Entity, [Rule]) -> Stack a -> Either (Pos, [String]) ([Text], Entity, [Rule])
__gotoGrammar toks term stk@(state, _, _) = case state of
  S0 -> __runGrammar S1 toks (term :> stk)
  _ -> error ""

__gotoLines :: ([Lexeme], Pos) -> [Text] -> Stack a -> Either (Pos, [String]) ([Text], Entity, [Rule])
__gotoLines toks term stk@(state, _, _) = case state of
  S34 -> __runGrammar S36 toks (term :> stk)
  S35 -> __runGrammar S37 toks (term :> stk)
  _ -> error ""

__gotoPoint :: ([Lexeme], Pos) -> Point -> Stack a -> Either (Pos, [String]) ([Text], Entity, [Rule])
__gotoPoint toks term stk@(state, _, _) = case state of
  S8 -> __runGrammar S32 toks (term :> stk)
  S10 -> __runGrammar S32 toks (term :> stk)
  S18 -> __runGrammar S32 toks (term :> stk)
  S32 -> __runGrammar S32 toks (term :> stk)
  _ -> error ""

__gotoPoints :: ([Lexeme], Pos) -> [Point] -> Stack a -> Either (Pos, [String]) ([Text], Entity, [Rule])
__gotoPoints toks term stk@(state, _, _) = case state of
  S8 -> __runGrammar S2 toks (term :> stk)
  S10 -> __runGrammar S2 toks (term :> stk)
  S18 -> __runGrammar S2 toks (term :> stk)
  S32 -> __runGrammar S33 toks (term :> stk)
  _ -> error ""

__gotoRule :: ([Lexeme], Pos) -> Rule -> Stack a -> Either (Pos, [String]) ([Text], Entity, [Rule])
__gotoRule toks term stk@(state, _, _) = case state of
  S5 -> __runGrammar S5 toks (term :> stk)
  S12 -> __runGrammar S5 toks (term :> stk)
  _ -> error ""

__gotoRules :: ([Lexeme], Pos) -> [Rule] -> Stack a -> Either (Pos, [String]) ([Text], Entity, [Rule])
__gotoRules toks term stk@(state, _, _) = case state of
  S5 -> __runGrammar S11 toks (term :> stk)
  S12 -> __runGrammar S17 toks (term :> stk)
  _ -> error ""

__gotoStarter :: ([Lexeme], Pos) -> Entity -> Stack a -> Either (Pos, [String]) ([Text], Entity, [Rule])
__gotoStarter toks term stk@(state, _, _) = case state of
  S6 -> __runGrammar S12 toks (term :> stk)
  _ -> error ""

__gotoTerm :: ([Lexeme], Pos) -> Term -> Stack a -> Either (Pos, [String]) ([Text], Entity, [Rule])
__gotoTerm toks term stk@(state, _, _) = case state of
  S8 -> __runGrammar S25 toks (term :> stk)
  S10 -> __runGrammar S25 toks (term :> stk)
  S18 -> __runGrammar S25 toks (term :> stk)
  S28 -> __runGrammar S30 toks (term :> stk)
  S32 -> __runGrammar S25 toks (term :> stk)
  _ -> error ""
  
__runGrammar :: St a -> ([Lexeme], Pos) -> Stack' a -> Either (Pos, [String]) ([Text], Entity, [Rule])
__runGrammar = \cases {
; S0 ((__p,  "add") : __input, __end) __stk ->
    __runGrammar S34 (__input, __end) (() :> (S0, __p, __stk))
; S2 ((__p,  "=>") : __input, __end) __stk ->
    __runGrammar S7 (__input, __end) (() :> (S2, __p, __stk))
; S3 ((__p,  "|") : __input, __end) __stk ->
    __runGrammar S8 (__input, __end) (() :> (S3, __p, __stk))
; S4 ((__p,  ":") : __input, __end) __stk ->
    __runGrammar S9 (__input, __end) (() :> (S4, __p, __stk))
; S4 ((__p,  "=") : __input, __end) __stk ->
    __runGrammar S10 (__input, __end) (() :> (S4, __p, __stk))
; S5 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar S20 (__input, __end) (n :> (S5, __p, __stk))
; S6 ((__p,  "start") : __input, __end) __stk ->
    __runGrammar S27 (__input, __end) (() :> (S6, __p, __stk))
; S7 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar S13 (__input, __end) (n :> (S7, __p, __stk))
; S8 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar S23 (__input, __end) (n :> (S8, __p, __stk))
; S8 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runGrammar S24 (__input, __end) (n :> (S8, __p, __stk))
; S8 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar S21 (__input, __end) (n :> (S8, __p, __stk))
; S9 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar S15 (__input, __end) (n :> (S9, __p, __stk))
; S10 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar S23 (__input, __end) (n :> (S10, __p, __stk))
; S10 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runGrammar S24 (__input, __end) (n :> (S10, __p, __stk))
; S10 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar S21 (__input, __end) (n :> (S10, __p, __stk))
; S12 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar S20 (__input, __end) (n :> (S12, __p, __stk))
; S15 ((__p,  "=") : __input, __end) __stk ->
    __runGrammar S18 (__input, __end) (() :> (S15, __p, __stk))
; S18 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar S23 (__input, __end) (n :> (S18, __p, __stk))
; S18 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runGrammar S24 (__input, __end) (n :> (S18, __p, __stk))
; S18 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar S21 (__input, __end) (n :> (S18, __p, __stk))
; S24 ((__p,  ":") : __input, __end) __stk ->
    __runGrammar S28 (__input, __end) (() :> (S24, __p, __stk))
; S27 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar S22 (__input, __end) (n :> (S27, __p, __stk))
; S28 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar S23 (__input, __end) (n :> (S28, __p, __stk))
; S28 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar S21 (__input, __end) (n :> (S28, __p, __stk))
; S32 ((__p, UppercaseName n) : __input, __end) __stk ->
    __runGrammar S23 (__input, __end) (n :> (S32, __p, __stk))
; S32 ((__p, LowercaseName n) : __input, __end) __stk ->
    __runGrammar S24 (__input, __end) (n :> (S32, __p, __stk))
; S32 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar S21 (__input, __end) (n :> (S32, __p, __stk))
; S34 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar S35 (__input, __end) (n :> (S34, __p, __stk))
; S35 ((__p, StringLiteral n) : __input, __end) __stk ->
    __runGrammar S35 (__input, __end) (n :> (S35, __p, __stk))
; S1 ([], __end) (res :> __stk@(_, __pos, _)) -> pure res
; S3 ([], __end) (c :> __stk@(_, __pos, _)) ->
    __gotoClauses ([], __end) (action32 __pos c) __stk
; S3 ((__p, UppercaseName tok) : __input, __end) (c :> __stk@(_, __pos, _)) ->
    __gotoClauses ((__p, UppercaseName tok) : __input, __end) (action32 __pos c) __stk
; S5 ([], __end) (r :> __stk@(_, __pos, _)) ->
    __gotoRules ([], __end) (action40 __pos r) __stk
; S11 ([], __end) (rs :> r :? __stk@(_, __pos, _)) ->
    __gotoRules ([], __end) (action41 __pos r rs) __stk
; S13 ([], __end) (reducer :> _ :? points :? __stk@(_, __pos, _)) ->
    __gotoClause ([], __end) (action28 __pos points reducer) __stk
; S13 ((__p, UppercaseName tok) : __input, __end) (reducer :> _ :? points :? __stk@(_, __pos, _)) ->
    __gotoClause ((__p, UppercaseName tok) : __input, __end) (action28 __pos points
                                                                             reducer) __stk
; S13 ((__p,  "|") : __input, __end) (reducer :> _ :? points :? __stk@(_, __pos, _)) ->
    __gotoClause ((__p,  "|") : __input, __end) (action28 __pos points
                                                                reducer) __stk
; S14 ([], __end) (cs :> _ :? c :? __stk@(_, __pos, _)) ->
    __gotoClauses ([], __end) (action33 __pos c cs) __stk
; S14 ((__p, UppercaseName tok) : __input, __end) (cs :> _ :? c :? __stk@(_, __pos, _)) ->
    __gotoClauses ((__p, UppercaseName tok) : __input, __end) (action33 __pos c
                                                                              cs) __stk
; S16 ([], __end) (clauses :> _ :? entity :? __stk@(_, __pos, _)) ->
    __gotoRule ([], __end) (action37 __pos entity clauses) __stk
; S16 ((__p, UppercaseName tok) : __input, __end) (clauses :> _ :? entity :? __stk@(_, __pos, _)) ->
    __gotoRule ((__p, UppercaseName tok) : __input, __end) (action37 __pos entity
                                                                           clauses) __stk
; S17 ([], __end) (r :> s :? a :? __stk@(_, __pos, _)) ->
    __gotoGrammar ([], __end) (action55 __pos a s r) __stk
; S19 ([], __end) (clauses :> _ :? type_ :? _ :? entity :? __stk@(_, __pos, _)) ->
    __gotoRule ([], __end) (action36 __pos entity type_ clauses) __stk
; S19 ((__p, UppercaseName tok) : __input, __end) (clauses :> _ :? type_ :? _ :? entity :? __stk@(_, __pos, _)) ->
    __gotoRule ((__p, UppercaseName tok) : __input, __end) (action36 __pos entity
                                                                           type_ clauses) __stk
; S20 ((__p,  ":") : __input, __end) (e :> __stk@(_, __pos, _)) ->
    __gotoEntity ((__p,  ":") : __input, __end) (action15 __pos e) __stk
; S20 ((__p,  "=") : __input, __end) (e :> __stk@(_, __pos, _)) ->
    __gotoEntity ((__p,  "=") : __input, __end) (action15 __pos e) __stk
; S21 ((__p, UppercaseName tok) : __input, __end) (t :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p, UppercaseName tok) : __input, __end) (action14 __pos t) __stk
; S21 ((__p, LowercaseName tok) : __input, __end) (t :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p, LowercaseName tok) : __input, __end) (action14 __pos t) __stk
; S21 ((__p, StringLiteral tok) : __input, __end) (t :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p, StringLiteral tok) : __input, __end) (action14 __pos t) __stk
; S21 ((__p,  "=>") : __input, __end) (t :> __stk@(_, __pos, _)) ->
    __gotoTerm ((__p,  "=>") : __input, __end) (action14 __pos t) __stk
; S22 ((__p, UppercaseName tok) : __input, __end) (e :> __stk@(_, __pos, _)) ->
    __gotoEntity ((__p, UppercaseName tok) : __input, __end) (action15 __pos e) __stk
; S23 ((__p, UppercaseName tok) : __input, __end) (e :> __stk@(_, __pos, _)) ->
    __gotoEntity ((__p, UppercaseName tok) : __input, __end) (action15 __pos e) __stk
; S23 ((__p, LowercaseName tok) : __input, __end) (e :> __stk@(_, __pos, _)) ->
    __gotoEntity ((__p, LowercaseName tok) : __input, __end) (action15 __pos e) __stk
; S23 ((__p, StringLiteral tok) : __input, __end) (e :> __stk@(_, __pos, _)) ->
    __gotoEntity ((__p, StringLiteral tok) : __input, __end) (action15 __pos e) __stk
; S23 ((__p,  "=>") : __input, __end) (e :> __stk@(_, __pos, _)) ->
    __gotoEntity ((__p,  "=>") : __input, __end) (action15 __pos e) __stk
; S25 ((__p, UppercaseName tok) : __input, __end) (t :> __stk@(_, __pos, _)) ->
    __gotoPoint ((__p, UppercaseName tok) : __input, __end) (action20 __pos t) __stk
; S25 ((__p, LowercaseName tok) : __input, __end) (t :> __stk@(_, __pos, _)) ->
    __gotoPoint ((__p, LowercaseName tok) : __input, __end) (action20 __pos t) __stk
; S25 ((__p, StringLiteral tok) : __input, __end) (t :> __stk@(_, __pos, _)) ->
    __gotoPoint ((__p, StringLiteral tok) : __input, __end) (action20 __pos t) __stk
; S25 ((__p,  "=>") : __input, __end) (t :> __stk@(_, __pos, _)) ->
    __gotoPoint ((__p,  "=>") : __input, __end) (action20 __pos t) __stk
; S26 ((__p, UppercaseName tok) : __input, __end) (e :> __stk@(_, __pos, _)) ->
    __gotoPoint ((__p, UppercaseName tok) : __input, __end) (action21 __pos e) __stk
; S26 ((__p, LowercaseName tok) : __input, __end) (e :> __stk@(_, __pos, _)) ->
    __gotoPoint ((__p, LowercaseName tok) : __input, __end) (action21 __pos e) __stk
; S26 ((__p, StringLiteral tok) : __input, __end) (e :> __stk@(_, __pos, _)) ->
    __gotoPoint ((__p, StringLiteral tok) : __input, __end) (action21 __pos e) __stk
; S26 ((__p,  "=>") : __input, __end) (e :> __stk@(_, __pos, _)) ->
    __gotoPoint ((__p,  "=>") : __input, __end) (action21 __pos e) __stk
; S29 ((__p, UppercaseName tok) : __input, __end) (e :> _ :? __stk@(_, __pos, _)) ->
    __gotoStarter ((__p, UppercaseName tok) : __input, __end) (action52 __pos e) __stk
; S30 ((__p, UppercaseName tok) : __input, __end) (t :> _ :? n :? __stk@(_, __pos, _)) ->
    __gotoPoint ((__p, UppercaseName tok) : __input, __end) (action18 __pos n
                                                                            t) __stk
; S30 ((__p, LowercaseName tok) : __input, __end) (t :> _ :? n :? __stk@(_, __pos, _)) ->
    __gotoPoint ((__p, LowercaseName tok) : __input, __end) (action18 __pos n
                                                                            t) __stk
; S30 ((__p, StringLiteral tok) : __input, __end) (t :> _ :? n :? __stk@(_, __pos, _)) ->
    __gotoPoint ((__p, StringLiteral tok) : __input, __end) (action18 __pos n
                                                                            t) __stk
; S30 ((__p,  "=>") : __input, __end) (t :> _ :? n :? __stk@(_, __pos, _)) ->
    __gotoPoint ((__p,  "=>") : __input, __end) (action18 __pos n
                                                                t) __stk
; S31 ((__p, UppercaseName tok) : __input, __end) (e :> _ :? n :? __stk@(_, __pos, _)) ->
    __gotoPoint ((__p, UppercaseName tok) : __input, __end) (action19 __pos n
                                                                            e) __stk
; S31 ((__p, LowercaseName tok) : __input, __end) (e :> _ :? n :? __stk@(_, __pos, _)) ->
    __gotoPoint ((__p, LowercaseName tok) : __input, __end) (action19 __pos n
                                                                            e) __stk
; S31 ((__p, StringLiteral tok) : __input, __end) (e :> _ :? n :? __stk@(_, __pos, _)) ->
    __gotoPoint ((__p, StringLiteral tok) : __input, __end) (action19 __pos n
                                                                            e) __stk
; S31 ((__p,  "=>") : __input, __end) (e :> _ :? n :? __stk@(_, __pos, _)) ->
    __gotoPoint ((__p,  "=>") : __input, __end) (action19 __pos n
                                                                e) __stk
; S32 ((__p,  "=>") : __input, __end) (p :> __stk@(_, __pos, _)) ->
    __gotoPoints ((__p,  "=>") : __input, __end) (action24 __pos p) __stk
; S33 ((__p,  "=>") : __input, __end) (ps :> p :? __stk@(_, __pos, _)) ->
    __gotoPoints ((__p,  "=>") : __input, __end) (action25 __pos p
                                                                 ps) __stk
; S34 ((__p,  "start") : __input, __end) (_ :> __stk@(_, __pos, _)) ->
    __gotoAdditions ((__p,  "start") : __input, __end) (action44 __pos ) __stk
; S35 ((__p,  "start") : __input, __end) (t :> __stk@(_, __pos, _)) ->
    __gotoLines ((__p,  "start") : __input, __end) (action48 __pos t) __stk
; S36 ((__p,  "start") : __input, __end) (ls :> _ :? __stk@(_, __pos, _)) ->
    __gotoAdditions ((__p,  "start") : __input, __end) (action45 __pos ls) __stk
; S37 ((__p,  "start") : __input, __end) (ts :> t :? __stk@(_, __pos, _)) ->
    __gotoLines ((__p,  "start") : __input, __end) (action49 __pos t
                                                                   ts) __stk
; S0 __input _ -> Left  (currentPos __input, ["add"])
; S1 __input _ -> Left  (currentPos __input, ["$"])
; S2 __input _ -> Left  (currentPos __input, ["=>"])
; S3 __input _ -> Left  (currentPos __input, ["$", "<Name>", "|"])
; S4 __input _ -> Left  (currentPos __input, [":", "="])
; S5 __input _ -> Left  (currentPos __input, ["$", "<Name>"])
; S6 __input _ -> Left  (currentPos __input, ["start"])
; S7 __input _ -> Left  (currentPos __input, ["<str>"])
; S8 __input _ ->
    Left  (currentPos __input, ["<Name>", "<name>", "<str>"])
; S9 __input _ -> Left  (currentPos __input, ["<str>"])
; S10 __input _ ->
    Left  (currentPos __input, ["<Name>", "<name>", "<str>"])
; S11 __input _ -> Left  (currentPos __input, ["$"])
; S12 __input _ -> Left  (currentPos __input, ["<Name>"])
; S13 __input _ -> Left  (currentPos __input, ["$", "<Name>", "|"])
; S14 __input _ -> Left  (currentPos __input, ["$", "<Name>"])
; S15 __input _ -> Left  (currentPos __input, ["="])
; S16 __input _ -> Left  (currentPos __input, ["$", "<Name>"])
; S17 __input _ -> Left  (currentPos __input, ["$"])
; S18 __input _ ->
    Left  (currentPos __input, ["<Name>", "<name>", "<str>"])
; S19 __input _ -> Left  (currentPos __input, ["$", "<Name>"])
; S20 __input _ -> Left  (currentPos __input, [":", "="])
; S21 __input _ ->
    Left  (currentPos __input, ["<Name>", "<name>", "<str>", "=>"])
; S22 __input _ -> Left  (currentPos __input, ["<Name>"])
; S23 __input _ ->
    Left  (currentPos __input, ["<Name>", "<name>", "<str>", "=>"])
; S24 __input _ -> Left  (currentPos __input, [":"])
; S25 __input _ ->
    Left  (currentPos __input, ["<Name>", "<name>", "<str>", "=>"])
; S26 __input _ ->
    Left  (currentPos __input, ["<Name>", "<name>", "<str>", "=>"])
; S27 __input _ -> Left  (currentPos __input, ["<Name>"])
; S28 __input _ -> Left  (currentPos __input, ["<Name>", "<str>"])
; S29 __input _ -> Left  (currentPos __input, ["<Name>"])
; S30 __input _ ->
    Left  (currentPos __input, ["<Name>", "<name>", "<str>", "=>"])
; S31 __input _ ->
    Left  (currentPos __input, ["<Name>", "<name>", "<str>", "=>"])
; S32 __input _ ->
    Left  (currentPos __input, ["<Name>", "<name>", "<str>", "=>"])
; S33 __input _ -> Left  (currentPos __input, ["=>"])
; S34 __input _ -> Left  (currentPos __input, ["<str>", "start"])
; S35 __input _ -> Left  (currentPos __input, ["<str>", "start"])
; S36 __input _ -> Left  (currentPos __input, ["start"])
; S37 __input _ -> Left  (currentPos __input, ["start"])
} where {
; action0 pos res =
{-# LINE  0 "<nowhere>" #-}
res
; action14 pos t =
{-# LINE  14 "gen-lr1-parser/grammar.grammar" #-}
         Term   t
; action15 pos e =
{-# LINE  15 "gen-lr1-parser/grammar.grammar" #-}
         Entity e
; action18 pos n t =
{-# LINE  18 "gen-lr1-parser/grammar.grammar" #-}
    T (Just n) t
; action19 pos n e =
{-# LINE  19 "gen-lr1-parser/grammar.grammar" #-}
    E (Just n) e
; action20 pos t =
{-# LINE  20 "gen-lr1-parser/grammar.grammar" #-}
                   T  Nothing t
; action21 pos e =
{-# LINE  21 "gen-lr1-parser/grammar.grammar" #-}
                   E  Nothing e
; action24 pos p =
{-# LINE  24 "gen-lr1-parser/grammar.grammar" #-}
    [p]
; action25 pos p ps =
{-# LINE  25 "gen-lr1-parser/grammar.grammar" #-}
     p : ps
; action28 pos points reducer =
{-# LINE  28 "gen-lr1-parser/grammar.grammar" #-}
    Clause {mark = 0, pos, points = listToArray points, reducer}
; action32 pos c =
{-# LINE  32 "gen-lr1-parser/grammar.grammar" #-}
    [c]
; action33 pos c cs =
{-# LINE  33 "gen-lr1-parser/grammar.grammar" #-}
     c : cs
; action36 pos entity type_ clauses =
{-# LINE  36 "gen-lr1-parser/grammar.grammar" #-}
    Rule {entity, type_ = Just type_, clauses}
; action37 pos entity clauses =
{-# LINE  37 "gen-lr1-parser/grammar.grammar" #-}
    Rule {entity, type_ = Nothing,    clauses}
; action40 pos r =
{-# LINE  40 "gen-lr1-parser/grammar.grammar" #-}
    [r]
; action41 pos r rs =
{-# LINE  41 "gen-lr1-parser/grammar.grammar" #-}
     r : rs
; action44 pos =
{-# LINE  44 "gen-lr1-parser/grammar.grammar" #-}
    []
; action45 pos ls =
{-# LINE  45 "gen-lr1-parser/grammar.grammar" #-}
    ls
; action48 pos t =
{-# LINE  48 "gen-lr1-parser/grammar.grammar" #-}
    [t]
; action49 pos t ts =
{-# LINE  49 "gen-lr1-parser/grammar.grammar" #-}
     t : ts
; action52 pos e =
{-# LINE  52 "gen-lr1-parser/grammar.grammar" #-}
    e
; action55 pos a s r =
{-# LINE  55 "gen-lr1-parser/grammar.grammar" #-}
    (a, s, r)
}
  
currentPos :: ([Lexeme], Pos) -> Pos
currentPos = \case
  ([],           end) -> end
  ((pos, _) : _, _)   -> pos
  
parseGrammar :: FilePath -> IO (Either LexerError (Either (Pos, [String]) ([Text], Entity, [Rule])))
parseGrammar filepath = do
  text <- Text.readFile filepath
  case lexText filepath text [":", "=", "=>", "add", "start",
                              "|"] of
    Left  err   -> pure (Left err)
    Right input -> pure (Right (__runGrammar  S0 input Nil))