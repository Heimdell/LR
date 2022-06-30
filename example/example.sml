(*
  Comment.

  (* Another comment. *)
*)

module Language.Syntax.Example where

(* Import `Data.List` as `List` qualified *)
use Data.List

(* Import with rename. *)
use Data.Pair as Tuple

(* Import some stuff unqualified (and rest qualified under rename). *)
use Data.List as List { map, fold, Cons, List }

(* Import some stuff unqualified. *)
use Data.List { map, Nil }

(* Declare empty type *)
type Void

(* Declare empty type w/params *)
type Void1 (a : (T -> T) -> T) b

(* Declare unit type *)
local type Unit =
  | Unit { }

(* Declare parametrised type *)
type List a =
  | Cons { head : a, tail : List a }
  | Nil  { }

(* Declare HKT *)
type Free (f : T -> T) x =
  | Free { run : f (Free f a) }
  | Pure { run : a }

(* Declare local, unexported type *)
local type Pair a b =
  | Pair { fst : a, snd : b }

type Map k v =
  | Assocs { assocs : List (Tuple k v) }

(* All toplevel defs are annotated *)
def one   : Int    = 1
def str   : String = "2"
def id    : a -> a = \x => x  (* lambda *)
def call  : Int    = id 1
def qname : Int    = Foo.Bar.length
def annot : Int    = (1 : Int)
def get   : Int    = Pair { fst = 1, snd = "2"}.fst

(* Funny names are allowed, too *)
def get-1 : Int    = Pair { fst = 1, snd = "2"}.fst
def upd!  : Int    = Pair { fst = 1, snd = "2"} with { fst = "3" }
def get'  : Int    = let p = Pair { fst = 1, snd = "2"} in p.fst
def upd?  : Int    = let p = Pair { fst = 1, snd = "2"} in p with { fst = "3" }

(* Functions are recursive by default (language is lazy) *)
local def fold
  : (a -> z -> z) -> z -> List a -> z
  = \f, z, list =>
    case list of
    | Cons { head = x, tail } => f x (fold f z tail)
    | Nil  {}                 => z

(* Lambda args can be annontated. *)
local def annotated-lambda
  : Int -> (Int -> Int) -> Int
  = \x, f : Int -> Int => f x

(* EmptyCase *)
def absurd
  : Void -> a
  = \abs =>
    case abs of { }

(*
  Can pattern-match over unions.
  Constructors are always "fully applied".
*)
def filter
  : (a -> Bool) -> List a -> List a
  = \pred, list =>
    case list of
    | Cons { head, tail } when pred head =>
      Cons { head, tail = filter pred tail }

    | Cons { tail } =>
      filter pred tail

    | Nil {} => Nil {}

(* Can match over constants. *)
def fib : Int -> Int = \n =>
  case n of
  | 0 => 0
  | 1 => 1
  | n => add (fib (sub n 1)) (sub (n 2))

(* Lists have nicer syntax. *)
def fold_1
  : (a -> z -> z) -> z -> List a -> z
  = \f, z, list =>
    case list of
    | [x, ... xs] => f x (fold f z tail)
    | []          => z

(* In expression you can spread at any point. *)
def append
  : List a -> List a -> List a
  = \xs, ys => [... xs, ... ys]

(* Branches can have when-blocks. *)
def filter?
  : (a -> Bool) -> List a -> List a
  = \pred, list =>
    case list of
    | [x, ...xs] when pred x => [x, ...filter pred xs]
    | [x, ...xs]             => filter pred xs
    | []                     => []

def filter!
  : (a -> Bool) -> List a -> List a
  = \pred =>
    (* Let-expressions are still there. *)
    let go = \list =>
      case list of
      | [x, ...xs] when pred x => [x, ...go xs]
      | [x, ...xs]             => go xs
      | []                     => []
    in
      go

def filter!?
  : (a -> Bool) -> List a -> List a
  (* @go is a binder for fixpoint (see prev func). *)
  = \pred, @go, list =>
      case list of
      | [x, ...xs] when pred x => [x, ...go xs]
      | [x, ...xs]             => go xs
      | []                     => []
