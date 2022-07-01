(*
  Comment.

  (* Another comment. *)
*)

module Language.Syntax.Example where

(* Functions are recursive by default (language is lazy) *)
local def bool
  : Bool -> Int
  = \b =>
    case list of
    | Cons {x} => f x
    | Nil  => z
