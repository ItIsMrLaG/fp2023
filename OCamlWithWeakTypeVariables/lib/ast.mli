(** Copyright 2021-2023, wokuparalyzed *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** name of value *)
type name = string [@@deriving show { with_path = false }]

(** base constantes
    like: 1; 43; false; ... *)
type const =
  | CInt of int (** int constant *)
  | CBool of bool (** bool constant *)
  | CUnit (** unit constant *)
[@@deriving show { with_path = false }]

(** hard-coded binary operators
    like: +; -; !=; >; []; ... *)
type bin_op =
  | BAdd (** int addition *)
  | BSub (** int subtraction *)
  | BMul (** int multiplication *)
  | BDiv (** int division *)
  | BGT (** int > *)
  | BGE (** int >= *)
  | BLT (** int < *)
  | BLE (** int <= *)
  | BEq (** equality of two expressions *)
  | BNE (** not equality *)
  | BCons (** list cons (::) *)
  | BAnd (** logic and *)
  | BOr (** logic or *)
[@@deriving show { with_path = false }]

(** unary hard-coded operators *)
type un_op =
  | UnMin (** unary minus *)
  | UnNot (** logic not *)
[@@deriving show { with_path = false }]

(** patterns to everything *)
type pattern =
  | MPWildcard (** any value _ *)
  | MPConst of const (** constant value *)
  | MPVar of name (** any name of value *)
  | MPTuple of pattern * pattern * pattern list (** tuple pattern (a, b, c) *)
  | MPList of pattern list
  (** list pattern as filled list
      like: [a; b; c; d] where
      a, b, c, d - other patterns

      also represents blank list
      [] - LPList [] *)
  | MPHdTl of
      { head : pattern
      ; tail : pattern
      } (** [hd :: tl] pattern
            hd - noramal pattern
            tl - another list pattern *)
[@@deriving show { with_path = false }]

(** recursive flag *)
type rec_flag =
  | Rec (** recursive *)
  | NoRec (** not recursive *)
[@@deriving show { with_path = false }]

(** main expression and main programm *)
type expr =
  | EConst of const (** lenguage constant *)
  | EVar of name (** variable *)
  | EApply of expr * expr
  (** apply
      foo a b is EApply(EApply(EVar "foo", EVar "a"), EVar "b") *)
  | EBinOp of
      { op : bin_op
      ; left : expr
      ; right : expr
      }
  (** binary operator apply
      op - binary operator
      left - first argument
      right - second argument *)
  | EUnOp of
      { op : un_op
      ; arg : expr
      } (** unary operator apply
            op - unary operator
            arg - arg *)
  | EList of expr list
  (** list constant
      [e1; e2; e3] - list of e1, e2, e3 elements
      [] - blank list *)
  | ETuple of expr * expr * expr list
  (** n-tuple constant
      where n >= 2 always

      2-typle - ETuple(e1, e2, [])
      4-typle - ETuple(e1, e2, [e3; e4])

      1-tuple - never *)
  | Eite of
      { cond : expr
      ; th : expr
      ; el : expr
      }
  (** if-then-else expression
      cond - condition
      th - result of ite if cond
      el - result of ite if not cond *)
  | EFun of
      { args : pattern * pattern list
      ; expr : expr
      }
  (** fun a b c -> ... expression
      always has at least 1 argument

      patterns - names of arguments
      expr - result of function *)
  | EPattern of
      { match_expr : expr
      ; matches : (pattern * expr) * (pattern * expr) list
      }
  (** pattern-matching
      match_expr - matching expression
      matches - list of patterns ot match *)
  | ELetIn of binding
  (** let and let-in statements
      name - name of expression/expressions
      rec_flag - recursive or not
      value - binded expr
      expr - where it binded

      functions represented as EFun expr *)

and binding =
  { name : pattern
  ; rec_flag : rec_flag
  ; value : expr
  ; expr : expr
  }
[@@deriving show { with_path = false }]

type program = expr [@@deriving show { with_path = false }]

val pp_program : Format.formatter -> program -> unit

(** smart constructors *)
module Constr : sig
  val cint : int -> const
  val cbool : bool -> const
  val cunit : const
  val badd : bin_op
  val bsub : bin_op
  val bmul : bin_op
  val bdiv : bin_op
  val bgt : bin_op
  val bge : bin_op
  val blt : bin_op
  val ble : bin_op
  val beq : bin_op
  val bne : bin_op
  val bcons : bin_op
  val band : bin_op
  val bor : bin_op
  val unmin : un_op
  val unnot : un_op
  val mpwildcard : pattern
  val mpconst : const -> pattern
  val mpvar : name -> pattern
  val mptuple : pattern -> pattern -> pattern list -> pattern
  val mplist : pattern list -> pattern
  val mphdtl : pattern -> pattern -> pattern
  val econst : const -> expr
  val evar : name -> expr
  val eapply : expr -> expr -> expr
  val ebinop : bin_op -> expr -> expr -> expr
  val eunop : un_op -> expr -> expr
  val elist : expr list -> expr
  val etuple : expr -> expr -> expr list -> expr
  val eite : expr -> expr -> expr -> expr
  val efun : pattern * pattern list -> expr -> expr
  val epattern : expr -> (pattern * expr) * (pattern * expr) list -> expr
  val eletin : pattern -> rec_flag -> expr -> expr -> expr
end
