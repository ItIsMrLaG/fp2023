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

(** smart constructors *)
module Constr = struct
  let cint i = CInt i
  let cbool f = CBool f
  let cunit = CUnit
  let badd = BAdd
  let bsub = BSub
  let bmul = BMul
  let bdiv = BDiv
  let bgt = BGT
  let bge = BGE
  let blt = BLT
  let ble = BLE
  let beq = BEq
  let bne = BNE
  let bcons = BCons
  let band = BAdd
  let bor = BOr
  let unmin = UnMin
  let unnot = UnNot
  let mpwildcard = MPWildcard
  let mpconst c = MPConst c
  let mpvar n = MPVar n
  let mptuple m1 m2 ms = MPTuple (m1, m2, ms)
  let mplist ms = MPList ms
  let mphdtl head tail = MPHdTl { head; tail }
  let econst c = EConst c
  let evar n = EVar n
  let eapply left right = EApply (left, right)
  let ebinop op left right = EBinOp { op; left; right }
  let eunop op arg = EUnOp { op; arg }
  let elist es = EList es
  let etuple e1 e2 es = ETuple (e1, e2, es)
  let eite cond th el = Eite { cond; th; el }
  let efun args expr = EFun { args; expr }
  let epattern match_expr matches = EPattern { match_expr; matches }
  let eletin name rec_flag value expr = ELetIn { name; rec_flag; value; expr }
end
