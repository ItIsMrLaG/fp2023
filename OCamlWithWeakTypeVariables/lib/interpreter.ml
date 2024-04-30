(** Copyright 2021-2023, wokuparalyzed *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module Interpreter = struct
  type std_fun =
    { name : string
    ; arity : int
    ; action : value list -> value
    ; applied : value list
    }

  (** main return value *)
  and value =
    | Int of int
    | Bool of bool
    | List of value list
    | Tuple of value list
    | Fun of
        { args : pattern * pattern list
        ; expr : expr
        ; applied : value list
        }
    | Stdfun of std_fun
    | Unit
    | Err of string

  module Constr = struct
    let vint i = Int i
    let vbool f = Bool f
    let vlist vs = List vs
    let vtuple vs = Tuple vs
    let vfun args expr applied = Fun { args; expr; applied }
    let verr msg = Err msg
    let vunit = Unit
    let stdfun name arity action = { name; arity; action; applied = [] }
  end

  module StdFuns = struct
    include Constr

    let rec value_to_string = function
      | Int i -> string_of_int i
      | Bool b -> string_of_bool b
      | List elements ->
        let elements = List.map value_to_string elements in
        let s = String.concat ", " elements in
        Format.sprintf "[%s]" s
      | Tuple elements ->
        let elements = List.map value_to_string elements in
        let s = String.concat ", " elements in
        Format.sprintf "(%s)" s
      | Fun _ -> "<fun>"
      | Unit -> "()"
      | Stdfun _ -> "<std fun>"
      | Err e -> Format.sprintf "<Error \"%s\">" e
    ;;

    (** print value *)
    let print x = print_endline @@ value_to_string x

    let print_cmn = function
      | [ x ] ->
        print x;
        vunit
      | _ -> verr "Print exception"
    ;;

    let std_funs =
      [ stdfun "print_int" 1 print_cmn
      ; stdfun "print_bool" 1 print_cmn
      ; stdfun "print" 1 print_cmn
      ]
    ;;
  end

  module SMap = Map.Make (String)

  type context = { bindings : value SMap.t }

  let bind t f ctx =
    let a, ctx = t ctx in
    let b, ctx = f a ctx in
    b, ctx
  ;;

  let empty_context =
    { bindings =
        List.fold_left
          (fun m (p : std_fun) -> SMap.add p.name (Stdfun p) m)
          SMap.empty
          StdFuns.std_funs
    }
  ;;

  let ( let* ) = bind
  let ( >>= ) = bind
  let return a ctx = a, ctx

  let get_val name ctx =
    match SMap.find_opt name ctx.bindings with
    | Some e -> e
    | None -> Constr.verr @@ Format.sprintf "\"%S\" not bound" name
  ;;
end

open Interpreter
open Constr

let zip l1 l2 =
  match Base.List.zip l1 l2 with
  | Base.List.Or_unequal_lengths.Unequal_lengths -> None
  | Base.List.Or_unequal_lengths.Ok combined -> Some combined
;;

let equals a c =
  match a, c with
  | CInt i, Int j -> i = j
  | CBool b, Bool d -> b = d
  | CUnit, Unit -> true
  | _ -> false
;;

let rec check_pattern pat arg m =
  let ( let* ) = Option.bind in
  match pat, arg with
  | MPWildcard, _ -> Some m
  | MPConst c, arg when equals c arg -> Some m
  | MPVar n, arg -> Some (SMap.add n arg m)
  | MPTuple (p1, p2, ps), Tuple vs ->
    let ps = p1 :: p2 :: ps in
    let* zip = zip ps vs in
    List.fold_left
      (fun m (f, s) ->
        let* m = m in
        (check_pattern f s) m)
      (Some m)
      zip
  | MPList ps, List vs ->
    let* zip = zip ps vs in
    List.fold_left
      (fun m (f, s) ->
        let* m = m in
        (check_pattern f s) m)
      (Some m)
      zip
  | MPHdTl { head; tail }, List (hd :: tl) ->
    let res = check_pattern head hd m in
    vlist tl |> check_pattern tail |> Option.bind res
  | _, _ -> None
;;

let rec interpret_expression expr =
  match expr with
  | EConst (CInt i) -> return (vint i)
  | EConst (CBool f) -> return (vbool f)
  | EConst CUnit -> return vunit
  | EVar v -> fun c -> return (get_val v c) c
  | EFun { args; expr } -> return @@ vfun args expr []
  | ELetIn bnd -> interpret_letin bnd
  | ETuple (e1, e2, exprs) -> interpret_tuple (e1 :: e2 :: exprs)
  | EList exprs -> interpret_list exprs
  | EApply (fn, arg) -> interpret_apply fn arg
  | EBinOp { op; left; right } -> interpret_binop op left right
  | EUnOp { op : un_op; arg : expr } -> interpret_unop op arg
  | Eite { cond; th; el } -> interpret_ite cond th el
  | EPattern { match_expr; matches } -> interpret_match match_expr matches

and interpret_ite cond th el =
  interpret_expression cond
  >>= function
  | Bool b when b -> interpret_expression th
  | Bool _ -> interpret_expression el
  | _ -> return @@ verr "Condition in if was not bool"

and interpret_unop op arg =
  let* arg = interpret_expression arg in
  match op, arg with
  | UnMin, Int i -> return @@ vint (-i)
  | UnNot, Bool b -> return @@ vbool (not b)
  | _ -> return @@ verr "Operation not supported"

and interpret_binop op l r =
  let* l = interpret_expression l in
  let* r = interpret_expression r in
  return
    (match op, l, r with
     | BAdd, Int x, Int y -> vint (x + y)
     | BSub, Int x, Int y -> vint (x - y)
     | BMul, Int x, Int y -> vint (x * y)
     | BDiv, Int x, Int y -> vint (x / y)
     | BGT, Int x, Int y -> vbool (x > y)
     | BGE, Int x, Int y -> vbool (x >= y)
     | BLT, Int x, Int y -> vbool (x < y)
     | BLE, Int x, Int y -> vbool (x <= y)
     | BEq, Int x, Int y -> vbool (x = y)
     | BNE, Int x, Int y -> vbool (x <> y)
     | BCons, x, List y -> vlist (x :: y)
     | BAnd, Bool x, Bool y -> vbool (x && y)
     | BOr, Bool x, Bool y -> vbool (x || y)
     | _ -> verr "Operation not supported")

and interpret_letin bnd ctx =
  let v, _ = interpret_expression bnd.value ctx in
  match check_pattern bnd.name v ctx.bindings with
  | Some m -> interpret_expression bnd.expr { bindings = m }
  | None -> return (verr "Can't match let in right and left hand side") ctx

and interpret_tuple exprs ctx =
  let evaluated = List.map (fun e -> interpret_expression e ctx |> fst) exprs in
  return (vtuple evaluated) ctx

and interpret_list exprs ctx =
  let evaluated = List.map (fun e -> interpret_expression e ctx) exprs in
  let evaluated = List.map fst evaluated in
  return (vlist evaluated) ctx

and interpret_apply fn arg =
  let apply_inner pats args expr ctx =
    let ( let* ) = Option.bind in
    let* zip = zip pats args in
    let* m =
      List.fold_left
        (fun m (f, s) ->
          let* m = m in
          check_pattern f s m)
        (Some ctx.bindings)
        zip
    in
    let ctx = { bindings = m } in
    Some (interpret_expression expr ctx)
  in
  let* fn = interpret_expression fn in
  let* arg = interpret_expression arg in
  match fn with
  | Stdfun f when f.arity = List.length (arg :: f.applied) ->
    return @@ f.action (arg :: f.applied)
  | Stdfun f -> return @@ Stdfun { f with applied = arg :: f.applied }
  | Fun { args; expr; applied } ->
    let elt, elts = args in
    (match elt :: elts, arg :: applied with
     | pats, args when List.length args = List.length pats ->
       fun ctx ->
         (match apply_inner pats (List.rev args) expr ctx with
          | Some (v, _) -> return v ctx
          | None -> return (verr "Unapplyable expr") ctx)
     | _, applied -> return @@ vfun args expr applied)
  | _ -> return @@ verr "Unapplyable expr"

and interpret_match expr pats ctx =
  let p, ats = pats in
  let pats = p :: ats in
  let check_pattern_ext p a m =
    let p, e = p in
    Option.bind (check_pattern p a m) (fun p -> Some (p, e))
  in
  let expr = fst (interpret_expression expr ctx) in
  let found_case = List.find_map (fun p -> check_pattern_ext p expr ctx.bindings) pats in
  match found_case with
  | Some (m, e) -> interpret_expression e { bindings = m }
  | None -> return (verr "Mathing case not found") ctx
;;

(** main interpreter *)
let interpret expr = interpret_expression expr empty_context |> fst
