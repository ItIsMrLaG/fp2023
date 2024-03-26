(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Errorinter

(** main program *)
type program = expr list [@@deriving show { with_path = false }]

type value =
  | VString of string
  | VBool of bool
  | VInt of int
  | VList of value list
  | VTuple of value list
  | VFun of pattern * expr * (name * value) list
  | VLetWAPat of name * value
  | VLetAPat of name list * value
  | VCases of name
  | VNone
[@@deriving show { with_path = false }]

module type MonadFail = sig
  include Base.Monad.S2

  val run : ('a, 'e) t -> ok:('a -> ('b, 'e) t) -> err:('e -> ('b, 'e) t) -> ('b, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

let is_constr = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

type environment = (name, value, String.comparator_witness) Map.t

module Environment (M : MonadFail) = struct
  open M

  let empty = Map.empty (module Base.String)

  let find_val map key =
    if String.length key = 0
    then fail (StringOfLengthZero key)
    else (
      match Map.find map key with
      | Some value -> return value
      | None when is_constr @@ String.get key 0 -> fail (UnboundConstructor key)
      | _ -> fail (UnboundValue key))
  ;;

  let add_bind map key value = Map.update map key ~f:(fun _ -> value)

  let add_binds map binds =
    List.fold ~f:(fun map (k, v) -> add_bind map k v) ~init:map binds
  ;;
end

module Interpret (M : MonadFail) = struct
  open M
  open Environment (M)

  let rec bind_fun_params ?(env = empty) =
    let bind_pat_list patl argl =
      let binded_list =
        List.fold2
          patl
          argl
          ~f:(fun acc pat arg ->
            let* acc = acc in
            let* binding = bind_fun_params ~env (pat, arg) in
            return (acc @ binding))
          ~init:(return [])
      in
      match binded_list with
      | Ok v -> v
      | _ -> fail MatchFailure
    in
    function
    | Wild, _ -> return []
    | Const c, app_arg ->
      (match c, app_arg with
       | CBool b1, VBool b2 when Bool.equal b1 b2 -> return []
       | CInt i1, VInt i2 when i1 = i2 -> return []
       | CString s1, VString s2 when String.equal s1 s2 -> return []
       | _ -> fail Unreachable)
    | Var var, app_arg -> return [ var, app_arg ]
    | Tuple pl, VTuple vl | List pl, VList vl -> bind_pat_list pl vl
    | Case (acase_id, _), value_to_match ->
      let* apat = find_val env acase_id in
      (match apat with
       | VLetAPat (_, VFun (apat_arg, apat_expr, apat_env)) ->
         let* bind_matching_val = bind_fun_params ~env (apat_arg, value_to_match) in
         let* eval_res_apat =
           eval apat_expr (add_binds (add_binds empty apat_env) bind_matching_val)
         in
         (match eval_res_apat with
          | VCases a when String.( = ) a acase_id -> return []
          | VInt _ | VString _ | VList _ | VTuple _ | VBool _ ->
            (match apat_arg with
             | Var name -> return [ name, eval_res_apat ]
             | _ -> fail Unreachable)
          | _ -> fail MatchFailure)
       | _ -> fail MatchFailure)
    | _ -> fail MatchFailure

  and eval expr env =
    match expr with
    | ConstExpr v -> return @@ inter_const v
    | BinExpr (op, l, r) -> inter_binary op l r env
    | VarExpr id -> find_val env id
    | ListExpr l -> inter_list l env
    | TupleExpr t -> inter_tuple t env
    | IfExpr (cond, e_then, e_else) -> inter_if cond e_then e_else env
    | FunExpr (pat, expr) -> return @@ VFun (pat, expr, Map.to_alist env)
    | AppExpr (func, arg) -> inter_app func arg env
    | LetExpr (is_rec, name, body) -> inter_let is_rec name body env
    | MatchExpr (expr_match, cases) -> inter_match expr_match cases env
    | CaseExpr constr_id -> return @@ VCases constr_id
    | LetActExpr (act_name, body) -> inter_act_let act_name body env

  and inter_const = function
    | CBool b -> VBool b
    | CInt i -> VInt i
    | CString s -> VString s

  and inter_act_let pat_name body env =
    let* fun_pat = eval body env in
    return @@ VLetAPat (pat_name, fun_pat)

  and inter_list l env =
    let* eval_list = all (List.map l ~f:(fun expr -> eval expr env)) in
    return @@ VList eval_list

  and inter_tuple t env =
    let* eval_list = all (List.map t ~f:(fun expr -> eval expr env)) in
    return @@ VTuple eval_list

  and inter_binary op l r env =
    let* rigth_val = eval r env in
    let* left_val = eval l env in
    match op, left_val, rigth_val with
    | Div, VInt _, VInt 0 -> fail DivisionByZero
    | Mod, VInt _, VInt 0 -> fail DivisionByZero
    | Add, VInt l, VInt r -> return @@ VInt (l + r)
    | Sub, VInt l, VInt r -> return @@ VInt (l - r)
    | Mul, VInt l, VInt r -> return @@ VInt (l * r)
    | Div, VInt l, VInt r -> return @@ VInt (l / r)
    | Mod, VInt l, VInt r -> return @@ VInt (l % r)
    | Less, VInt l, VInt r -> return @@ VBool (l < r)
    | LEq, VInt l, VInt r -> return @@ VBool (l <= r)
    | Gre, VInt l, VInt r -> return @@ VBool (l > r)
    | GEq, VInt l, VInt r -> return @@ VBool (l >= r)
    | Eq, VInt l, VInt r -> return @@ VBool (l = r)
    | NEq, VInt l, VInt r -> return @@ VBool (l <> r)
    | _ -> fail TypeError

  and inter_if cond e_then e_else env =
    let* cond_branch = eval cond env in
    match cond_branch with
    | VBool b -> eval (if b then e_then else e_else) env
    | _ -> fail TypeError

  and inter_let is_rec name body env =
    if is_rec
    then
      let* func_body = eval body env in
      return @@ VLetWAPat (name, func_body)
    else eval body env

  and inter_app func arg env =
    let* fun_to_apply = eval func env in
    match fun_to_apply with
    | VFun (pat, expr, fun_env) ->
      let* arg_to_apply = eval arg env in
      let* res = bind_fun_params ~env (pat, arg_to_apply) in
      eval expr (add_binds (add_binds empty fun_env) res)
    | VLetWAPat (name, VFun (pat, expr, fun_env)) ->
      let* arg_to_apply = eval arg env in
      let* res = bind_fun_params ~env (pat, arg_to_apply) in
      eval
        expr
        (add_binds
           (add_bind
              (add_binds empty fun_env)
              name
              (VLetWAPat (name, VFun (pat, expr, fun_env))))
           res)
    | _ -> fail TypeError

  and inter_match expr_match cases env =
    let* val_match = eval expr_match env in
    let rec eval_match = function
      | (pat, expr) :: cases ->
        run
          (bind_fun_params ~env (pat, val_match))
          ~ok:(fun binds -> eval expr (add_binds env binds))
          ~err:(fun _ -> eval_match cases)
      | [] -> fail Unreachable
    in
    eval_match cases
  ;;

  let eval_program (program : expr list) : (value, error_inter) t =
    let rec helper env = function
      | h :: [] -> eval h env
      | [] -> fail EmptyProgram
      | h :: tl ->
        let* eval_h = eval h env in
        let eval_env =
          match h with
          | LetExpr (_, f, _) -> add_bind env f eval_h
          | LetActExpr (fl, _) ->
            List.fold_right ~f:(fun h acc -> add_bind acc h eval_h) ~init:env fl
          | _ -> env
        in
        helper eval_env tl
    in
    helper empty program
  ;;
end

module InterpretResult = Interpret (struct
    include Result

    let run x ~ok ~err =
      match x with
      | Ok v -> ok v
      | Error e -> err e
    ;;

    let ( let* ) monad f = bind monad ~f
  end)

let eval_program = InterpretResult.eval_program
