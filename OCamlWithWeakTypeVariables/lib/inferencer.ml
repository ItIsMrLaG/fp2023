(** Copyright 2021-2023, wokuparalyzed *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Types

module Constr = struct
  let gr_int = GrInt
  let gr_bool = GrBool
  let gr_unit = GrUnit
  let ty_ground t = TyGround t
  let ty_arrow left right = TyArrow (left, right)
  let ty_tuple t1 t2 ts = TyTuple (t1, t2, ts)
  let ty_list t = TyList t
  let ty_var n = TyVar n
  let int_t = gr_int |> ty_ground
  let bool_t = gr_bool |> ty_ground
  let unit_t = gr_unit |> ty_ground
end

module State = struct
  type 'a t = int -> int * ('a, string) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun monad f state ->
    let last, result = monad state in
    match result with
    | Error e -> last, Error e
    | Ok value -> f value last
  ;;

  let ( let* ) = ( >>= )
  let fail error state = state, Base.Result.fail error
  let return value last = last, Base.Result.return value

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun monad f state ->
    match monad state with
    | state, Ok x -> state, Ok (f x)
    | state, Error e -> state, Error e
  ;;

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run monad = snd (monad 0)
end

type id = int

module Type = struct
  let occurs_in v =
    let rec helper f = function
      | TyVar id -> f (id = v)
      | TyArrow (left, right) -> helper (( || ) (helper f left)) right
      | TyTuple (t1, t2, ts) ->
        let ts = t1 :: t2 :: ts in
        List.fold_left (fun f t -> ( || ) (helper f t)) f ts false
      | TyList t -> helper f t
      | TyGround _ -> false
    in
    helper (fun id -> id)
  ;;

  let free_vars =
    let empty = Base.Set.empty (module Base.Int) in
    let rec helper acc = function
      | TyVar n -> Base.Set.add acc n
      | TyArrow (left, right) -> helper (helper acc left) right
      | TyTuple (t1, t2, ts) ->
        let ts = t1 :: t2 :: ts in
        List.fold_right (fun t -> t |> helper empty |> Base.Set.union) ts acc
      | TyList t -> helper acc t
      | TyGround _ -> acc
    in
    helper empty
  ;;
end

module Subst = struct
  open State
  open Constr

  type t = (id, ty, Base.Int.comparator_witness) Base.Map.t

  let empty : t = Base.Map.empty (module Base.Int)

  let mapping key value =
    if Type.occurs_in key value then fail "Occurse check" else return (key, value)
  ;;

  let singleton k v =
    let* k, v = mapping k v in
    Base.Map.update empty k ~f:(fun _ -> v) |> return
  ;;

  let find = Base.Map.find
  let remove = Base.Map.remove

  let apply sb =
    let rec helper = function
      | TyVar n ->
        (match find sb n with
         | None -> ty_var n
         | Some x -> x)
      | TyArrow (left, right) -> ty_arrow (helper left) (helper right)
      | TyTuple (t1, t2, ts) -> ty_tuple (helper t1) (helper t2) (List.map helper ts)
      | TyList t -> t |> helper |> ty_list
      | gr -> gr
    in
    helper
  ;;

  let rec unify left right =
    match left, right with
    | TyGround left, TyGround right when left = right -> return empty
    | TyVar left, TyVar right when left = right -> return empty
    | TyVar n, t | t, TyVar n -> singleton n t
    | TyArrow (left, right), TyArrow (left', right') ->
      let* left_sb = unify left left' in
      let* right_sb = unify (apply left_sb right) (apply left_sb right') in
      compose left_sb right_sb
    | TyTuple (t1, t2, ts), TyTuple (t1', t2', ts') ->
      (match Base.List.zip (t1 :: t2 :: ts) (t1' :: t2' :: ts') with
       | Base.List.Or_unequal_lengths.Unequal_lengths -> fail "Unify fail"
       | Base.List.Or_unequal_lengths.Ok combined ->
         List.fold_right
           (fun (left, right) sb ->
             let* sb = sb in
             let* sb' = unify left right in
             compose sb' sb)
           combined
           (return empty))
    | TyList left, TyList right -> unify left right
    | _ -> fail "Unify fail"

  and extend k v sb =
    match find sb k with
    | None ->
      let* snd_sb = apply sb v |> singleton k in
      Base.Map.fold_right sb ~init:(return snd_sb) ~f:(fun ~key:k ~data:v acc ->
        let* acc = acc in
        let* k, v = apply snd_sb v |> mapping k in
        Base.Map.update acc k ~f:(fun _ -> v) |> return)
    | Some older_v -> unify v older_v >>= compose sb

  and compose left_sb right_sb =
    Base.Map.fold_right right_sb ~init:(return left_sb) ~f:(fun ~key ~data acc ->
      acc >>= extend key data)

  and compose_all sbs =
    Base.List.fold_left sbs ~init:(return empty) ~f:(fun acc subst ->
      let* acc = acc in
      compose acc subst)
  ;;
end

module TypeEnv = struct
  type scheme = ty * (id, Base.Int.comparator_witness) Base.Set.t
  type t = (name, scheme, Base.String.comparator_witness) Base.Map.t

  let free_vars = function
    | t, sch -> Base.Set.diff (Type.free_vars t) sch
  ;;

  let apply sb (t, sch) =
    let sch' = Base.Set.fold sch ~init:sb ~f:Subst.remove in
    Subst.apply sch' t, sch
  ;;

  let extend env id sch = Base.Map.update env id ~f:(fun _ -> sch)
  let empty : t = Base.Map.empty (module Base.String)

  let free_vars env =
    Base.Map.fold
      ~init:(Base.Set.empty (module Base.Int))
      ~f:(fun ~key:_ ~data acc -> free_vars data |> Base.Set.union acc)
      env
  ;;

  let apply sb env = Base.Map.map env ~f:(apply sb)
end

open State
open Constr

let unify = Subst.unify
let fresh_var = fresh >>| ty_var

let instantiate (t, set) =
  Base.Set.fold_right set ~init:(return t) ~f:(fun name typ ->
    let* typ = typ in
    let* f' = fresh_var in
    let* s = Subst.singleton name f' in
    Subst.apply s typ |> return)
;;

let generalize env t =
  let free = Base.Set.diff (Type.free_vars t) (TypeEnv.free_vars env) in
  t, free
;;

let lookup_env e map =
  match Base.Map.find map e with
  | None -> fail "No variable"
  | Some (t, set) ->
    let* t = instantiate (t, set) in
    return (Subst.empty, t)
;;

module Patterns = struct
  let whildcard =
    let* fresh = fresh_var in
    return (None, fresh)
  ;;

  let var name =
    let* fresh = fresh_var in
    return (Some [ name, fresh ], fresh)
  ;;

  let const = function
    | CInt _ -> return (None, int_t)
    | CBool _ -> return (None, bool_t)
    | CUnit -> return (None, unit_t)
  ;;

  let tuple infer p1 p2 ps =
    let* n1, t1 = infer p1 in
    let* n2, t2 = infer p2 in
    let* ns, ts =
      List.fold_right
        (fun p acc ->
          let* ns, ts = acc in
          let* n, t = infer p in
          return (n :: ns, t :: ts))
        ps
        (return ([], []))
    in
    let ns =
      n1 :: n2 :: ns
      |> List.filter_map (fun id -> id)
      |> List.concat
      |> function
      | [] -> None
      | ns -> Some ns
    in
    return (ns, ty_tuple t1 t2 ts)
  ;;

  let list infer ps =
    let* fresh = fresh_var in
    let* ns, ts =
      List.fold_right
        (fun p acc ->
          let* ns, ts = acc in
          let* n, t = infer p in
          return (n :: ns, t :: ts))
        ps
        (return ([], []))
    in
    let* sb =
      List.fold_left
        (fun acc t ->
          let* sb = acc in
          let* sb' = unify fresh t in
          Subst.compose sb sb')
        (return Subst.empty)
        ts
    in
    let t = fresh |> Subst.apply sb |> ty_list in
    let ns =
      ns
      |> List.filter_map (fun id -> id)
      |> List.flatten
      |> List.map (fun (n, t) -> n, Subst.apply sb t)
      |> function
      | [] -> None
      | ns -> Some ns
    in
    return (ns, t)
  ;;

  let hdtl infer head tail =
    let* hdns, hdt = infer head in
    let* tlns, tlt = infer tail in
    let* sb = unify (ty_list hdt) tlt in
    let t = Subst.apply sb hdt |> ty_list in
    let ns =
      Option.to_list hdns @ Option.to_list tlns
      |> List.flatten
      |> List.map (fun (n, t) -> n, Subst.apply sb t)
      |> function
      | [] -> None
      | ns -> Some ns
    in
    return (ns, t)
  ;;

  let rec pattern_infer = function
    | MPWildcard -> whildcard
    | MPConst c -> const c
    | MPVar name -> var name
    | MPTuple (p1, p2, ps) -> tuple pattern_infer p1 p2 ps
    | MPList ps -> list pattern_infer ps
    | MPHdTl { head; tail } -> hdtl pattern_infer head tail
  ;;
end

let bootstrap env = function
  | None -> env
  | Some ns ->
    List.fold_left
      (fun env (n, t) -> TypeEnv.extend env n (t, Base.Set.empty (module Base.Int)))
      env
      ns
;;

let rec infer_expr =
  let rec helper env = function
    | EConst (CInt _) -> return (Subst.empty, int_t)
    | EConst (CBool _) -> return (Subst.empty, bool_t)
    | EConst CUnit -> return (Subst.empty, unit_t)
    | EVar name -> lookup_env name env
    | EFun { args; expr } ->
      let hd, tl = args in
      let* ns, hd_t = Patterns.pattern_infer hd in
      let env' = bootstrap env ns in
      let* sb, tl_t =
        match tl with
        | [] -> helper env' expr
        | hd :: tl -> helper env' (EFun { args = hd, tl; expr })
      in
      let t = ty_arrow (Subst.apply sb hd_t) tl_t in
      return (sb, t)
    | EUnOp { op = UnMin; arg } ->
      let* sb, t = helper env arg in
      let* sb' = unify t int_t in
      let* sb'' = Subst.compose sb sb' in
      return (sb'', int_t)
    | EUnOp { op = UnNot; arg } ->
      let* sb, t = helper env arg in
      let* sb' = unify t bool_t in
      let* sb'' = Subst.compose sb sb' in
      return (sb'', bool_t)
    | EBinOp { op; left; right } ->
      let* lsb, lt = helper env left in
      let* rsb, rt = helper env right in
      (match op with
       | BAdd | BSub | BMul | BDiv ->
         let* sb' = unify lt int_t in
         let* sb'' = unify rt int_t in
         let* fin_sb = Subst.compose_all [ sb'; sb''; lsb; rsb ] in
         return (fin_sb, int_t)
       | BEq | BNE | BGT | BGE | BLT | BLE ->
         let* sb' = unify lt rt in
         let* fin_sb = Subst.compose_all [ sb'; lsb; rsb ] in
         return (fin_sb, bool_t)
       | BAnd | BOr ->
         let* sb' = unify lt bool_t in
         let* sb'' = unify rt bool_t in
         let* fin_sb = Subst.compose_all [ sb'; sb''; lsb; rsb ] in
         return (fin_sb, bool_t)
       | BCons ->
         let* fresh = fresh_var in
         let* sb' = unify lt fresh in
         let* sb'' = unify rt (ty_list fresh) in
         let* fin_sb = Subst.compose_all [ sb'; sb''; lsb; rsb ] in
         return (fin_sb, ty_list fresh))
    | EApply (left, right) ->
      let* lsb, lt = helper env left in
      let* rsb, rt = helper (TypeEnv.apply lsb env) right in
      let* fresh = fresh_var in
      let* sb' = unify (ty_arrow rt fresh) (Subst.apply rsb lt) in
      let t = Subst.apply sb' fresh in
      let* fin_sb = Subst.compose_all [ lsb; rsb; sb' ] in
      return (fin_sb, t)
    | Eite { cond; th; el } ->
      let* csb, ct = helper env cond in
      let* tsb, tt = helper env th in
      let* esb, et = helper env el in
      let* sb' = unify ct bool_t in
      let* sb'' = unify tt et in
      let* fin_sb = Subst.compose_all [ csb; tsb; esb; sb'; sb'' ] in
      return (fin_sb, Subst.apply fin_sb tt)
    | EList list ->
      (match list with
       | [] ->
         let* fresh = fresh_var in
         return (Subst.empty, ty_list fresh)
       | hd :: tl ->
         let* hdsb, hdt = helper env hd in
         let rec sblist sb = function
           | [] -> return sb
           | e :: es ->
             let* esb, et = helper env e in
             let* sb' = unify et hdt in
             let* sb'' = Subst.compose_all [ sb; esb; sb' ] in
             sblist sb'' es
         in
         let* fin_sb = sblist hdsb tl in
         return (fin_sb, Subst.apply fin_sb hdt |> ty_list))
    | ETuple (e1, e2, es) ->
      let rec sbtuple sb = function
        | [] -> return (sb, [])
        | e :: es ->
          let* esb, et = helper env e in
          let* sb' = Subst.compose sb esb in
          let* sb'', est = sbtuple sb' es in
          return (sb'', et :: est)
      in
      let* fin_sb, ts = e1 :: e2 :: es |> sbtuple Subst.empty in
      let* t =
        ts
        |> List.map (Subst.apply fin_sb)
        |> function
        | t1 :: t2 :: ts -> ty_tuple t1 t2 ts |> return
        | _ -> fail "Infer fail"
      in
      return (fin_sb, t)
    | EPattern { match_expr; matches = hd, tl } ->
      let* msb, mt = helper env match_expr in
      let* fresh = fresh_var in
      let* sb', t =
        List.fold_left
          (fun acc (p, e) ->
            let* sb, rez_t = acc in
            let* ns, t = Patterns.pattern_infer p in
            let env'' = bootstrap env ns in
            let* sb'' = unify mt t in
            let* esb, et = helper env'' e in
            let* sb''' = unify et rez_t in
            let* fin_sb = Subst.compose_all [ sb; sb''; sb'''; esb ] in
            return (fin_sb, et))
          (return (Subst.empty, fresh))
          (hd :: tl)
      in
      let* fin_sb = Subst.compose sb' msb in
      return (fin_sb, Subst.apply fin_sb t)
    | ELetIn { name; rec_flag; value; expr } ->
      let* names, t =
        Patterns.pattern_infer name
        >>| function
        | None, t -> [], t
        | Some names, t -> names, t
      in
      let env' =
        match rec_flag with
        | NoRec -> env
        | Rec ->
          List.fold_left
            (fun env (n, t) -> TypeEnv.extend env n (t, Base.Set.empty (module Base.Int)))
            env
            names
      in
      let* vsb, vt = infer_expr env' value in
      let* vsb' = unify (Subst.apply vsb t) vt in
      let* vsb'' = Subst.compose vsb vsb' in
      let env'' = TypeEnv.apply vsb'' env in
      let env''' =
        List.fold_left
          (fun env (n, t) ->
            t |> Subst.apply vsb'' |> generalize env |> TypeEnv.extend env n)
          env''
          names
      in
      let* esb, et = infer_expr env''' expr in
      let* fin_sb = Subst.compose vsb'' esb in
      return (fin_sb, Subst.apply fin_sb et)
  in
  helper
;;

(** infer single expression without contex *)
let common_env =
  let empty = TypeEnv.empty in
  let empty_set = Base.Set.empty (module Base.Int) in
  let fs = [ "print_int", ty_arrow int_t unit_t; "print_bool", ty_arrow bool_t unit_t ] in
  List.fold_left (fun env (n, t) -> TypeEnv.extend env n (t, empty_set)) empty fs
;;

(** main infer with baselib function types *)
let run_inferencer expression =
  expression |> infer_expr common_env |> run |> Result.map (fun _ -> ())
;;
