(** Copyright 2021-2023, wokuparalyzed *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlWithWeakTypeVariables
open Ast

(* Parser tests *)
open Parser

let test_parse s =
  match parse s with
  | Result.Ok ast -> Format.printf "%a\n%!" Ast.pp_program ast
  | Error _ -> Format.printf "Some error"
;;

let%expect_test _ =
  test_parse {|3|};
  [%expect {|(EConst (CInt 3))|}]
;;

let%expect_test _ =
  test_parse {|1 + 2 + 3 * 4 / 1 + 2|};
  [%expect
    {|
EBinOp {op = BAdd;
  left =
  EBinOp {op = BAdd;
    left =
    EBinOp {op = BAdd; left = (EConst (CInt 1)); right = (EConst (CInt 2))};
    right =
    EBinOp {op = BDiv;
      left =
      EBinOp {op = BMul; left = (EConst (CInt 3)); right = (EConst (CInt 4))};
      right = (EConst (CInt 1))}};
  right = (EConst (CInt 2))}|}]
;;

let%expect_test _ =
  test_parse {|let x = 1 in x + 1|};
  [%expect
    {|
(ELetIn
   { name = (MPVar "x"); rec_flag = NoRec; value = (EConst (CInt 1));
     expr = EBinOp {op = BAdd; left = (EVar "x"); right = (EConst (CInt 1))}
     })
     |}]
;;

let%expect_test _ =
  test_parse {|if true then 1 else 2|};
  [%expect
    {|
Eite {cond = (EConst (CBool true)); th = (EConst (CInt 1)); 
  el = (EConst (CInt 2))}
  |}]
;;

let%expect_test _ =
  test_parse {|let rec fib n = if n > 1 then fib (n - 1) + fib (n - 2) else 1 in fib 3|};
  [%expect
    {|
(ELetIn
   { name = (MPVar "fib"); rec_flag = Rec;
     value =
     EFun {args = ((MPVar "n"), []);
       expr =
       Eite {
         cond =
         EBinOp {op = BGT; left = (EVar "n"); right = (EConst (CInt 1))};
         th =
         EBinOp {op = BAdd;
           left =
           (EApply ((EVar "fib"),
              EBinOp {op = BSub; left = (EVar "n"); right = (EConst (CInt 1))}
              ));
           right =
           (EApply ((EVar "fib"),
              EBinOp {op = BSub; left = (EVar "n"); right = (EConst (CInt 2))}
              ))};
         el = (EConst (CInt 1))}};
     expr = (EApply ((EVar "fib"), (EConst (CInt 3)))) })
|}]
;;

let%expect_test _ =
  test_parse
    {|match x with | h1 :: h2 :: tl -> if h1 >= h2 then h1 else h2 | h1 :: [] -> h1 | _ -> 0|};
  [%expect
    {|
EPattern {match_expr = (EVar "x");
  matches =
  ((MPHdTl {head = (MPVar "h1");
      tail = MPHdTl {head = (MPVar "h2"); tail = (MPVar "tl")}},
    Eite {cond = EBinOp {op = BGE; left = (EVar "h1"); right = (EVar "h2")};
      th = (EVar "h1"); el = (EVar "h2")}),
   [(MPHdTl {head = (MPVar "h1"); tail = (MPList [])}, (EVar "h1"));
     (MPWildcard, (EConst (CInt 0)))])}
|}]
;;

(* Interpreter tests *)

open Interpreter

let test_interp s =
  match Parser.parse s with
  | Error err -> print_endline err
  | Ok expr ->
    expr
    |> Inferencer.run_inferencer
    |> (function
     | Error err -> print_endline err
     | Ok _ -> expr |> interpret |> Interpreter.StdFuns.print)
;;

let%expect_test _ =
  test_interp {|1 + 2|};
  [%expect {|3|}]
;;

let%expect_test _ =
  test_interp {|let [a; b] = [1; 2] in a + b|};
  [%expect {|3|}]
;;

let%expect_test _ =
  test_interp
    {|let rec len l =
  match l with
  | [] -> 0
  | _ :: tl -> len tl + 1
in
len [ 0; 1; 2; 3; 4 ]|};
  [%expect {|5|}]
;;

let%expect_test _ =
  test_interp
    {|let sum l =
  let rec helper acc l =
    match l with
    | [] -> acc
    | hd :: tl -> helper (acc + hd) tl
  in
  helper 0 l
in
sum [ 0; 1; 2; 3; 4 ]|};
  [%expect {|10|}]
;;

let%expect_test _ =
  test_interp {|let id a = a in
id (id (id (id 42)))|};
  [%expect {|42|}]
;;

let%expect_test _ =
  test_interp {|let sum (a, (b, c), d) = a + b + c + d in
sum (1, (20, 20), 1)|};
  [%expect {|42|}]
;;

let%expect_test _ =
  test_interp
    {|let fac n = 
  let rec helper n acc = 
    if n <= 1 then
      acc
    else
      helper (n - 1) (n * acc)
  in
  helper n 1
in
fac 5|};
  [%expect {|120|}]
;;

let%expect_test _ =
  test_interp
    {|let fib n = 
  let rec helper a b n = 
    if n > 0 then 
      helper b (a + b) (n - 1)
    else
      a
    in
  helper 0 1 n
in
fib 5|};
  [%expect {|5|}]
;;

(* Inferencer tests *)

open Ast.Constr
open Inferencer

let check e =
  print_string
  @@
  match run_inferencer e with
  | Result.Ok _ -> "Typed"
  | Result.Error err -> err
;;

let%expect_test _ =
  eunop unmin (econst @@ cbool true) |> check;
  [%expect {| Unify fail |}]
;;

let%expect_test _ =
  eunop unmin (econst @@ cint 42) |> check;
  [%expect {| Typed |}]
;;

let%expect_test _ =
  let c = econst @@ cint 42 in
  let f =
    efun (mptuple (mpvar "a") (mpvar "b") [], []) (etuple (evar "a") (evar "b") [])
  in
  eapply f c |> check;
  [%expect {| Unify fail |}]
;;

let%expect_test _ =
  let a = etuple (econst @@ cint 42) (econst cunit) [] in
  let f =
    efun (mptuple (mpvar "a") (mpvar "b") [], []) (etuple (evar "a") (evar "b") [])
  in
  eapply f a |> check;
  [%expect {| Typed |}]
;;

let%expect_test _ =
  let n = mpvar "foo" in
  let v = elist [] in
  eletin n NoRec v (econst cunit) |> check;
  [%expect {| Typed |}]
;;

let%expect_test _ =
  let n = mplist [ mpvar "a"; mpvar "b"; mpvar "c" ] in
  let v = elist [ econst (cint 42) ] in
  eletin n NoRec v (econst cunit) |> check;
  [%expect {| Typed |}]
;;

let%expect_test _ =
  let n = mplist [ mpvar "a"; mpvar "b"; mpvar "c" ] in
  let v = elist [ econst (cint 42); econst (cbool false) ] in
  eletin n NoRec v (econst cunit) |> check;
  [%expect {| Unify fail |}]
;;

let%expect_test _ =
  let n = mplist [ mpvar "a"; mpvar "b"; mpvar "c" ] in
  let v = elist [ econst @@ cint 42 ] in
  eletin n NoRec v (evar "a") |> check;
  [%expect {| Typed |}]
;;

let%expect_test _ =
  let n = mplist [ mpvar "a"; mpvar "b"; mpvar "c" ] in
  let v = elist [ econst @@ cint 42 ] in
  eletin n NoRec v (evar "foo") |> check;
  [%expect {| No variable |}]
;;

let%expect_test _ =
  let n = mphdtl (mpvar "hd") (mpvar "tl") in
  let v = ebinop bcons (evar "hd") (evar "tl") in
  let e = v in
  eletin n NoRec v e |> check;
  [%expect {| No variable |}]
;;

let%expect_test _ =
  let n = mphdtl (mpvar "hd") (mpvar "tl") in
  let v = ebinop bcons (evar "hd") (evar "tl") in
  let e = v in
  eletin n Rec v e |> check;
  [%expect {| Typed |}]
;;

let%expect_test _ =
  let m = econst @@ cint 42 in
  let p = mpconst @@ cint 42, econst @@ cint 42 in
  let ps = [] in
  epattern m (p, ps) |> check;
  [%expect {| Typed |}]
;;

let%expect_test _ =
  let m = econst (cint 42) in
  let p = mpconst cunit, econst (cint 42) in
  let ps = [] in
  epattern m (p, ps) |> check;
  [%expect {| Unify fail |}]
;;

let%expect_test _ =
  let m = econst @@ cint 42 in
  let p = mpvar "a", evar "a" in
  let ps = [] in
  epattern m (p, ps) |> check;
  [%expect {| Typed |}]
;;

let%expect_test _ =
  let m = econst (cint 41) in
  let p = mpconst (cint 42), econst cunit in
  let ps = [ mpconst cunit, econst cunit ] in
  epattern m (p, ps) |> check;
  [%expect {| Unify fail |}]
;;

let%expect_test _ =
  let m = econst (cint 41) in
  let p = mpconst (cint 42), econst (cint 43) in
  let ps = [ mpconst (cint 44), econst cunit ] in
  epattern m (p, ps) |> check;
  [%expect {| Unify fail |}]
;;
