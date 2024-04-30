(** Copyright 2021-2023, wokuparalyzed *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module Parsers = struct
  let explode s =
    let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []
  ;;

  let implode x = String.of_seq (List.to_seq x)

  let keywords =
    [ "let"
    ; "rec"
    ; "and"
    ; "match"
    ; "with"
    ; "function"
    ; "fun"
    ; "if"
    ; "then"
    ; "else"
    ; "in"
    ]
  ;;

  type input = char list

  type 'a parse_result =
    | Failed of string
    | Parsed of 'a * input

  type 'a parser = input -> 'a parse_result

  let return x : _ parser = fun s -> Parsed (x, s)
  let fail message _ = Failed message

  let ( >>= ) p f s =
    match p s with
    | Failed msg -> Failed msg
    | Parsed (h, t) -> f h t
  ;;

  let ( let* ) = ( >>= )
  let ( *> ) p1 p2 = p1 >>= fun _ -> p2
  let ( <* ) p1 p2 = p1 >>= fun h -> p2 *> return h

  let ( >> ) p1 p2 s =
    match p1 s with
    | Parsed (_, t) -> p2 t
    | _ -> p2 s
  ;;

  let ( <|> ) p1 p2 s =
    match p1 s with
    | Failed _ -> p2 s
    | res -> res
  ;;

  let ( >>| ) p f = p >>= fun x -> return (f x)

  let rec parse_many parser i =
    match parser i with
    | Parsed (result, rem) -> (parse_many parser >>| List.cons result) rem
    | Failed _ -> Parsed ([], i)
  ;;

  let parse_char recogniser = function
    | h :: t when recogniser h -> return h t
    | _ :: _ -> fail "Char not found" []
    | _ -> fail "EOF" []
  ;;

  let parse_cchar c = parse_char (( = ) c)

  let parser_to_bool p i =
    match p i with
    | Parsed (_, t) -> return true t
    | Failed _ -> return false i
  ;;

  let parse_string str =
    let rec parse_str_inner = function
      | h :: t ->
        let* c = parse_char (( = ) h) in
        let* other = parse_str_inner t in
        return (c :: other)
      | [] -> return []
    in
    parse_str_inner (explode str) >>| implode
  ;;

  let parse_many1 p =
    parse_many p
    >>= function
    | [] -> fail "Needed at least one"
    | e -> return e
  ;;
end

open Parsers
open Constr

let is_digit x = '0' <= x && x <= '9'
let is_char x = ('a' <= x && x <= 'z') || ('A' <= x && x <= 'Z') || x = '_'
let is_op_char x = List.mem x [ '+'; '-'; '<'; '>'; '='; '|'; '&'; '!'; ':'; '*'; '/' ]
let is_whitespace x = List.mem x [ ' '; '\t'; '\n'; '\r' ]
let parse_int = parse_many1 (parse_char is_digit) >>| implode >>| int_of_string

let parse_var =
  let acceptable1 = parse_char is_char <|> parse_char is_digit in
  let* c = parse_char is_char in
  let* rem = parse_many acceptable1 in
  let v = c :: rem |> implode in
  if List.mem v keywords then fail "Variable name cannot be keyword" else return v
;;

let parse_ws = parse_char is_whitespace |> parse_many
let parse_cchar c = parse_ws >> parse_cchar c
let parse_string s = parse_ws >> parse_string s
let parse_var = parse_ws >> parse_var
let parse_int = parse_ws >> parse_int

let parse_op =
  parse_ws
  >> parse_many1 (parse_char is_op_char)
  >>| implode
  >>= function
  | "+" -> return band
  | "-" -> return bsub
  | "*" -> return bmul
  | "/" -> return bdiv
  | ">" -> return bgt
  | ">=" -> return bge
  | "<" -> return blt
  | "<=" -> return ble
  | "=" -> return beq
  | "<>" -> return bne
  | "::" -> return bcons
  | "&&" -> return band
  | "||" -> return bor
  | other -> fail (String.cat other " is an unknown operator")
;;

let parse_uop =
  parse_ws
  >> parse_many1 (parse_char is_op_char)
  >>| implode
  >>= function
  | "-" -> return unmin
  | "!" -> return unnot
  | other -> fail (String.cat other " is an unknown operator")
;;

let parse_bool =
  let true_parser = parse_string "true" *> return (cbool true) in
  let false_parser = parse_string "false" *> return (cbool false) in
  true_parser <|> false_parser
;;

let parse_const =
  let parse_int = parse_int >>| cint in
  parse_bool <|> parse_int >>| econst
;;

let parse_sep_many1 parse_val parse_sep =
  let* elts = parse_many (parse_val <* parse_sep) in
  parse_val >>| fun elt -> elts @ [ elt ]
;;

let parse_sep_many parse_val parse_sep = parse_sep_many1 parse_val parse_sep <|> return []
let parse_pwildcard = parse_string "_" *> return mpwildcard
let parse_pvar = parse_var >>| mpvar

let parse_pconst =
  let parse_int = parse_int >>| cint in
  parse_bool <|> parse_int >>| mpconst
;;

let rec parse_pattern ignore_plist_hdtl =
  let plist_hdtl = if ignore_plist_hdtl then fail "Ignored HDTL" else parse_plist_hdtl in
  plist_hdtl
  <|> parse_pwildcard
  <|> parse_pvar
  <|> parse_ptuple
  <|> parse_plist_constr
  <|> parse_pconst

and parse_ptuple i =
  (parse_cchar '(' *> parse_sep_many1 (parse_pattern false) (parse_cchar ',')
   <* parse_cchar ')'
   >>= function
   | f :: s :: l -> return @@ mptuple f s l
   | f :: [] -> return f
   | _ -> fail "Unit is not implemented")
    i

and parse_plist_constr i =
  (parse_cchar '[' *> parse_sep_many (parse_pattern false) (parse_cchar ';')
   <* parse_cchar ']'
   >>| mplist)
    i

and parse_plist_hdtl i =
  (parse_pattern true
   <* parse_string "::"
   >>= fun head -> parse_pattern false >>| mphdtl head)
    i
;;

let parse_awildcard = parse_string "_" *> return mpwildcard
let parse_avar = parse_var >>| mpvar

let rec parse_apattern i =
  (parse_awildcard <|> parse_avar <|> parse_atuple <|> parse_alist_constr) i

and parse_atuple i =
  (parse_cchar '(' *> parse_sep_many1 parse_apattern (parse_cchar ',')
   <* parse_cchar ')'
   >>= function
   | f :: s :: l -> return @@ mptuple f s l
   | f :: [] -> return f
   | _ -> fail "Unit is not implemented")
    i

and parse_alist_constr i =
  (parse_cchar '[' *> parse_sep_many parse_apattern (parse_cchar ';')
   <* parse_cchar ']'
   >>| mplist)
    i
;;

let precedence = function
  | BAdd -> 50
  | BSub -> 50
  | BMul -> 60
  | BDiv -> 60
  | BGT -> 30
  | BGE -> 30
  | BLT -> 30
  | BLE -> 30
  | BEq -> 20
  | BNE -> 20
  | BCons -> 40
  | BAnd -> 10
  | BOr -> 0
;;

let bool_to_rec_flag b = if b then Rec else NoRec

let rec parse_expr_cmn ignore_binop ignore_app i =
  let p_binop = if ignore_binop then fail "Ignored" else parse_binop <|> parse_unop in
  let p_app = if ignore_app then fail "Ignored" else parse_app in
  (parse_letin
   <|> parse_if_then_else
   <|> parse_match
   <|> p_binop
   <|> p_app
   <|> parse_list_constr
   <|> parse_const
   <|> parse_fun
   <|> parse_evar
   <|> parse_tuple_constr
   <|> parse_par)
    i

and parse_primary_expr i = parse_expr_cmn true false i
and parse_expr_no_app i = parse_expr_cmn true true i
and parse_expr i = parse_expr_cmn false false i
and parse_par i = (parse_cchar '(' *> parse_expr <* parse_cchar ')') i

and parse_tuple_constr i =
  (parse_cchar '(' *> parse_sep_many1 parse_expr (parse_cchar ',')
   <* parse_cchar ')'
   >>= function
   | f :: s :: t -> return @@ etuple f s t
   | _ -> fail "Tuple needs at least 2 arguments")
    i

and parse_list_constr i =
  (parse_cchar '[' *> parse_sep_many parse_expr (parse_cchar ';')
   <* parse_cchar ']'
   >>| elist)
    i

and parse_unop i = (parse_uop >>= fun op -> parse_expr >>| eunop op) i
and parse_evar i = (parse_var >>| evar) i

and parse_fun i =
  (parse_string "fun" *> parse_many parse_apattern
   <* parse_string "->"
   >>= fun args ->
   parse_expr
   >>= fun expr ->
   match args with
   | h :: t -> return @@ efun (h, t) expr
   | _ -> fail "Expected arguments")
    i

and parse_app i =
  (parse_many1 parse_expr_no_app
   >>| fun apps -> List.fold_left eapply (List.hd apps) (List.tl apps))
    i

and parse_binop_rhs prec lhs i =
  match parse_op i with
  | Parsed (fst_op, t) when precedence fst_op >= prec ->
    (parse_primary_expr
     >>= fun r_expr i ->
     let rhs =
       match parse_op i with
       | Parsed (snd_op, _) when precedence fst_op < precedence snd_op ->
         parse_binop_rhs (precedence fst_op + 1) r_expr
       | _ -> return r_expr
     in
     let leftover = rhs >>= fun rhs -> parse_binop_rhs prec (ebinop fst_op lhs rhs) in
     leftover i)
      t
  | _ -> return lhs i

and parse_binop i = (parse_primary_expr >>= parse_binop_rhs 0) i

and parse_letin i =
  (parse_string "let" *> parser_to_bool (parse_string "rec")
   >>= fun is_rec ->
   parse_pattern false
   >>= fun name ->
   parse_many parse_apattern
   <* parse_cchar '='
   >>= fun arguments ->
   parse_expr
   <* parse_string "in"
   >>= fun value ->
   let value =
     match arguments with
     | [] -> value
     | h :: t -> efun (h, t) value
   in
   parse_expr >>| fun expr -> eletin name (bool_to_rec_flag is_rec) value expr)
    i

and parse_match i =
  (let parse_match_case =
     parse_pattern false
     <* parse_string "->"
     >>= fun pattern -> parse_expr >>| fun e -> pattern, e
   in
   parse_string "match" *> parse_expr
   <* parse_string "with"
   >>= fun match_expr ->
   parse_cchar '|'
   >> parse_sep_many1 parse_match_case (parse_cchar '|')
   >>= function
   | h :: t -> return @@ epattern match_expr (h, t)
   | _ -> fail "Match needs at least 1 pattern")
    i

and parse_if_then_else i =
  (parse_string "if" *> parse_expr
   >>= fun cond ->
   parse_string "then" *> parse_expr
   >>= fun th -> parse_string "else" *> parse_expr >>| fun el -> Eite { cond; th; el })
    i
;;

(** main parser *)
let parse s =
  match s |> explode |> parse_expr with
  | Parsed (expr, _) -> Result.Ok expr
  | Failed msg -> Result.Error msg
;;
