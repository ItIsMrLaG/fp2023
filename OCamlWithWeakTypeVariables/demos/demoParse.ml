(** Copyright 2021-2023, wokuparalyzed *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlWithWeakTypeVariables

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match OCamlWithWeakTypeVariables.Parser.parse s with
  | Result.Ok ast -> Format.printf "%a\n%!" Ast.pp_program ast
  | Error _ -> Format.printf "Some error"
;;
