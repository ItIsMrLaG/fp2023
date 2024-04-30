(** Copyright 2021-2023, wokuparalyzed *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlWithWeakTypeVariables

let () =
  let code = Stdio.In_channel.input_all stdin in
  match Parser.parse code with
  | Error err -> print_endline err
  | Ok expr ->
    expr
    |> Inferencer.run_inferencer
    |> (function
     | Error err -> print_endline err
     | Ok _ -> expr |> Interpreter.interpret |> Interpreter.Interpreter.StdFuns.print)
;;
