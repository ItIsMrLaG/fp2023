(** Copyright 2021-2023, wokuparalyzed *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** main parser *)
val parse : Ast.name -> (Ast.expr, string) Result.t
