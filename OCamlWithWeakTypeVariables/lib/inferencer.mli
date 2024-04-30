(** Copyright 2021-2023, wokuparalyzed *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** main infer with baselib function types *)
val run_inferencer : Ast.expr -> (unit, string) Result.t
