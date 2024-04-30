(** Copyright 2021-2023, wokuparalyzed *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Interpreter : sig
  (** main reterning value *)
  type value

  module StdFuns : sig
    (** print value *)
    val print : value -> unit
  end
end

(** main interpreter *)
val interpret : Ast.expr -> Interpreter.value
