(** Copyright 2021-2023, wokuparalyzed *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** id of type variable *)
type id = int

(** common types *)
type gr_ty =
  | GrInt (** int type *)
  | GrBool (** bool type *)
  | GrUnit (** unit type *)

(** composite types *)
type ty =
  | TyGround of gr_ty (** common types *)
  | TyArrow of ty * ty (** function types *)
  | TyTuple of ty * ty * ty list (** tuple types *)
  | TyList of ty (** 'ty list *)
  | TyVar of id (** type variable *)
