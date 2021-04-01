type var = int

(** Formulas *)
type t =
  | Var of var
  | And of t * t
  | Or of t * t
  | Imp of t * t
  | Not of t
  | True
  | False
