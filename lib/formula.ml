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

(* Literals (bool, var) where false is negated *)
type literal = bool * var

(* Clauses and formulas *)
type clause = literal list

type cnf = clause list
