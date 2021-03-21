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

type literal = bool * var (* Literals (bool, var) where false is negated *)

type clause = literal list

type cnf = clause list

let clausal a : cnf =
  let merge a b =
    List.flatten (List.map (fun c -> List.map (fun d -> c @ d) b) a)
  in
  let rec pos = function
    | Var x -> [ [ (true, x) ] ]
    | And (a, b) ->
        let a = pos a in
        let b = pos b in
        a @ b
    | Or (a, b) ->
        let a = pos a in
        let b = pos b in
        merge a b
    | Imp (a, b) ->
        let a = neg a in
        let b = pos b in
        merge a b
    | Not a -> neg a
    | True -> []
    | False -> [ [] ]
  and neg = function
    | Var x -> [ [ (false, x) ] ]
    | And (a, b) -> let a = neg a in let b = neg b in
        merge a b
    | Or (a, b) ->
        let a = neg a in
        let b = neg b in
        a @ b
    | Imp (a, b) ->
        let a = pos a in
        let b = neg b in
        a @ b
    | Not a -> pos a
    | True -> [ [] ]
    | False -> []
  in
  pos a
