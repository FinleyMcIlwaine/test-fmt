open Formula

(** Substitute a variable in a formula by a formula *)
let rec subst x c = function
  | Var y -> if x = y then c else Var y
  | And (a, b) -> And (subst x c a, subst x c b)
  | Or (a, b) -> Or (subst x c a, subst x c b)
  | Imp (a, b) -> Imp (subst x c a, subst x c b)
  | Not a -> Not (subst x c a)
  | True -> True
  | False -> False

(** Find a free variable in a formula *)
let free_var a =
  let exception Found of int in
  let rec aux = function
    | Var x -> raise (Found x)
    | And (a, b) | Or (a, b) | Imp (a, b) ->
        aux a;
        aux b
    | Not a -> aux a
    | True | False -> ()
  in
  try
    aux a;
    raise Not_found
  with Found x -> x

(** Evaluate a closed formula *)
let rec eval = function
  | Var _ -> assert false
  | And (a, b) -> eval a && eval b
  | Or (a, b) -> eval a || eval b
  | Imp (a, b) -> (not (eval a)) || eval b
  | Not a -> not (eval a)
  | True -> true
  | False -> false

(** Simple-minded satisfiability *)
let rec sat a =
  try
    let x = free_var a in
    sat (subst x True a) || sat (subst x False a)
  with Not_found -> eval a
