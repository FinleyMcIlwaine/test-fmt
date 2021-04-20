open Formula

(* Implements thm 2.5.7.2 in Program=Proof by Mimram, where a[T/x] (resp.
a[F/x]) is computed by removing all clauses containing `x` (resp. `not x`) and
then removing any literal `not x` (resp. `x`) from the remaining clauses *)

(** Returns a canonical clausal formula for a[v/x] *)
let subst (a : cnf) (v : bool) (x : var) : cnf =
  let a = List.filter (fun c -> not (List.mem (v, x) c)) a in
  List.map (fun c -> List.filter (fun l -> l <> (not v, x)) c) a

(** Find a unitary clause *)
let rec unit : cnf -> literal = function
  | [ (n, x) ] :: _ -> (n, x)
  | _ :: a -> unit a
  | [] -> raise Not_found

(** Find a pure literal in a clausal formula *)
let pure (a : cnf) : literal =
  let rec clause vars = function
    | [] -> vars
    | (n, x) :: c -> (
        try
          match List.assoc x vars with
          | Some n' ->
              if n' = n then clause vars c
              else
                let vars = List.filter (fun (y, _) -> y <> x) vars in
                clause ((x, None) :: vars) c
          | None -> clause vars c
        with Not_found -> clause ((x, Some n) :: vars) c)
  in
  let vars = List.fold_left clause [] a in
  let x, n = List.find (function _, Some _ -> true | _ -> false) vars in
  (Option.get n, x)

(** DPLL satisfiability algorithm *)
let rec dpll a =
  if a = [] then true
  else if List.mem [] a then false
  else
    try
      let n, x = unit a in
      dpll (subst a n x)
    with Not_found -> (
      try
        let n, x = pure a in
        dpll (subst a n x)
      with Not_found ->
        let x = snd (List.hd (List.hd a)) in
        dpll (subst a false x) || dpll (subst a true x))
