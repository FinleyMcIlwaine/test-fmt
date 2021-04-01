open Formula

type literal = bool * int (* Literals (bool, var) where false is negated *)

type clause = literal list

type cnf = clause list

let append_uniq a b = List.sort_uniq Stdlib.compare (a @ b)

let filter_negs (c : clause) : clause =
  List.fold_left
    (fun c' l -> if List.mem (not (fst l), snd l) c then c' else l :: c')
    [] c

let clause_combine (a : clause) (b : clause) = append_uniq a b |> filter_negs

let merge (a : cnf) (b : cnf) : cnf =
  List.flatten
    (List.map (fun c -> List.map (clause_combine c) b) a
    |> List.filter (function [ [] ] -> false | _ -> true))

let clausal a : cnf =
  let rec pos = function
    | Var x -> [ [ (true, x) ] ]
    | And (a, b) ->
        let a = pos a in
        let b = pos b in
        append_uniq a b
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
    | And (a, b) ->
        let a = neg a in
        let b = neg b in
        merge a b
    | Or (a, b) ->
        let a = neg a in
        let b = neg b in
        append_uniq a b
    | Imp (a, b) ->
        let a = pos a in
        let b = neg b in
        append_uniq a b
    | Not a -> pos a
    | True -> [ [] ]
    | False -> []
  in
  pos a
