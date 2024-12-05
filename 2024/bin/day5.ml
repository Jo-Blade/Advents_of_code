open Angstrom

let test_input =
  {|47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47|}

let is_digit = function '0' .. '9' -> true | _ -> false
let integer = take_while1 is_digit >>| int_of_string

let couple_parser =
  both (integer <* char '|') (integer <* char '\n') >>| fun (x, y) ->
  ( (fun z -> if z = y then Some x else None) (* fonction precedent *),
    fun z -> if z = x then Some y else None )
(* fonction suivant *)

let int_list_parser = sep_by (char ',') integer

(** Parse tous les couples et renvoie 2 fonctions prec, succ:
  - prec n : fonction qui renvoie la liste des nombres qui doivent apparaitre avant n
  - succ n : fonction qui renvoie la liste des nombres qui doivent apparaitre après n
  *)
let parse_all_couples =
  let reduce_functions l n =
    List.map (fun f -> f n) l |> List.filter_map (fun x -> x)
  in
  many couple_parser >>| fun l ->
  List.fold_left (fun (acc1, acc2) (x, y) -> (x :: acc1, y :: acc2)) ([], []) l
  |> fun (l1, l2) -> (reduce_functions l1, reduce_functions l2)

(** renvoie les fonctions (prec, succ), et les listes à tester *)
let parse_full =
  both parse_all_couples (char '\n' *> sep_by (char '\n') int_list_parser)

let in_list x = List.exists (fun y -> x = y)

let is_list_valid prec succ l =
  (* cette fonction auxilière vérifie que aucun élément banni est dans la liste
     et ajoute à la liste des éléments bannis f(premier element) à chaque appel récursif *)
  let rec partial_is_list_valid f l banned_els =
    match l with
    | [] -> true
    | h :: t ->
        if in_list h banned_els then false
        else partial_is_list_valid f t (f h @ banned_els)
  in
  (* vérifier qu'aucun prédécesseur de chaque élément de la liste se trouve après lui dans la liste *)
  partial_is_list_valid prec l []
  && (* vérifier qu'aucun successeur de chaque élément de la liste se trouve après lui dans la liste *)
  partial_is_list_valid succ (List.rev l) []

(** renvoie l'élément au milieu de la liste *)
let find_middle l =
  let n = List.length l in
  List.filteri (fun i _ -> n / 2 = i) l |> function
  | [] -> failwith "liste vide"
  | [ x ] -> x
  | _ -> failwith "error in find_middle, devrait être impossible"

let list_sum = List.fold_left ( + ) 0

let day5_part1 prec succ l =
  List.filter (is_list_valid prec succ) l |> List.map find_middle |> list_sum

let apply f p input =
  parse_string ~consume:Prefix p input |> function
  | Ok l -> f l |> Printf.printf "result=%d\n"
  | Error x -> failwith ("parse_error=" ^ x)

let readfile file =
  (* Read file and display the first line *)
  let ic = open_in file in
  let rec aux () =
    try
      let line = input_line ic in
      (* read line, discard \n *)
      line :: aux ()
    with End_of_file ->
      close_in ic;
      []
  in
  String.concat "\n" (aux ())

let () =
  apply (fun ((prec, succ), l) -> day5_part1 prec succ l) parse_full test_input

let () =
  apply
    (fun ((prec, succ), l) -> day5_part1 prec succ l)
    parse_full (readfile "input5.txt")

let day5_part2 prec succ l =
  let order a b =
    if a = b then 0 (* a = b *)
    else if in_list a (prec b) || in_list b (succ a) then -1 (* a < b *)
    else if in_list b (prec a) || in_list a (succ b) then 1 (* b > a *)
    else failwith "unknown order"
  in
  List.filter (fun x -> not (is_list_valid prec succ x)) l
  |> List.map (List.sort order)
  |> List.map find_middle |> list_sum

let () =
  apply (fun ((prec, succ), l) -> day5_part2 prec succ l) parse_full test_input

let () =
  apply
    (fun ((prec, succ), l) -> day5_part2 prec succ l)
    parse_full (readfile "input5.txt")
