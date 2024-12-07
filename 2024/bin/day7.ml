open Angstrom

let test_input =
  {|190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20|}

let is_digit = function '0' .. '9' -> true | _ -> false
let integer = take_while1 is_digit >>| int_of_string
let parse_ligne = both (integer <* string ": ") (sep_by (char ' ') integer)
let parse_full = sep_by (char '\n') parse_ligne

let is_valid list_op expected_result l =
  let rec is_valid_aux curr_result l =
    match l with
    | [] -> curr_result = expected_result
    | h :: t ->
        List.map (fun op -> is_valid_aux (op curr_result h) t) list_op
        |> List.fold_left ( || ) false
  in
  match l with [] -> failwith "liste vide" | h :: t -> is_valid_aux h t

let list_sum = List.fold_left ( + ) 0

let day7_part1 (l : (int * int list) list) : int =
  List.filter (fun (n, l) -> is_valid [ ( + ); ( * ) ] n l) l
  |> List.map fst |> list_sum

let apply f p input =
  parse_string ~consume:Prefix p input |> function
  | Ok l -> f l |> Printf.printf "result=%d\n"
  | Error x -> failwith ("parse_error=" ^ x)

let () = apply day7_part1 parse_full test_input

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

let () = apply day7_part1 parse_full (readfile "input7.txt")

(** concatenation des chiffres en base 10 de n1 suivi de n2 *)
let concat_op n1 n2 =
  (* trouver la puissance de 10 par laquelle
     multiplier n1 pour la concatenation *)
  let rec puiss_10 n = if n >= 10 then 10 * puiss_10 (n / 10) else 10 in
  (n1 * puiss_10 n2) + n2

let day7_part2 (l : (int * int list) list) : int =
  List.filter (fun (n, l) -> is_valid [ ( + ); ( * ); concat_op ] n l) l
  |> List.map fst |> list_sum

let () = apply day7_part2 parse_full test_input
let () = apply day7_part2 parse_full (readfile "input7.txt")
