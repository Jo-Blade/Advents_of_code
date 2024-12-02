open Angstrom

let test_input = {|7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9|}

let is_digit = function '0' .. '9' -> true | _ -> false
let integer = take_while1 is_digit >>| int_of_string
let parse_ligne = sep_by (char ' ') integer
let parse_full = sep_by (char '\n') parse_ligne

let list_increments l =
  List.fold_left
    (fun (prev, l) x ->
      match prev with None -> (Some x, l) | Some y -> (Some x, (y - x) :: l))
    (None, []) (List.rev l)
  |> snd

let list_in_inter l min max =
  List.map (fun x -> x >= min && x <= max) l |> List.fold_left ( && ) true

let is_valid l =
  let inc_l = list_increments l in
  list_in_inter inc_l (-3) (-1) || list_in_inter inc_l 1 3

let day2_part1 l =
  List.map is_valid l |> List.filter (fun x -> x) |> List.length

let apply f input =
  parse_string ~consume:Prefix parse_full input |> function
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

let () = apply day2_part1 test_input
let () = apply day2_part1 (readfile "input2.txt")

(** toutes les listes construites a partir de l a laquelle on retire un element *)
let all_sub_lists l =
  let rec f l1 l2 acc =
    match l1 with
    | [] -> acc
    | h :: t ->
        let new_acc = l2 :: List.map (fun l -> h :: l) acc in
        f t (h :: l2) new_acc
  in
  f (List.rev l) [] []

let day2_part2 l =
  List.map (fun l -> day2_part1 (all_sub_lists l)) l
  |> List.filter (fun x -> x > 0)
     (* on garde que les cas ou
        ca a reussi au moins une fois *)
  |> List.length

let () = apply day2_part2 test_input
let () = apply day2_part2 (readfile "input2.txt")
