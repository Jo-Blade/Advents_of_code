open Angstrom

let test_input = {|125 17|}
let is_digit = function '0' .. '9' -> true | _ -> false
let integer = take_while1 is_digit >>| int_of_string
let parse_list = sep_by (char ' ') integer
let rec get_num_digits n = if n >= 10 then 1 + get_num_digits (n / 10) else 1
let rec puiss_10 = function 0 -> 1 | n -> 10 * puiss_10 (n - 1)

let evolve n : int list =
  if n = 0 then [ 1 ]
  else
    let num_digits = get_num_digits n in
    if num_digits mod 2 = 0 then
      let d = puiss_10 (num_digits / 2) in
      [ n / d; n mod d ]
    else [ n * 2024 ]

let rec evolve_list_x_times x l =
  match x with
  | 0 -> l
  | x -> List.map evolve l |> List.flatten |> evolve_list_x_times (x - 1)

let day7_part1 l = evolve_list_x_times 25 l |> List.length

let apply f p input =
  parse_string ~consume:Prefix p input |> function
  | Ok l -> f l |> Printf.printf "result=%d\n"
  | Error x -> failwith ("parse_error=" ^ x)

let () = apply day7_part1 parse_list test_input

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
  apply day7_part1 parse_list (readfile "input11.txt");
  flush stdout

let evolve_x_times x n : int =
  let htbl = Hashtbl.create 10000 in
  let rec evolve_x_times_aux x n : int =
    match Hashtbl.find_opt htbl (x, n) with
    | Some res -> res
    | None ->
        let res =
          match x with
          | 0 -> 1
          | _ ->
              if n = 0 then evolve_x_times_aux (x - 1) 1
              else
                let num_digits = get_num_digits n in
                if num_digits mod 2 = 0 then
                  let d = puiss_10 (num_digits / 2) in
                  evolve_x_times_aux (x - 1) (n / d)
                  + evolve_x_times_aux (x - 1) (n mod d)
                else evolve_x_times_aux (x - 1) (n * 2024)
        in
        Hashtbl.add htbl (x, n) res;
        res
  in
  evolve_x_times_aux x n

let list_sum = List.fold_left ( + ) 0
let day7_part2 l = List.map (evolve_x_times 75) l |> list_sum
let () = apply day7_part2 parse_list test_input
let () = apply day7_part2 parse_list (readfile "input11.txt")
