open Angstrom

let test_input = {|3   4
4   3
2   5
1   3
3   9
3   3|}

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false

let whitespace = take_while is_whitespace
let is_digit = function '0' .. '9' -> true | _ -> false
let integer = take_while1 is_digit >>| int_of_string
let int_ws = integer <* whitespace

let test_parser parser str =
  parse_string ~consume:Prefix parser str |> function
  | Ok s -> print_endline ("result=" ^ s)
  | Error s -> failwith ("error=" ^ s)

let parse_ligne = lift2 (fun a b -> (a, b)) int_ws int_ws

let separe l =
  List.fold_left (fun (acc1, acc2) (x, y) -> (x :: acc1, y :: acc2)) ([], []) l
  |> fun (x, y) -> (List.rev x, List.rev y)

let parse_full = many parse_ligne >>| separe

let () =
  test_parser
    ( parse_full >>| fun (l1, l2) ->
      String.concat "," (List.map string_of_int l1)
      ^ "|"
      ^ String.concat "," (List.map string_of_int l2) )
    test_input

let distance n1 n2 = abs (n1 - n2)
let list_sort = List.sort (fun x y -> y - x)

let day1 (l1 : int list) (l2 : int list) =
  let rec aux l1 l2 =
    match (l1, l2) with
    | x1 :: t1, x2 :: t2 -> distance x1 x2 :: aux t1 t2
    | [], [] -> []
    | _ -> failwith "impossible"
  in
  aux (list_sort l1) (list_sort l2)

let list_sum = List.fold_left ( + ) 0

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
  parse_string ~consume:Prefix parse_full test_input |> function
  | Ok (l1, l2) -> day1 l1 l2 |> list_sum |> Printf.printf "result=%d\n"
  | Error x -> failwith ("parse_error=" ^ x)

let () =
  parse_string ~consume:Prefix parse_full (readfile "input.txt") |> function
  | Ok (l1, l2) -> day1 l1 l2 |> list_sum |> Printf.printf "result=%d"
  | Error x -> failwith ("parse_error=" ^ x)

let nb_occurences (l : int list) n =
  List.filter (fun x -> x = n) l |> List.length

let day1_part2 (l1 : int list) (l2 : int list) =
  List.map (fun x -> x * nb_occurences l2 x) l1

let () =
  parse_string ~consume:Prefix parse_full test_input |> function
  | Ok (l1, l2) -> day1_part2 l1 l2 |> list_sum |> Printf.printf "result=%d\n"
  | Error x -> failwith ("parse_error=" ^ x)

let () =
  parse_string ~consume:Prefix parse_full (readfile "input.txt") |> function
  | Ok (l1, l2) -> day1_part2 l1 l2 |> list_sum |> Printf.printf "result=%d"
  | Error x -> failwith ("parse_error=" ^ x)
