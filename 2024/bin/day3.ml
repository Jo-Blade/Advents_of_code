open Angstrom

let test_input =
  {|xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))|}

let is_digit = function '0' .. '9' -> true | _ -> false
let integer = take_while1 is_digit >>| int_of_string

let parse_mul =
  lift2
    (fun x y -> (x, y))
    (string "mul(" *> integer)
    (char ',' *> integer <* char ')')

let parser_part1 =
  many (parse_mul >>= (fun x -> return (Some x)) <|> advance 1 *> return None)

let parser_filter p = p >>| List.filter_map (fun x -> x)
let list_sum = List.fold_left ( + ) 0

let day3_part1 (l : (int * int) list) =
  List.map (fun (x, y) -> x * y) l |> list_sum

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

let () = apply day3_part1 (parser_filter parser_part1) test_input
let () = apply day3_part1 (parser_filter parser_part1) (readfile "input3.txt")

(* skip tout ce qui suit un "don't" jusq'au prochain "do" ou fin de l'input *)
let parser_dont =
  string "don't()"
  *> many_till (advance 1) (advance 0 <* string "do()" <|> end_of_input)

let parser_part2 =
  many
    (parse_mul
    >>= (fun x -> return (Some x))
    <|> parser_dont *> return None
    <|> advance 1 *> return None)

let () = apply day3_part1 (parser_filter parser_part2) test_input
let () = apply day3_part1 (parser_filter parser_part2) (readfile "input3.txt")
