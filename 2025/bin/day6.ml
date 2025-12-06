open Aoc25

type operations = Sum | Product

(** Transposition of a matrix represented with 'a list list source:
    https://rosettacode.org/wiki/Matrix_transposition *)
let rec transpose m =
  assert (m <> []);
  if List.mem [] m then []
  else List.map List.hd m :: transpose (List.map List.tl m)

let execute_op (op, vals) =
  List.fold_left
    (match op with Sum -> ( + ) | Product -> ( * ))
    (match op with Sum -> 0 | Product -> 1)
    vals

(** The parser for today's input
    @param parse_numbers the parser to read numbers in the input correctly *)
let p parse_numbers =
  let open Parsers in
  combine2
    (fun vals operators -> List.combine (List.of_seq operators) vals)
    parse_numbers
    (char '\n'
    +-> repeat
          (spaces
          +-> map_next (function
                | '+' -> Some Sum
                | '*' -> Some Product
                | _ -> None)))

(** Read numbers normally and order them by columns *)
let parse_numbers_part1 =
  let open Parsers in
  map (fun xss -> transpose @@ List.of_seq @@ (Seq.map List.of_seq) xss)
  @@ grid (spaces +-> uint +<- spaces)

(** Convert a list of chars to a string *)
let string_of_char_list l = String.of_seq (List.to_seq l)

(** Read numbers by reading all digits aligned by columns *)
let parse_numbers_part2 =
  let open Parsers in
  map (fun xss ->
      transpose @@ List.of_seq @@ (Seq.map List.of_seq) xss
      |>
      (* remove spaces *)
      List.map (List.filter (function ' ' -> false | _ -> true))
      |>
      (* concat chars to strings *)
      List.map string_of_char_list
      |> fun l ->
      let curr, all =
        List.fold_right
          (fun s (curr, all) ->
            if s = "" then ([], curr :: all) else (s :: curr, all))
          l ([], [])
      in
      curr :: all
      |>
      (* convert strings to numbers *)
      List.map (List.map int_of_string))
  @@ grid (map_next (fun c -> if c = ' ' || is_digit c then Some c else None))

let () =
  List.map execute_op @@ Parsers.run_on_argv1 (p parse_numbers_part1)
  |> List.fold_left ( + ) 0
  |> Printf.printf "solution part1 = %d\n"

let () =
  List.map execute_op @@ Parsers.run_on_argv1 (p parse_numbers_part2)
  |> List.fold_left ( + ) 0
  |> Printf.printf "solution part2 = %d\n"
