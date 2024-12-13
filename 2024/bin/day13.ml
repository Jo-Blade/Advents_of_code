open Angstrom

let test_input =
  {|Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279|}

let is_digit = function '0' .. '9' -> true | _ -> false
let integer = take_while1 is_digit >>| int_of_string

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false

let whitespace = take_while is_whitespace

let parse_button =
  both
    (string "Button " *> take 1 *> string ": X+" *> integer)
    (string ", Y+" *> integer <* char '\n')

let parse_prix = both (string "Prize: X=" *> integer) (string ", Y=" *> integer)

let parse_one_chall =
  lift3
    (fun (a, c) (b, d) (x, y) -> ((a, b, c, d), (x, y)))
    parse_button parse_button parse_prix

let parse_full = sep_by whitespace parse_one_chall

type matrix2x2 = int * int * int * int
(** type pour une matrice 2x2. Les coefficients sont dans l'ordre
    a,b,c,d tels que M = ((a,b),(c,d)) *)

type vec2 = int * int

let det ((a, b, c, d) : matrix2x2) : int = (a * d) - (b * c)

let ( >. ) ((a, b, c, d) : matrix2x2) ((x, y) : vec2) : vec2 =
  ((a * x) + (b * y), (c * x) + (d * y))

let division_entiere ((x, y) : vec2) (d : int) : vec2 option =
  if x mod d = 0 && y mod d = 0 then Some (x / d, y / d) else None

(** renvoie le cout d'appuyer sur les deux boutons. On peut pas appuyer
    negativement sur un bouton donc si x ou y negatif on renvoie 0 *)
let cost_presses ((x, y) : vec2) : int =
  if x >= 0 && y >= 0 then (3 * x) + y else 0

(** renvoie la matrice ((d,-b),(-c,a)). Il suffit de la diviser par le
    determinant pour obtenir l'inverse de la matrice
    (à condition que le determinant soit non nul) *)
let partial_inv_mat ((a, b, c, d) : matrix2x2) : matrix2x2 = (d, -b, -c, a)

(** renvoie le nombre d'appuis à faire pour gagner le prix dans
    l'ordre bouton_a, bouton_b
    ou None s'il n'y a pas de solution *)
let find_presses (m : matrix2x2) (expected_result : vec2) : vec2 option =
  let det_m = det m in
  if det_m = 0 then
    (* matrice liée, donc les vecteurs sont colinéaires et
       on doit trouver le minimum des solutions possibles *)
    failwith "not implemented"
  else
    let presses = partial_inv_mat m >. expected_result in
    division_entiere presses det_m

let list_sum = List.fold_left ( + ) 0

let day13_part1 (l : (matrix2x2 * vec2) list) =
  List.filter_map (fun (m, res) -> find_presses m res) l
  |> List.map cost_presses |> list_sum

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

let () = apply day13_part1 parse_full test_input
let () = apply day13_part1 parse_full (readfile "input13.txt")

let day13_part2 (l : (matrix2x2 * vec2) list) =
  List.filter_map
    (fun (m, (x, y)) -> find_presses m (10000000000000 + x, 10000000000000 + y))
    l
  |> List.map cost_presses |> list_sum

let () = apply day13_part2 parse_full test_input
let () = apply day13_part2 parse_full (readfile "input13.txt")
