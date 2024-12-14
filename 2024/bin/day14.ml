open Angstrom

let test_input =
  {|p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3|}

let is_digit = function '0' .. '9' -> true | _ -> false
let integer = take_while1 is_digit >>| int_of_string

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false

let whitespace = take_while is_whitespace

let signed =
  char '+' *> integer <|> (char '-' *> integer >>| fun x -> -x) <|> integer

let parse_vec2 = both (signed <* char ',') signed
let parse_ligne = both (string "p=" *> parse_vec2) (string " v=" *> parse_vec2)
let parse_full = sep_by whitespace parse_ligne

type vec2 = int * int

let ( +-> ) (a, b) (c, d) = (a + c, b + d)
let ( *> ) (a, b) x = (x * a, x * b)
let ( %-> ) (a, b) (x, y) = (((a mod x) + x) mod x, ((b mod y) + y) mod y)

(** we suppose all points are already in the grid.
    Group points by their quadrant appartenance
    (top-left, top-right, bottom-left, bottom-right) *)
let groupby_quadrants (grid_x, grid_y) (l : vec2 list) : vec2 list list =
  let mid_x, mid_y = (grid_x / 2, grid_y / 2) in
  let in_quadrant compx compy (x, y) = compx x mid_x && compy y mid_y in
  let l_TL = List.filter (in_quadrant ( < ) ( < )) l
  and l_TR = List.filter (in_quadrant ( > ) ( < )) l
  and l_BL = List.filter (in_quadrant ( < ) ( > )) l
  and l_BR = List.filter (in_quadrant ( > ) ( > )) l in
  [ l_TL; l_TR; l_BL; l_BR ]

let list_product = List.fold_left ( * ) 1

let simulate grid_size sim_duration (l : (vec2 * vec2) list) : vec2 list =
  List.map (fun (p, v) -> (p +-> (v *> sim_duration)) %-> grid_size) l

let day14_part1 grid_size sim_duration (l : (vec2 * vec2) list) =
  simulate grid_size sim_duration l
  |> groupby_quadrants grid_size
  |> List.map List.length |> list_product

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

let () = apply (day14_part1 (11, 7) 100) parse_full test_input
let () = apply (day14_part1 (101, 103) 100) parse_full (readfile "input14.txt")

let print_robots (grid_x, grid_y) (l : vec2 list) =
  let m = Array.make_matrix grid_y grid_x '.' in
  List.iter (fun (x, y) -> m.(y).(x) <- 'X') l;
  for i = 0 to grid_y - 1 do
    for j = 0 to grid_x - 1 do
      print_char m.(i).(j)
    done;
    print_newline ()
  done

(** Renvoie true si le point (x,y) est le sommet d'un triangle
    de hauteur 'hauteur' *)
let triangle hauteur l (x, y) =
  List.filter
    (fun (a, b) ->
      let d = b - y in
      0 <= d && d <= hauteur - 1 && x - d <= a && a <= x + d)
    l
  |> List.length
  |> fun c -> c = hauteur * hauteur

(** teste l'existence d'un triangle de hauteur 4
    dans le dessin *)
let exist_triangle (l : vec2 list) : bool = List.exists (triangle 4 l) l

let day14_part2 grid_size max_sim_duration input =
  parse_string ~consume:Prefix parse_full input |> function
  | Ok l ->
      for i = 1 to max_sim_duration do
        let new_l = simulate grid_size i l in
        (* affiche que les grilles qui contiennent au moins un triangle
           de hauteur 4 pour réduire le nombre de cas à vérifier à l'oeil *)
        if exist_triangle new_l then (
          Printf.printf "ITERATION=%d\n" i;
          print_robots grid_size new_l;
          flush stdout)
      done
  | Error x -> failwith ("parse_error=" ^ x)

let () = day14_part2 (101, 103) 8000 (readfile "input14.txt")
