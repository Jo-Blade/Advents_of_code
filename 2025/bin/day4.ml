open Aoc25

(** Type for elements in today's grid *)
type grid_content = Roll | Empty

(** Test if the position pos in the grid contains a roll, which is accessible by
    the forklift *)
let is_accessible_roll grid_content (pos : Vec2.t) =
  if grid_content pos = Roll then
    List.map grid_content @@ Vec2.cross pos @ Vec2.diag pos
    |> List.filter (function Roll -> true | Empty -> false)
    |> List.length
    |> fun x -> x < 4
  else false

(** Compute the list of the positions of all accessible rolls in the given grid
    of dimensions grid_dim *)
let all_accessible_rolls (grid_dim, grid_content) =
  List.filter (is_accessible_roll grid_content) @@ Vec2.square (0, 0) grid_dim

(** Recursively remove all accessible rolls and count the number of removals *)
let day4_part2 (grid_dim, grid_content) =
  let rec aux l n grid =
    let l1, l2 =
      List.partition (is_accessible_roll @@ Grids.naive_of_mapgrid grid) l
    in
    match l1 with
    | [] -> n
    | l ->
        aux l2 (n + List.length l)
        @@ List.fold_left (fun acc a -> Grids.set a Empty acc) grid l
  in
  aux
    (List.filter (fun pos -> grid_content pos = Roll)
    @@ Vec2.square (0, 0) grid_dim)
    0
    (Grids.init_mapgrid grid_content)

(** The parser for today's input *)
let p =
  let open Parsers in
  grid_fun ~oob:Empty @@ function
  | '@' -> Some Roll
  | '.' -> Some Empty
  | _ -> None

let () =
  all_accessible_rolls @@ Parsers.run_on_argv1 p
  |> List.length
  |> Printf.printf "solution part1 = %d\n"

let () =
  day4_part2 @@ Parsers.run_on_argv1 p |> Printf.printf "solution part2 = %d\n"
