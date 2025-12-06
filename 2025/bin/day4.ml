open Aoc25

(** Type for elements in today's grid *)
type grid_content = Roll | Empty

(** Test if the position pos in the grid contains a roll, which is accessible by
    the forklift *)
let is_accessible_roll grid_content (pos : Vec.vec2) =
  if grid_content pos = Roll then
    List.map grid_content @@ Vec.cross pos @ Vec.diag pos
    |> List.filter (function Roll -> true | Empty -> false)
    |> List.length
    |> fun x -> x < 4
  else false

let day4_part1 ((lines, columns), grid_content) =
  List.init lines (fun i ->
      List.init columns (fun j ->
          Bool.to_int @@ is_accessible_roll grid_content (i, j)))
  |> List.map @@ List.fold_left ( + ) 0
  |> List.fold_left ( + ) 0

(** The parser for today's input *)
let p =
  let open Parsers in
  grid_fun ~oob:Empty @@ function
  | '@' -> Some Roll
  | '.' -> Some Empty
  | _ -> None

let () =
  day4_part1 @@ Parsers.run_on_argv1 p |> Printf.printf "solution part1 = %d\n"
