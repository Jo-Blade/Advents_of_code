open Aoc25

(** Type for elements in today's grid *)
type grid_content = Start | Empty | Splitter

module Vec2Set = Set.Make (Vec2)
(** Optimisation: Use sets instead of lists to remove duplicates states early in
    execution *)

let rec move_beams_part1 (((height, _), grid_fun) as grid)
    (splits_pos : Vec2Set.t) (beams : Vec2Set.t) =
  (* IMPORTANT FOR PERFORMANCE: use min_elt and not max_elt to advance the most
     delayed beam first.
     It forces duplicated positions which are deleted by the beams Set *)
  match Vec2Set.min_elt_opt beams with
  | None -> splits_pos
  | Some (i, j) -> (
      let t = Vec2Set.remove (i, j) beams in
      if i > height then move_beams_part1 grid splits_pos t
      else
        match grid_fun (i, j) with
        | Start | Empty ->
            move_beams_part1 grid splits_pos (Vec2Set.add (i + 1, j) t)
        | Splitter ->
            (* recursive call on the 2 new beams created by the splitter. The
               "beams" set will remove duplicates, reducing useless
               computations *)
            move_beams_part1 grid
              (Vec2Set.add (i, j) splits_pos)
              (Vec2Set.add (i, j - 1) @@ Vec2Set.add (i, j + 1) t))

let move_beams_part2 ((height, _), grid_fun) pos =
  (* IMPORTANT FOR PERFORMANCE: use memoization to not recompute yet seen paths *)
  let memo = Hashtbl.create 1000 in
  let rec move_beams_part2__ (i, j) =
    if i > height then 1 (* beam is outside the grid, stop *)
    else
      match Hashtbl.find_opt memo (i, j) with
      | Some x -> x
      | None -> (
          match grid_fun (i, j) with
          | Start | Empty -> move_beams_part2__ (i + 1, j)
          | Splitter ->
              let res =
                (* recursive call on the 2 possible timelines *)
                move_beams_part2__ (i, j - 1) + move_beams_part2__ (i, j + 1)
              in
              (* memoize result *)
              Hashtbl.add memo (i, j) res;
              res)
  in
  move_beams_part2__ pos

(** The parser for today's input *)
let p =
  let open Parsers in
  grid_fun ~oob:Empty @@ function
  | 'S' -> Some Start
  | '.' -> Some Empty
  | '^' -> Some Splitter
  | _ -> None

let () =
  let ((size, grid_fun) as grid) = Parsers.run_on_argv1 p in
  let start =
    (* Search Start symbol in the entire grid *)
    List.find (fun p -> match grid_fun p with Start -> true | _ -> false)
    @@ Vec2.square (0, 0) size
  in
  move_beams_part1 grid Vec2Set.empty (Vec2Set.singleton start)
  |> Vec2Set.cardinal
  |> Printf.printf "solution part1 = %d\n";
  move_beams_part2 grid start |> Printf.printf "solution part2 = %d\n"
