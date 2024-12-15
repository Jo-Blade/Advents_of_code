open Angstrom

let test_input =
  {|##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^|}

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false

let whitespace = take_while is_whitespace

module Vec2 = struct
  type t = int * int

  let compare (i1, j1) (i2, j2) = if i1 - i2 = 0 then j1 - j2 else i1 - i2
end

module Vec2_Set = Set.Make (Vec2)

let direction_of_char : Vec2.t t =
  any_char >>= function
  | '^' -> return (-1, 0)
  | 'v' -> return (1, 0)
  | '>' -> return (0, 1)
  | '<' -> return (0, -1)
  | _ -> fail "not a direction"

let parse_moves = many (whitespace *> direction_of_char)

let grid_line =
  take_while1 (function '#' | 'O' | '.' | '@' -> true | _ -> false)

let grid = sep_by (char '\n') grid_line

(** teste si le point donné de ligne d'indice i et de colonne d'indice j
    appartient à la matrice de taille n, m *)
let in_matrix n m (i, j) =
  if i < n && i >= 0 && j < m && j >= 0 then true else false

(** renvoie: wall_reader, n, m, boxes_list, init_pos *)
let analyse_grid (l : string list) =
  match l with
  | [] -> failwith "empty grid"
  | h :: _ ->
      let n, m = (List.length l, String.length h) in
      let mat = Array.make_matrix n m '.' in
      let init_pos = ref (0, 0) in
      let boxes_list =
        List.mapi
          (fun i li ->
            String.to_seqi li
            |> Seq.filter_map (fun (j, c) ->
                   match c with
                   | '@' ->
                       init_pos := (i, j);
                       None
                   | 'O' -> Some (i, j)
                   | '#' ->
                       mat.(i).(j) <- c;
                       None
                   | _ -> None)
            |> List.of_seq)
          l
        |> List.flatten
      in
      ( (fun ((i, j) as case) ->
          if in_matrix n m case then mat.(i).(j) = '#' else true),
        n,
        m,
        boxes_list,
        !init_pos )

let parse_full = both grid parse_moves
let ( +-> ) (a, b) (c, d) = (a + c, b + d)

(** Essaie de faire un mouvement. Si le mouvement est possible,
    renvoie le set updated, sinon renvoie None *)
let rec maybe_make_mouvement wall_reader (boxes_set : Vec2_Set.t)
    ((di, dj) as move_direction : Vec2.t) ((i, j) as to_case : Vec2.t) :
    Vec2_Set.t option =
  if not (wall_reader to_case) then
    (* on regarde si la case est vide *)
    match Vec2_Set.find_opt to_case boxes_set with
    | None -> Some boxes_set
    | Some _ -> (
        match
          maybe_make_mouvement wall_reader boxes_set move_direction
            (i + di, j + dj)
        with
        | None -> None (* impossible de déplacer la box *)
        | Some new_boxes_set ->
            Some
              (Vec2_Set.remove to_case new_boxes_set
              |> Vec2_Set.add (i + di, j + dj))
            (* la case suivante est libre, donc on déplace la box dessus *))
  else None

let execute_all_moves wall_reader boxes_list init_pos moves_list =
  List.fold_left
    (fun (pos, boxes_set) dir ->
      match maybe_make_mouvement wall_reader boxes_set dir (pos +-> dir) with
      | None -> (pos, boxes_set)
      | Some new_boxes_set -> (pos +-> dir, new_boxes_set))
    (init_pos, Vec2_Set.of_list boxes_list)
    moves_list

let gps_score (i, j) = (100 * i) + j
let list_sum = List.fold_left ( + ) 0

let day15_part1 (grid, moves_list) =
  let wall_reader, _, _, boxes_list, init_pos = analyse_grid grid in
  let _, final_boxes_set =
    execute_all_moves wall_reader boxes_list init_pos moves_list
  in
  Vec2_Set.to_list final_boxes_set |> List.map gps_score |> list_sum

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

let () = apply day15_part1 parse_full test_input
let () = apply day15_part1 parse_full (readfile "input15.txt")

let find_opt_box (boxes_set : Vec2_Set.t) (i, j) =
  Vec2_Set.find_opt (i, j - 1) boxes_set |> function
  | Some x -> Some x
  | None -> Vec2_Set.find_opt (i, j) boxes_set

let rec try_push_box wall_reader boxes_set ((di, dj) as dir) (a, b) :
    Vec2_Set.t option =
  if wall_reader (a + di, b + dj) || wall_reader (a + di, b + dj + 1) then None
  else
    let boxes : Vec2.t list =
      if di = 0 then
        Vec2_Set.find_opt (a, b + (2 * dj)) boxes_set |> function
        | None -> []
        | Some x -> [ x ]
      else
        (find_opt_box boxes_set (a + di, b) |> function
         | None -> []
         | Some x -> [ x ])
        @ (Vec2_Set.find_opt (a + di, b + 1) boxes_set |> function
           | None -> []
           | Some x -> [ x ])
    in
    List.fold_left
      (fun acc b ->
        match acc with
        | None -> None
        | Some set -> try_push_box wall_reader set dir b)
      (Some boxes_set) boxes
    |> function
    | None -> None
    | Some set ->
        Some (Vec2_Set.remove (a, b) set |> Vec2_Set.add (a + di, b + dj))

let maybe_make_mouvement_part2_robot wall_reader boxes_set dir to_pos =
  if wall_reader to_pos then None
  else
    match find_opt_box boxes_set to_pos with
    | None -> Some boxes_set
    | Some box -> try_push_box wall_reader boxes_set dir box

let execute_all_moves_part2 wall_reader boxes_list init_pos moves_list =
  let get_wall_reader_part2 wall_reader (i, j) = wall_reader (i, j / 2)
  and init_pos_part2 (i, j) = (i, 2 * j) in
  List.fold_left
    (fun (pos, boxes_set) dir ->
      match
        maybe_make_mouvement_part2_robot
          (get_wall_reader_part2 wall_reader)
          boxes_set dir (pos +-> dir)
      with
      | None -> (pos, boxes_set)
      | Some new_boxes_set -> (pos +-> dir, new_boxes_set))
    ( init_pos_part2 init_pos,
      Vec2_Set.of_list (List.map init_pos_part2 boxes_list) )
    moves_list

let day15_part2 (grid, moves_list) =
  let wall_reader, _, _, boxes_list, init_pos = analyse_grid grid in
  let _, final_boxes_set =
    execute_all_moves_part2 wall_reader boxes_list init_pos moves_list
  in
  Vec2_Set.to_list final_boxes_set |> List.map gps_score |> list_sum

let () = apply day15_part2 parse_full test_input
let () = apply day15_part2 parse_full (readfile "input15.txt")
