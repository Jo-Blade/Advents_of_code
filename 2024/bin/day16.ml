let test_input =
  {|###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############|}

module Vec2 = struct
  type t = int * int

  let compare (i1, j1) (i2, j2) = if i1 - i2 = 0 then j1 - j2 else i1 - i2
end

module Vec2_Set = Set.Make (Vec2)

(** Obtenir une fonction qui permet de lire un caractère du tableau
    à partir de ses indices de ligne et colonne.
    Si on déborde, on renvoie '.'.
    on renvoie aussi le nombre de lignes et de colonnes du tableau *)
let get_reader str outside_char =
  (* on suppose que toutes les lignes sont de la même longueur *)
  let n, m =
    String.split_on_char '\n' str |> function
    | [] -> failwith "liste vide"
    | h :: t -> (List.length (h :: t), String.length h + 1)
  in
  let mat = Array.make_matrix n m ' ' in
  String.iteri (fun k c -> mat.(k / m).(k mod m) <- c) str;
  (* on renvoie une fonction qui permet de lire le tableau
     et renvoie '.' si on déborde.
     On utilise m-1 pour empêcher de pouvoir lire les '\n' *)
  ( (fun i j ->
      if i < n && i >= 0 && j < m - 1 && j >= 0 then mat.(i).(j)
      else outside_char),
    n,
    m - 1 )

type direction = Nord | Sud | Est | Ouest

let vec_of_direction = function
  | Nord -> (-1, 0)
  | Sud -> (1, 0)
  | Est -> (0, 1)
  | Ouest -> (0, -1)

let rotate_90 = function
  | Nord -> Est
  | Est -> Sud
  | Sud -> Ouest
  | Ouest -> Nord

let rotate_minus90 = function
  | Nord -> Ouest
  | Ouest -> Sud
  | Sud -> Est
  | Est -> Nord

let find_pos_letter reader letter n m =
  List.init (n * m) (fun k ->
      if reader (k / m) (k mod m) = letter then [ (k / m, k mod m) ] else [])
  |> List.flatten
  |> function
  | [] -> failwith "no init pos"
  | h :: _ -> h

let is_wall reader (i, j) = reader i j = '#'

type state = direction * (int * int)
(** un état est une orientation + une position *)

let ( +-> ) (a, b) (c, d) = (a + c, b + d)

(*
let print_vec (a,b) = Printf.printf "debug:(%d,%d)\n" a b
*)

(* renvoie le meilleur score et le chemin trouvé *)
let pathfinding_score n m (is_wall : int * int -> bool) score
    (init_state : state) (final_pos : int * int) : int * state list =
  let etats_visites = Hashtbl.create (n * m) in
  let rec pathfinding_aux ((dir, pos) as curr_state : state)
      (curr_score : int * int) (prev_state : state) =
    if is_wall pos then ()
    else
      (match Hashtbl.find_opt etats_visites pos with
      | None -> Ok ()
      | Some (old_score, _) ->
          if score old_score < score curr_score then Error () else Ok ())
      |> function
      | Error () -> ()
      | Ok () ->
          Hashtbl.replace etats_visites pos (curr_score, prev_state);
          pathfinding_aux
            (dir, pos +-> vec_of_direction dir)
            (curr_score +-> (1, 0))
            curr_state;
          pathfinding_aux
            (rotate_90 dir, pos +-> vec_of_direction (rotate_90 dir))
            (curr_score +-> (1, 1))
            curr_state;
          pathfinding_aux
            (rotate_minus90 dir, pos +-> vec_of_direction (rotate_minus90 dir))
            (curr_score +-> (1, 1))
            curr_state
  in
  pathfinding_aux init_state (0, 0) init_state;
  let rec trace_chemin (curr_state : state) (acc : state list) =
    if curr_state = init_state then curr_state :: acc
    else
      trace_chemin
        (Hashtbl.find etats_visites (snd curr_state) |> snd)
        (curr_state :: acc)
  in
  match Hashtbl.find_opt etats_visites final_pos with
  | None -> (Int.max_int, [])
  | Some (s, state) -> (score s, trace_chemin state [])

let day16_part1 str =
  let reader, n, m = get_reader str '#' in
  let is_wall = is_wall reader
  and init_pos = find_pos_letter reader 'S' n m
  and final_pos = find_pos_letter reader 'E' n m in
  pathfinding_score n m is_wall
    (fun (a, b) -> a + (1000 * b))
    (Est, init_pos) final_pos
  |> fst

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
  Printf.printf "res=%d\n" (day16_part1 test_input);
  flush stdout

let () = Printf.printf "res=%d\n" (day16_part1 (readfile "input16.txt"))

let all_cases_of_best_path n m is_wall score init_state final_pos expected_score
    =
  let ( || ) = Vec2_Set.add and ( @ ) = Vec2_Set.union in
  let etats_visites = Hashtbl.create (n * m) in
  let get_old_score_etat state =
    match Hashtbl.find_opt etats_visites state with
    | None -> (Int.max_int, Int.max_int)
    | Some x -> x
  in
  let rec pathfinding_aux ((dir, pos) as curr_state : state)
      (curr_score : int * int) good_cases =
    if is_wall pos then Vec2_Set.empty
    else if score curr_score > score (get_old_score_etat curr_state) then
      Vec2_Set.empty
    else if score curr_score > expected_score then Vec2_Set.empty
    else if score curr_score = expected_score && pos = final_pos then
      Vec2_Set.add pos good_cases
    else (
      Hashtbl.replace etats_visites curr_state curr_score;
      pathfinding_aux
        (dir, pos +-> vec_of_direction dir)
        (curr_score +-> (1, 0))
        (pos || good_cases)
      @ pathfinding_aux
          (rotate_90 dir, pos +-> vec_of_direction (rotate_90 dir))
          (curr_score +-> (1, 1))
          (pos || good_cases)
      @ pathfinding_aux
          (rotate_minus90 dir, pos +-> vec_of_direction (rotate_minus90 dir))
          (curr_score +-> (1, 1))
          (pos || good_cases))
  in
  pathfinding_aux init_state (0, 0) Vec2_Set.empty |> Vec2_Set.to_list

let print_part2 n m is_wall cases_O =
  let mat = Array.make_matrix n m '.' in
  List.iter (fun (i, j) -> mat.(i).(j) <- 'O') cases_O;
  for i = 0 to n do
    for j = 0 to m do
      print_char (if is_wall (i, j) then '#' else mat.(i).(j))
    done;
    print_newline ()
  done

let day16_part2 str =
  let reader, n, m = get_reader str '#' in
  let is_wall = is_wall reader
  and init_state = (Est, find_pos_letter reader 'S' n m)
  and final_pos = find_pos_letter reader 'E' n m
  and score (a, b) = a + (1000 * b) in
  pathfinding_score n m is_wall score init_state final_pos
  |> fst
  |> all_cases_of_best_path n m is_wall score init_state final_pos
  |> fun cases_O ->
  print_part2 n m is_wall cases_O;
  List.length cases_O

let () =
  Printf.printf "res=%d\n" (day16_part2 test_input);
  flush stdout

let () = Printf.printf "res=%d\n" (day16_part2 (readfile "input16.txt"))
