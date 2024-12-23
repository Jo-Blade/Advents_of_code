let test_input =
  {|###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############|}

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

let sign (a, b) = if a = 0 then b else a

module Vec2 = struct
  type t = int * int

  let ( - ) (a, b) (c, d) = (a - c, b - d)
  let compare (x : t) (y : t) = sign (x - y)
end

module Vec2Set = Set.Make (Vec2)

let is_wall reader (i, j) = reader i j = '#'

(* 'a = type de noeud; 'b = type de l'arête *)
type ('a, 'b) graph = 'a -> ('a * 'b) list

let voisins (i, j) = [ (i + 1, j); (i - 1, j); (i, j + 1); (i, j - 1) ]

(** cherche toutes les positions d'une lettre dans le cadre délimité par
    le coin sup gauche (i1,j1) et le coin inf droit (i2,j2) *)
let find_all_pos_letter reader letter (i1, j1) (i2, j2) =
  let n, m = (i2 - i1, j2 - j1) in
  List.init (n * m) (fun k ->
      if reader (i1 + (k / m)) (j1 + (k mod m)) = letter then
        [ (i1 + (k / m), j1 + (k mod m)) ]
      else [])
  |> List.flatten

type shortcut = Vec2.t * Vec2.t

let shortcut_list_part1 n m reader =
  List.map
    (fun pos ->
      List.filter_map
        (fun ((i, j) as x) -> if reader i j = '#' then None else Some (pos, x))
        (voisins pos))
    (find_all_pos_letter reader '#' (1, 1) (n - 1, m - 1))
  |> List.flatten

module Shortcut = struct
  type t = shortcut

  let compare ((a, b) : t) ((c, d) : t) : int =
    sign (Vec2.compare a b, Vec2.compare c d)
end

module ShortcutMap = Map.Make (Shortcut)

module NodeScore = struct
  type t = int * Vec2.t

  let compare ((x, vec1) : t) ((y, vec2) : t) =
    (x - y, compare vec1 vec2) |> sign
end

module NodeScoreSet = Set.Make (NodeScore)
module Vec2Map = Map.Make (Vec2)

(** teste si le point donné de ligne d'indice i et de colonne d'indice j
    appartient à la matrice de taille n, m *)
let in_matrix n m (i, j) =
  if i < n && i >= 0 && j < m && j >= 0 then true else false

let get_graph is_wall pos =
  if is_wall pos then []
  else
    List.filter (fun x -> not (is_wall x)) (voisins pos)
    |> List.map (fun x -> (x, 1))

let all_dijkstra_distances (graph : (Vec2.t, int) graph) (start : Vec2.t) =
  let rec all_dijkstra_distance_aux (todo : NodeScoreSet.t)
      (current_scores : int Vec2Map.t) : int Vec2Map.t =
    let maybe_update_todo_and_scores (todo : NodeScoreSet.t) scores node
        new_score =
      if
        match Vec2Map.find_opt node scores with
        | None -> true
        | Some x -> x > new_score
      then
        ( NodeScoreSet.add (new_score, node) todo,
          Vec2Map.add node new_score scores )
      else (todo, scores)
    in
    match NodeScoreSet.min_elt_opt todo with
    | Some ((score, node) as x) ->
        List.fold_left
          (fun (t, s) (nd, di) ->
            maybe_update_todo_and_scores t s nd (score + di))
          (NodeScoreSet.remove x todo, current_scores)
          (graph node)
        |> fun (new_todo, new_scores) ->
        all_dijkstra_distance_aux new_todo new_scores
    | None -> current_scores
  in
  all_dijkstra_distance_aux
    (NodeScoreSet.singleton (0, start))
    (Vec2Map.singleton start 0)

let all_dijkstra_distances2 max_allowed_score (graph : (Vec2.t, int) graph)
    (start : Vec2.t) =
  let rec all_dijkstra_distance_aux (todo : NodeScoreSet.t)
      (current_scores : int Vec2Map.t) : int Vec2Map.t =
    let maybe_update_todo_and_scores (todo : NodeScoreSet.t) scores node
        new_score =
      if
        new_score <= max_allowed_score
        &&
        match Vec2Map.find_opt node scores with
        | None -> true
        | Some x -> x > new_score
      then
        ( NodeScoreSet.add (new_score, node) todo,
          Vec2Map.add node new_score scores )
      else (todo, scores)
    in
    match NodeScoreSet.min_elt_opt todo with
    | Some ((score, node) as x) ->
        List.fold_left
          (fun (t, s) (nd, di) ->
            maybe_update_todo_and_scores t s nd (score + di))
          (NodeScoreSet.remove x todo, current_scores)
          (graph node)
        |> fun (new_todo, new_scores) ->
        all_dijkstra_distance_aux new_todo new_scores
    | None -> current_scores
  in
  all_dijkstra_distance_aux
    (NodeScoreSet.singleton (0, start))
    (Vec2Map.singleton start 0)

let test_shortcut expected_save_time dist_dest
    (scores_from_start : int Vec2Map.t) (scores_from_dest : int Vec2Map.t)
    (start_pos : Vec2.t) (end_pos : Vec2.t) shortcut_duration =
  let score_before_start_pos =
    List.filter_map
      (fun pos -> Vec2Map.find_opt pos scores_from_start)
      (voisins start_pos)
    |> List.fold_left min dist_dest
  in
  match Vec2Map.find_opt end_pos scores_from_dest with
  | None -> false
  | Some x ->
      let new_dist_dest = score_before_start_pos + x + shortcut_duration in
      dist_dest - new_dist_dest >= expected_save_time

let list_sum = List.fold_left ( + ) 0

let get_inv_graph n m pos =
  List.filter (fun x -> in_matrix n m x) (voisins pos)
  |> List.map (fun x -> (x, 1))

let shortcut is_wall inv_graph start_pos max_shortcut_duration :
    (shortcut * int) list =
  let dists_to_start =
    all_dijkstra_distances2 (max_shortcut_duration - 1) inv_graph start_pos
  in
  Vec2Map.to_list dists_to_start
  |> List.filter_map (fun (end_pos, d) ->
         if is_wall end_pos then None else Some ((start_pos, end_pos), d))

let shortcut_list_part2 n m reader shortcut_duration =
  let is_wall = is_wall reader in
  let inv_graph = get_inv_graph n m in
  List.fold_left
    (fun acc p -> shortcut is_wall inv_graph p shortcut_duration @ acc)
    []
    (find_all_pos_letter reader 'S' (0, 0) (n, m)
    @ find_all_pos_letter reader 'E' (0, 0) (n, m)
    @ find_all_pos_letter reader '.' (0, 0) (n, m))

let day20_part1 str expected_save_time =
  let reader, n, m = get_reader str '#' in
  let start_pos = find_all_pos_letter reader 'S' (0, 0) (n, m) |> List.hd
  and end_pos = find_all_pos_letter reader 'E' (0, 0) (n, m) |> List.hd
  and graph = get_graph (is_wall reader) in
  let dists_from_start = all_dijkstra_distances graph start_pos
  and dists_from_dest = all_dijkstra_distances graph end_pos
  and all_shortcuts = shortcut_list_part1 n m reader in
  let dist_dest = Vec2Map.find end_pos dists_from_start in
  Printf.printf "debug: orig_dist=%d\n" dist_dest;
  Printf.printf "debug: shortcut_list_len = %d\n" (List.length all_shortcuts);
  List.map
    (fun (a, b) ->
      flush stdout;
      test_shortcut expected_save_time dist_dest dists_from_start
        dists_from_dest a b 2
      |> Bool.to_int)
    all_shortcuts
  |> list_sum

let test_shortcut_part2 expected_save_time dist_dest
    (scores_from_start : int Vec2Map.t) (scores_from_dest : int Vec2Map.t)
    (start_pos : Vec2.t) (end_pos : Vec2.t) shortcut_duration =
  let score_before_start_pos =
    List.filter_map
      (fun pos -> Vec2Map.find_opt pos scores_from_start)
      [ start_pos ]
    |> List.fold_left min dist_dest
  in
  match Vec2Map.find_opt end_pos scores_from_dest with
  | None -> false
  | Some x ->
      let new_dist_dest = score_before_start_pos + x + shortcut_duration in
      dist_dest - new_dist_dest >= expected_save_time

let day20_part2 shortcut_duration str expected_save_time =
  let reader, n, m = get_reader str '#' in
  let start_pos = find_all_pos_letter reader 'S' (0, 0) (n, m) |> List.hd
  and end_pos = find_all_pos_letter reader 'E' (0, 0) (n, m) |> List.hd
  and graph = get_graph (is_wall reader) in
  let dists_from_start = all_dijkstra_distances graph start_pos
  and dists_from_dest = all_dijkstra_distances graph end_pos
  and all_shortcuts = shortcut_list_part2 n m reader shortcut_duration in
  let dist_dest = Vec2Map.find end_pos dists_from_start in
  Printf.printf "debug: orig_dist=%d\n" dist_dest;
  Printf.printf "debug: shortcut_list_len = %d\n" (List.length all_shortcuts);
  List.map
    (fun ((a, b), n) ->
      flush stdout;
      test_shortcut_part2 expected_save_time dist_dest dists_from_start
        dists_from_dest a b n
      |> Bool.to_int)
    all_shortcuts
  |> list_sum

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

let () = day20_part1 test_input 1 |> Printf.printf "result:%d\n"

(*
let () = day20_part1 (readfile "input20.txt") 100 |> Printf.printf "result:%d\n"
let () = flush stdout
*)
let () = day20_part2 2 test_input 1 |> Printf.printf "result:%d\n"
let () = day20_part2 18 test_input 50 |> Printf.printf "result:%d\n"
let () = flush stdout

let () =
  day20_part2 21 (readfile "input20.txt") 100 |> Printf.printf "result:%d\n"
