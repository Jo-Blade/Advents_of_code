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

let shortcut_list n m reader =
  List.map
    (fun pos -> List.filter_map (fun ((i,j) as x) -> if reader i j = '#' then None else Some (pos, x)) (voisins pos))
    (find_all_pos_letter reader '#' (1, 1) (n - 1, m - 1))
  |> List.flatten

let get_graph is_wall pos =
  if is_wall pos then []
  else
    List.filter (fun x -> not (is_wall x)) (voisins pos)
    |> List.map (fun x -> (x, 1))

module NodeScore = struct
  type t = int * Vec2.t

  let compare ((x, vec1) : t) ((y, vec2) : t) =
    (x - y, compare vec1 vec2) |> sign
end

module NodeScoreSet = Set.Make (NodeScore)
module Vec2Map = Map.Make (Vec2)

let all_dijkstra_distances (graph : (Vec2.t, int) graph) (start : Vec2.t)
    (destination : Vec2.t) =
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
    Vec2Map.empty

let test_shortcut dist_dest (current_scores : int Vec2Map.t) (start_pos : Vec2.t)
    (end_pos : Vec2.t) =
  let score_start_pos =
    (List.filter_map
       (fun pos -> Vec2Map.find_opt pos current_scores)
       (voisins start_pos)
    |> List.fold_left min Int.max_int)
    + 1
  in
      match Vec2Map.find_opt end_pos current_scores with
      | None -> false
      | Some x -> (x <= dist_dest) && (x - (score_start_pos + 1) >= 100)

let list_sum = List.fold_left ( + ) 0

let day20_part1 str =
  let reader, n, m = get_reader str '#' in
  let start_pos = find_all_pos_letter reader 'S' (0,0) (n,m) |> List.hd
  and end_pos = find_all_pos_letter reader 'E' (0,0) (n,m) |> List.hd in
  let all_dists =
    all_dijkstra_distances (get_graph (is_wall reader)) start_pos end_pos
  and all_shortcuts = shortcut_list n m reader in
  let dist_dest = (Vec2Map.find end_pos all_dists) in
  Printf.printf "debug: orig_dist=%d\n" dist_dest;
  List.map
    (fun (((i1, j1) as a), ((i2, j2) as b)) ->
      (* if test_shortcut all_dists a b then Printf.printf "(%d,%d)->(%d,%d)\n" i1 j1 i2 j2; *)
      test_shortcut dist_dest all_dists a b |> Bool.to_int)
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

let () = day20_part1 test_input |> Printf.printf "result:%d\n"

let () = day20_part1 (readfile "input20.txt") |> Printf.printf "result:%d\n"
