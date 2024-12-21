let test_input = {|029A
980A
179A
456A
379A|}

module Parser = struct
  open Angstrom

  let is_token = function 'A' | '0' .. '9' -> true | _ -> false
  let sequence = many (satisfy is_token)
  let parse = sep_by (char '\n') sequence

  let apply f input (printer : 'a -> unit) =
    parse_string ~consume:Prefix (parse >>| f) input |> function
    | Ok x -> printer x
    | Error _ -> Printf.printf "Error: parsing failed\n"
end

module Graph =
functor
  (Node : Set.OrderedType)
  ->
  struct
    type graph_cout = Node.t -> (Node.t * int) list

    module NodeMap = Map.Make (Node)

    let sign (a, b) = if a = 0 then b else a

    module NodeScore = struct
      type t = int * Node.t

      let compare ((x, nd1) : t) ((y, nd2) : t) =
        (x - y, compare nd1 nd2) |> sign
    end

    module NodeScoreSet = Set.Make (NodeScore)

    let all_dijkstra_paths_from (graph : graph_cout) (start : Node.t) =
      let rec dijkstra_aux (todo : NodeScoreSet.t)
          (current_scores : (int * Node.t list) NodeMap.t) :
          (int * Node.t list) NodeMap.t =
        let maybe_update_todo_and_scores (todo : NodeScoreSet.t)
            (scores : (int * Node.t list) NodeMap.t) node (new_score : int)
            prev_node =
          let old_score, old_prevs_node =
            match NodeMap.find_opt node scores with
            | None -> (Int.max_int, [])
            | Some (x, prevs) -> (x, prevs)
          in
          if old_score = new_score then
            ( NodeScoreSet.add (new_score, node) todo,
              NodeMap.add node (new_score, prev_node :: old_prevs_node) scores
            )
          else if old_score > new_score then
            ( NodeScoreSet.add (new_score, node) todo,
              NodeMap.add node (new_score, [ prev_node ]) scores )
          else (todo, scores)
        in
        match NodeScoreSet.min_elt_opt todo with
        | Some ((score, node) as x) ->
            List.fold_left
              (fun (t, s) (nd, di) ->
                maybe_update_todo_and_scores t s nd (score + di) node)
              (NodeScoreSet.remove x todo, current_scores)
              (graph node)
            |> fun (new_todo, new_scores) -> dijkstra_aux new_todo new_scores
        | None -> current_scores
      in
      dijkstra_aux
        (NodeScoreSet.singleton (0, start))
        (NodeMap.singleton start (0, []))

    let extract_shortest_paths
        (dijkstra_vec2map : (int * Node.t list) NodeMap.t) (start : Node.t)
        (dest : Node.t) =
      let rec extract_shortest_paths_aux (dest : Node.t) =
        if dest = start then [ [ start ] ]
        else
          match NodeMap.find_opt dest dijkstra_vec2map with
          | None -> []
          | Some (_, prev_nodes) ->
              List.map
                (fun prev_node : Node.t list list ->
                  extract_shortest_paths_aux prev_node |> fun l ->
                  List.map (fun x -> dest :: x) l)
                prev_nodes
              |> List.flatten
      in
      extract_shortest_paths_aux dest |> fun l -> List.map List.rev l

    type 'b graph_transition = Node.t -> Node.t -> 'b
    (** une fonction qui donne les transition d'un graphe prend en parametre
    le noeud précédent et le noeud suivant et renvoie la transition *)

    let rec transitions_chemin (f_transitions : 'b graph_transition)
        (chemin : Node.t list) =
      match chemin with
      | [ _ ] | [] -> []
      | prev :: next :: t ->
          f_transitions prev next :: transitions_chemin f_transitions (next :: t)
  end

module type Keypad = sig
  val reader : int -> int -> char
  val node_pos : char -> int * int
end

module Numeric_keypad = struct
  (** teste si le point donné de ligne d'indice i et de colonne d'indice j
    appartient à la matrice de taille n, m *)
  let in_matrix n m (i, j) =
    if i < n && i >= 0 && j < m && j >= 0 then true else false

  let reader i j =
    if in_matrix 4 3 (i, j) then
      [|
        [| '7'; '8'; '9' |];
        [| '4'; '5'; '6' |];
        [| '1'; '2'; '3' |];
        [| '#'; '0'; 'A' |];
      |].(i).(j)
    else '#'

  let node_pos = function
    | 'A' -> (3, 2)
    | '0' -> (3, 1)
    | '1' -> (2, 0)
    | '2' -> (2, 1)
    | '3' -> (2, 2)
    | '4' -> (1, 0)
    | '5' -> (1, 1)
    | '6' -> (1, 2)
    | '7' -> (0, 0)
    | '8' -> (0, 1)
    | '9' -> (0, 2)
    | _ -> failwith "unknown node"
end

module Directional_keypad = struct
  (** teste si le point donné de ligne d'indice i et de colonne d'indice j
    appartient à la matrice de taille n, m *)
  let in_matrix n m (i, j) =
    if i < n && i >= 0 && j < m && j >= 0 then true else false

  let reader i j =
    if in_matrix 2 3 (i, j) then
      [| [| '#'; '^'; 'A' |]; [| '<'; 'v'; '>' |] |].(i).(j)
    else '#'

  let node_pos = function
    | 'A' -> (0, 2)
    | '^' -> (0, 1)
    | '<' -> (1, 0)
    | 'v' -> (1, 1)
    | '>' -> (1, 2)
    | _ -> failwith "unknown node"
end

module KeypadGraph =
functor
  (Keypad : Keypad)
  ->
  struct
    open Keypad

    let voisins_transitions c =
      let i, j = node_pos c in
      List.filter_map
        (fun ((i, j), t) ->
          let new_c = reader i j in
          if new_c = '#' then None else Some (new_c, t))
        [
          ((i + 1, j), 'v');
          ((i - 1, j), '^');
          ((i, j + 1), '>');
          ((i, j - 1), '<');
        ]

    let graphe_cout c =
      List.map (fun (new_c, _) -> (new_c, 1)) (voisins_transitions c)

    let graphe_transition prev next =
      List.find (fun (new_c, _) -> new_c = next) (voisins_transitions prev)
      |> snd

    module KeypadGraph = Graph (Char)

    let memo_cost = Hashtbl.create 0

    let how_to_press_cost previous_button button =
      let aux previous_button button =
        match Hashtbl.find_opt memo_cost (previous_button, button) with
        | Some x -> x
        | None ->
            let dijkstra_nodemap =
              KeypadGraph.all_dijkstra_paths_from graphe_cout previous_button
            in
            fst (KeypadGraph.NodeMap.find button dijkstra_nodemap) + 1
            |> fun x ->
            Hashtbl.replace memo_cost (previous_button, button) x;
            x
      in
      aux previous_button button

    let memo_list = Hashtbl.create 0

    (** gives the sequence to press the button from previous pos *)
    let how_to_press previous_button button =
      let aux previous_button button =
        match Hashtbl.find_opt memo_list (previous_button, button) with
        | Some l -> l
        | None ->
            let dijkstra_nodemap =
              KeypadGraph.all_dijkstra_paths_from graphe_cout previous_button
            in
            KeypadGraph.extract_shortest_paths dijkstra_nodemap previous_button
              button
            |> List.map (fun l ->
                   KeypadGraph.transitions_chemin graphe_transition l
                   |> fun l -> l @ [ 'A' ])
            |> fun l ->
            Hashtbl.replace memo_list (previous_button, button) l;
            l
      in
      aux previous_button button

    let how_to_sequence (seq : char list) : char list list list =
      let rec aux = function
        | [ _ ] | [] -> []
        | prev :: next :: t -> how_to_press prev next :: aux (next :: t)
      in
      aux ('A' :: seq)

    let how_to_sequence_cost (seq : char list) : int =
      let rec aux = function
        | [ _ ] | [] -> 0
        | prev :: next :: t -> how_to_press_cost prev next + aux (next :: t)
      in
      aux ('A' :: seq)
  end

module Numeric_keypad_graph = KeypadGraph (Numeric_keypad)
module Directional_keypad_graph = KeypadGraph (Directional_keypad)

let list_sum = List.fold_left ( + ) 0
let list_min = List.fold_left min Int.max_int

let min_cost_level2 (l : char list list list) =
  List.map
    (fun l ->
      List.map Directional_keypad_graph.how_to_sequence_cost l |> list_min)
    l
  |> list_sum

let get_min_cost_level1 () =
  let memo = Hashtbl.create 0 in
  let rec min_cost_level1 n l =
    match Hashtbl.find_opt memo (n, l) with
    | Some x -> x
    | None ->
        List.map
          (fun l ->
            List.map
              (fun li ->
                Directional_keypad_graph.how_to_sequence li
                |> if n <= 1 then min_cost_level2 else min_cost_level1 (n - 1))
              l
            |> list_min)
          l
        |> list_sum
        |> fun x ->
        Hashtbl.replace memo (n, l) x;
        x
  in
  min_cost_level1

let min_cost_level1 = get_min_cost_level1 ()

let min_cost_level0 n (seq : char list) =
  Numeric_keypad_graph.how_to_sequence seq |> min_cost_level1 n

let numeric_val l =
  List.to_seq l
  |> Seq.take (List.length l - 1)
  |> String.of_seq |> int_of_string

let day20_part1 (l : char list list) =
  List.map (fun l -> min_cost_level0 1 l * numeric_val l) l |> list_sum

let () = Parser.apply day20_part1 test_input (Printf.printf "result:%d\n")

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
  Parser.apply day20_part1 (readfile "input21.txt")
    (Printf.printf "result:%d\n")

let day20_part2 (l : char list list) =
  List.map (fun l -> min_cost_level0 24 l * numeric_val l) l |> list_sum

let () =
  flush stdout;
  Parser.apply day20_part2 test_input (Printf.printf "result:%d\n")

let () =
  Parser.apply day20_part2 (readfile "input21.txt")
    (Printf.printf "result:%d\n")
