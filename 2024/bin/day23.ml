let test_input =
  {|kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn|}

module Parser = struct
  open Angstrom

  let parse_token = take 2
  let parse_line = both (parse_token <* char '-') parse_token
  let parse = sep_by (char '\n') parse_line

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
    type graph = Node.t -> Node.t list
    type graph_cout = Node.t -> (Node.t * int) list

    module NodeMap = Map.Make (Node)

    let sign (a, b) = if a = 0 then b else a

    module NodeScore = struct
      type t = int * Node.t

      let compare ((x, nd1) : t) ((y, nd2) : t) =
        (x - y, compare nd1 nd2) |> sign
    end

    module NodeScoreSet = Set.Make (NodeScore)

    let all_dijkstra_paths_from (graph : graph_cout) (max_allowed_score : int)
        (start : Node.t) =
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
          if new_score <= max_allowed_score && old_score = new_score then
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

    module NodeSet = Set.Make (Node)

    (** find all triangles of graph that contain start_node *)
    let triangles (f_graph : graph) start_node =
      let first_voisins = f_graph start_node in
      let second_voisins = List.map f_graph first_voisins in
      let second_voisins_interc =
        List.map
          (fun l ->
            NodeSet.inter (NodeSet.of_list first_voisins) (NodeSet.of_list l))
          second_voisins
      in
      List.combine first_voisins second_voisins_interc
      |> List.map (fun (a, l) ->
             List.map
               (fun x -> NodeSet.of_list [ start_node; a; x ])
               (NodeSet.to_list l))
      |> List.flatten
      |> List.sort_uniq NodeSet.compare
  end

let list_tokens (l : (string * string) list) =
  List.split l |> fun (l1, l2) -> List.sort_uniq String.compare (l1 @ l2)

module StringMap = Map.Make (String)

let get_graph (l : (string * string) list) =
  let get_mapping (x : string) (map : string list StringMap.t) =
    match StringMap.find_opt x map with None -> [] | Some l -> l
  and replace_mapping x new_mapping map =
    StringMap.remove x map |> StringMap.add x new_mapping
  in
  let aux (map : string list StringMap.t) ((a, b) : string * string) =
    replace_mapping a (b :: get_mapping a map) map
    |> replace_mapping b (a :: get_mapping b map)
  in
  fun x -> get_mapping x (List.fold_left aux StringMap.empty l)

module Network = Graph (String)

let is_admin_tok str =
  match String.to_seq str () with Seq.Cons ('t', _) -> true | _ -> false

let day23_part1 (l : (string * string) list) =
  let graph = get_graph l
  and l_toks = list_tokens l |> List.filter is_admin_tok in
  List.map (Network.triangles graph) l_toks
  |> List.flatten
  |> List.sort_uniq Network.NodeSet.compare
  |> List.length

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

let () = Parser.apply day23_part1 test_input (Printf.printf "result:%d\n")

let () =
  Parser.apply day23_part1 (readfile "input23.txt")
    (Printf.printf "result:%d\n")
