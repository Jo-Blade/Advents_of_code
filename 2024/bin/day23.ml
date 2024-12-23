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

    module NodeSetSet = Set.Make (NodeSet)

    (** Implementation de l'algo de bronKerbosch avec pivot,
        voir https://fr.wikipedia.org/wiki/Algorithme_de_Bron-Kerbosch *)
    let bronKerbosch (graph : graph) (nodes : Node.t list) : NodeSetSet.t =
      let memo = Hashtbl.create (List.length nodes) in
      List.iter (fun s -> Hashtbl.add memo s (NodeSet.of_list (graph s))) nodes;
      let rec bronKerbosch_aux (r : NodeSet.t) (p : NodeSet.t) (x : NodeSet.t) :
          NodeSetSet.t =
        if NodeSet.is_empty p && NodeSet.is_empty x then NodeSetSet.singleton r
        else
          let u = NodeSet.choose (NodeSet.union p x) in
          let voisins_u = Hashtbl.find memo u in
          List.fold_left
            (fun acc v ->
              NodeSetSet.union acc
                (let voisins_v = Hashtbl.find memo v in
                 bronKerbosch_aux (NodeSet.add v r)
                   (NodeSet.inter p voisins_v)
                   (NodeSet.inter x voisins_v)))
            NodeSetSet.empty
            (NodeSet.to_list (NodeSet.diff p voisins_u))
      in
      bronKerbosch_aux NodeSet.empty (NodeSet.of_list nodes) NodeSet.empty
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

let day23_part2 (l : (string * string) list) =
  let graph = get_graph l and l_toks = list_tokens l in
  Network.bronKerbosch graph l_toks
  |> Network.NodeSetSet.to_list
  |> List.sort (fun a b ->
         -Int.compare (Network.NodeSet.cardinal a) (Network.NodeSet.cardinal b))
  |> List.hd |> Network.NodeSet.to_list

let print_list l = String.concat "," l

let () =
  Parser.apply day23_part2 test_input (fun l ->
      Printf.printf "result:%s\n" (print_list l))

let () = flush stdout

let () =
  Parser.apply day23_part2 (readfile "input23.txt") (fun l ->
      Printf.printf "result:%s\n" (print_list l))
