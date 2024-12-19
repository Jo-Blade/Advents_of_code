let test_input =
  {|5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0|}

(** teste si le point donné de ligne d'indice i et de colonne d'indice j
    appartient à la matrice de taille n, m *)
let in_matrix n m (i, j) =
  if i < n && i >= 0 && j < m && j >= 0 then true else false

let sign (a, b) = if a = 0 then b else a

module Vec2 = struct
  type t = int * int

  let ( - ) (a, b) (c, d) = (a - c, b - d)
  let compare (x : t) (y : t) = sign (x - y)
end

module Vec2Set = Set.Make (Vec2)

let is_wall_set n m (l : (int * int) list) =
  let set = Vec2Set.of_list l in
  fun pos ->
    if in_matrix n m pos then
      match Vec2Set.find_opt pos set with None -> false | Some _ -> true
    else true

module Parser = struct
  open Angstrom

  let is_digit = function '0' .. '9' -> true | _ -> false
  let integer = take_while1 is_digit >>| int_of_string
  let parse = sep_by (char '\n') (both (integer <* char ',') integer)

  let apply f input (printer : 'a -> unit) =
    parse_string ~consume:Prefix (parse >>| f) input |> function
    | Ok x -> printer x
    | Error _ -> Printf.printf "Error: parsing failed\n"
end

let rec list_cut n l =
  if n = 0 then []
  else
    match l with
    | [] -> failwith "too short list"
    | h :: t -> h :: list_cut (n - 1) t

(* 'a = type de noeud; 'b = type de l'arête *)
type ('a, 'b) graph = 'a -> ('a * 'b) list

let get_graph is_wall ((i, j) as pos) =
  if is_wall pos then []
  else
    List.filter
      (fun x -> not (is_wall x))
      [ (i + 1, j); (i - 1, j); (i, j + 1); (i, j - 1) ]
    |> List.map (fun x -> (x, 1))

module NodeScore = struct
  type t = int * Vec2.t

  let compare ((x, vec1) : t) ((y, vec2) : t) =
    (x - y, compare vec1 vec2) |> sign
end

module NodeScoreSet = Set.Make (NodeScore)
module Vec2Map = Map.Make (Vec2)

let dijkstra_distance (graph : (Vec2.t, int) graph) (start : Vec2.t)
    (destination : Vec2.t) =
  let rec dijkstra_distance_aux (todo : NodeScoreSet.t)
      (current_scores : int Vec2Map.t) (dest_score : int) =
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
        dijkstra_distance_aux new_todo new_scores
          (if node = destination && score < dest_score then score
           else dest_score)
    | None -> dest_score
  in
  dijkstra_distance_aux
    (NodeScoreSet.singleton (0, start))
    Vec2Map.empty Int.max_int

let day18_part1 n m k (full_pxl_list : (int * int) list) =
  let pxl_list = list_cut k full_pxl_list in
  (* Printf.printf "debug: list length = %d\n" (List.length pxl_list); *)
  flush stdout;
  let is_wall = is_wall_set n m pxl_list in
  dijkstra_distance (get_graph is_wall) (0, 0) (n - 1, m - 1)

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
  Parser.apply (day18_part1 7 7 12) test_input (Printf.printf "result:%d\n")

let () =
  Parser.apply (day18_part1 71 71 1024) (readfile "input18.txt")
    (flush stdout;
     Printf.printf "result:%d\n")

let day18_part2 n m (full_pxl_list : (int * int) list) =
  let rec dichot borne_min borne_max =
    if borne_min = borne_max then borne_max
    else
      let k = ((borne_max - borne_min) / 2) + borne_min in
      if day18_part1 n m k full_pxl_list = Int.max_int then dichot borne_min k
      else dichot (k + 1) borne_max
  in
  dichot 0 (List.length full_pxl_list) |> fun k ->
  list_cut k full_pxl_list |> List.rev |> List.hd

let () =
  Parser.apply (day18_part2 7 7) test_input (fun (x, y) ->
      Printf.printf "result:%d,%d\n" x y)

let () =
  Parser.apply (day18_part2 71 71) (readfile "input18.txt") (fun (x, y) ->
      Printf.printf "result:%d,%d\n" x y)
