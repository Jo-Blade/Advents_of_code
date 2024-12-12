let test_input =
  {|RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE|}

(** Obtenir une fonction qui permet de lire un caractère du tableau
    à partir de ses indices de ligne et colonne.
    Si on déborde, on renvoie '.'.
    on renvoie aussi le nombre de lignes et de colonnes du tableau *)
let get_reader str =
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
      if i < n && i >= 0 && j < m - 1 && j >= 0 then mat.(i).(j) else '.'),
    n,
    m - 1 )

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

module Vec2 = struct
  type t = int * int

  let compare (i1, j1) (i2, j2) = if i1 - i2 = 0 then j1 - j2 else i1 - i2
end

module Vec2_Set = Set.Make (Vec2)

(*
let composantes_connexes reader n m =
  let non_visites =
    List.fold_left
      (fun (acc : Vec2_Set.t) x -> Vec2_Set.add x acc)
      Vec2_Set.empty
      (List.init (n * m) (fun k -> (k / m, k mod m)))
  in
  let rec parcours_comp sym (non_visites : Vec2_Set.t) (i, j) :
      Vec2_Set.t * (int * int) list =
    if Vec2_Set.exists (fun x -> x = (i, j)) non_visites then
      if reader i j = sym then
        let new_visites0 = Vec2_Set.remove (i, j) non_visites in
        let new_visites1, l1 = parcours_comp sym new_visites0 (i + 1, j) in
        let new_visites2, l2 = parcours_comp sym new_visites1 (i - 1, j) in
        let new_visites3, l3 = parcours_comp sym new_visites2 (i, j + 1) in
        let new_visites4, l4 = parcours_comp sym new_visites3 (i, j - 1) in
        (new_visites4, (i, j) :: (l1 @ l2 @ l3 @ l4))
      else (non_visites, [])
    else (non_visites, [])
  in
  let rec all_comp non_visites l =
    match Vec2_Set.choose_opt non_visites with
    | None -> l
    | Some (i, j) ->
        let new_visites, l1 = parcours_comp (reader i j) non_visites (i, j) in
        all_comp new_visites (l1 :: l)
  in
  all_comp non_visites []
  *)

(** teste si le point donné de ligne d'indice i et de colonne d'indice j
    appartient à la matrice de taille n, m *)
let in_matrix n m (i, j) =
  if i < n && i >= 0 && j < m && j >= 0 then true else false

let composantes_connexes reader n m =
  let visites = Array.make_matrix n m false in
  let rec parcours_comp sym (i, j) : (int * int) list =
    if (not (in_matrix n m (i, j))) || visites.(i).(j) then []
    else if reader i j = sym then (
      visites.(i).(j) <- true;
      (i, j)
      :: (parcours_comp sym (i + 1, j)
         @ parcours_comp sym (i - 1, j)
         @ parcours_comp sym (i, j + 1)
         @ parcours_comp sym (i, j - 1)))
    else []
  in
  let rec all_comp k l =
    if k = n * m then l
    else
      let i, j = (k / m, k mod m) in
      if visites.(i).(j) then all_comp (k + 1) l
      else
        let l1 = parcours_comp (reader i j) (i, j) in
        all_comp (k + 1) (l1 :: l)
  in
  all_comp 0 []

let longueur_cloture reader (i, j) =
  let sym = reader i j in
  List.filter
    (fun (x, y) -> reader x y != sym)
    [ (i + 1, j); (i - 1, j); (i, j + 1); (i, j - 1) ]
  |> List.length

let list_sum = List.fold_left ( + ) 0

let day12_part1 str =
  let reader, n, m = get_reader str in
  let comp = composantes_connexes reader n m in
  List.map
    (fun l ->
      (List.map (longueur_cloture reader) l |> list_sum) * List.length l)
    comp
  |> list_sum

let () = Printf.printf "res=%d\n" (day12_part1 test_input)
let () = Printf.printf "res=%d\n" (day12_part1 (readfile "input12.txt"))

(** Une bordure est définie par sa position Top/Bottom * index_ligne
    ou Left/Right * index_colonne
    et l'index de la colonne/ligne de debut (exclu) et de fin (inclu)
    de la bordure *)
type border =
  | Top of int * (int * int)
  | Bottom of int * (int * int)
  | Left of int * (int * int)
  | Right of int * (int * int)

let fusion_inter (x1, y1) (x2, y2) : (int * int) list =
  if x1 <= y2 then
    if x2 <= y1 then [ (min x1 x2, max y1 y2) ] else [ (x1, y1); (x2, y2) ]
  else [ (x2, y2); (x1, y1) ]

let fusion_bordure (b1 : border) (b2 : border) : border list =
  match (b1, b2) with
  | Top (p1, inter1), Top (p2, inter2) ->
      if p1 = p2 then
        List.map (fun x -> Top (p1, x)) (fusion_inter inter1 inter2)
      else [ b1; b2 ]
  | Bottom (p1, inter1), Bottom (p2, inter2) ->
      if p1 = p2 then
        List.map (fun x -> Bottom (p1, x)) (fusion_inter inter1 inter2)
      else [ b1; b2 ]
  | Left (p1, inter1), Left (p2, inter2) ->
      if p1 = p2 then
        List.map (fun x -> Left (p1, x)) (fusion_inter inter1 inter2)
      else [ b1; b2 ]
  | Right (p1, inter1), Right (p2, inter2) ->
      if p1 = p2 then
        List.map (fun x -> Right (p1, x)) (fusion_inter inter1 inter2)
      else [ b1; b2 ]
  | _ -> [ b1; b2 ]

let rec fusion_list_bordure (l : border list) : border list =
  match l with
  | [ _ ] | [] -> l
  | x :: y :: t -> (
      match fusion_bordure x y with
      | [ x ] -> fusion_list_bordure (x :: t)
      | [ x2; y2 ] -> x2 :: fusion_list_bordure (y2 :: t)
      | _ -> failwith "impossible")

let sort_bordure (l : border list) =
  let dir2int = function
    | Top (x, y) -> (0, x, y)
    | Bottom (x, y) -> (1, x, y)
    | Left (x, y) -> (2, x, y)
    | Right (x, y) -> (3, x, y)
  and order (x1, y1, z1) (x2, y2, z2) =
    let diff = Vec2.compare (x1, y1) (x2, y2) in
    if diff = 0 then Vec2.compare z1 z2 else diff
  in
  List.sort (fun (a : border) (b : border) -> order (dir2int a) (dir2int b)) l

let bordure reader (i, j) =
  let sym = reader i j in
  (if reader (i + 1) j != sym then [ Bottom (i, (j - 1, j)) ] else [])
  @ (if reader (i - 1) j != sym then [ Top (i, (j - 1, j)) ] else [])
  @ (if reader i (j + 1) != sym then [ Right (j, (i - 1, i)) ] else [])
  @ if reader i (j - 1) != sym then [ Left (j, (i - 1, i)) ] else []

let day12_part2 str =
  let reader, n, m = get_reader str in
  let comp = composantes_connexes reader n m in
  List.map
    (fun l ->
      (List.map (bordure reader) l
      |> List.flatten |> sort_bordure |> fusion_list_bordure |> List.length)
      * List.length l)
    comp
  |> list_sum

let () = Printf.printf "res=%d\n" (day12_part2 test_input)
let () = Printf.printf "res=%d\n" (day12_part2 (readfile "input12.txt"))
