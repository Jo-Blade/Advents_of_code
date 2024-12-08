let test_input =
  {|............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............|}

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
    m )

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

(** teste si le point donné de ligne d'indice i et de colonne d'indice j
    appartient à la matrice de taille n, m *)
let in_matrix n m (i, j) =
  if i < n && i >= 0 && j < m - 1 && j >= 0 then true else false

(** si a, b et c sont 3 points alignés tel que a --> b --> c
    alors: trouver_point_aligne a b = c *)
let trouver_point_aligne (i1, j1) (i2, j2) = ((2 * i2) - i1, (2 * j2) - j1)

let rec couples (l : 'a list) : ('a * 'a) list =
  match l with
  | [] -> []
  | h :: t ->
      List.map (fun x -> (h, x)) t @ List.map (fun x -> (x, h)) t @ couples t

(** renvoie la liste dont chaque élément
    est une liste d'antennes de même fréquence *)
let get_antennas reader n m =
  let temp_storage = Array.make 128 [] in
  let add_antenna i j =
    match reader i j with
    | '.' -> ()
    | c ->
        let n = Char.code c in
        temp_storage.(n) <- (i, j) :: temp_storage.(n)
  in
  (* pour des raisons de performances cette fonction est écrite
     de façon itérative en utilisant un tableau mutable *)
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      add_antenna i j
    done
  done;
  Array.to_list temp_storage |> List.filter (function [] -> false | _ -> true)

let antinodes_locations n m (l : (int * int) list list) =
  let order (i1, j1) (i2, j2) = if i1 - i2 = 0 then j1 - j2 else i1 - i2 in
  (* trouver toutes les positions d'antennes *)
  List.map
    (fun (l : (int * int) list) ->
      let couples_list = couples l in
      List.map (fun (p1, p2) -> trouver_point_aligne p1 p2) couples_list)
    l
  |> List.flatten
  |> (* filtrer pour garder que celles qui sont dans le plateau *)
  List.filter (in_matrix n m)
  |> (* supprimer les doublons *) List.sort_uniq order

let day8_part1 str =
  let reader, n, m = get_reader str in
  antinodes_locations n m (get_antennas reader n m) |> List.length

let () = Printf.printf "res=%d\n" (day8_part1 test_input)
let () = Printf.printf "res=%d\n" (day8_part1 (readfile "input8.txt"))

(** renvoie la liste de toutes les positions d'antinodes
    suivant la direction de 2 antennes qui sont dans le plateau

    attention: il faut appeler cette fonction 2 fois en inversant
    l'ordre des antennes pour avoir toutes les position alignées avec ces antennes *)
let trouver_antinodes_alignes_part2 n m (i1, j1) (i2, j2) =
  let direction_i, direction_j = (i2 - i1, j2 - j1) in
  let rec aux (curr_pos_i, curr_pos_j) =
    let next_pos = (curr_pos_i + direction_i, curr_pos_j + direction_j) in
    if in_matrix n m next_pos then next_pos :: aux next_pos else []
  in
  aux (i1, j1)

let antinodes_locations_part2 n m (l : (int * int) list list) =
  let order (i1, j1) (i2, j2) = if i1 - i2 = 0 then j1 - j2 else i1 - i2 in
  (* trouver toutes les positions d'antennes *)
  List.map
    (fun (l : (int * int) list) ->
      let couples_list = couples l in
      List.map
        (fun (p1, p2) -> trouver_antinodes_alignes_part2 n m p1 p2)
        couples_list
      |> List.flatten)
    l
  |> List.flatten
  (* plus besoin de filtrer celles qui sont dans le tableau car la fonction
     trouver_antinodes_alignes_part2 vérifie déjà ça *)
  |> (* supprimer les doublons *) List.sort_uniq order

let day8_part2 str =
  let reader, n, m = get_reader str in
  antinodes_locations_part2 n m (get_antennas reader n m) |> List.length

let () = Printf.printf "res=%d\n" (day8_part2 test_input)
let () = Printf.printf "res=%d\n" (day8_part2 (readfile "input8.txt"))
