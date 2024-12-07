let test_input =
  {|....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...|}

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
      if i < n && i >= 0 && j < m - 1 && j >= 0 then mat.(i).(j) else ' '),
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

let is_wall reader i j = match reader i j with '#' -> true | _ -> false

type direction = NORD | SUD | EST | OUEST
type position_joueur = direction * (int * int)

let get_pos reader i j : position_joueur option =
  match reader i j with
  | '^' -> Some (NORD, (i, j))
  | 'v' -> Some (SUD, (i, j))
  | '>' -> Some (EST, (i, j))
  | '<' -> Some (OUEST, (i, j))
  | _ -> None

let get_pos_joueur reader n m =
  List.init (n * m) (fun k -> get_pos reader (k / m) (k mod m))
  |> List.find (function None -> false | _ -> true)

let rotate_joueur ((dir, p) : position_joueur) =
  match dir with
  | NORD -> (EST, p)
  | EST -> (SUD, p)
  | SUD -> (OUEST, p)
  | OUEST -> (NORD, p)

let case_devant_joueur ((dir, (i, j)) : position_joueur) : int * int =
  match dir with
  | NORD -> (i - 1, j)
  | EST -> (i, j + 1)
  | SUD -> (i + 1, j)
  | OUEST -> (i, j - 1)

let avancer_joueur ((dir, (i, j)) : position_joueur) : position_joueur =
  match dir with
  | NORD -> (NORD, (i - 1, j))
  | EST -> (EST, (i, j + 1))
  | SUD -> (SUD, (i + 1, j))
  | OUEST -> (OUEST, (i, j - 1))

let next_pos_joueur reader (pos : position_joueur) : position_joueur =
  let i, j = case_devant_joueur pos in
  if is_wall reader i j then rotate_joueur pos else avancer_joueur pos

let est_dehors reader ((_, (i, j)) : position_joueur) =
  match reader i j with ' ' -> true | _ -> false

let rec trouver_chemin reader (pos : position_joueur)
    (chemin : position_joueur list) : position_joueur list =
  let new_pos = next_pos_joueur reader pos in
  if est_dehors reader new_pos then List.rev chemin
  else trouver_chemin reader new_pos (new_pos :: chemin)

let pos_uniques (pos_l : position_joueur list) =
  let order (_, (i1, j1)) (_, (i2, j2)) =
    if i1 - i2 = 0 then j1 - j2 else i1 - i2
  in
  List.sort_uniq order pos_l

let day6_part1 str =
  let reader, n, m = get_reader str in
  let first_pos = get_pos_joueur reader n m |> Option.get in
  trouver_chemin reader first_pos [ first_pos ] |> pos_uniques |> List.length

let () = Printf.printf "res=%d\n" (day6_part1 test_input)
let () = Printf.printf "res=%d\n" (day6_part1 (readfile "input6.txt"))
let in_list x = List.exists (fun y -> x = y)

(** teste si on a une boucle, check_freq permet de décider la fréquence à laquelle
    on teste la condition de validation de boucle (améliore les performances) *)
let est_boucle check_freq reader (pos : position_joueur) =
  let rec est_boucle_aux it (pos : position_joueur)
      (chemin : position_joueur list) : bool =
    let new_pos = next_pos_joueur reader pos in
    if est_dehors reader new_pos then false
    else if it mod check_freq = 0 && in_list new_pos chemin then true
    else est_boucle_aux (it + 1) new_pos (new_pos :: chemin)
  in
  est_boucle_aux 0 pos

let add_wall reader i j x y = if x = i && y = j then '#' else reader x y

let day6_part2 str =
  let reader, n, m = get_reader str in
  let first_pos = get_pos_joueur reader n m |> Option.get in
  let old_chemin =
    trouver_chemin reader first_pos [ first_pos ] |> pos_uniques
  in
  List.map
    (fun (_, (i, j)) ->
      est_boucle 3000 (add_wall reader i j) first_pos [ first_pos ])
    old_chemin
  |> List.filter (fun x -> x)
  |> List.length

let () = Printf.printf "res=%d\n" (day6_part2 test_input)
let () = Printf.printf "res=%d\n" (day6_part2 (readfile "input6.txt"))

(* ============================================================================================================= *)
(* DEUXIEME VERSION DE LA PARTIE 2 EN DÉFINISSANT UN ORDEREDTYPE ET UTILISANT UN SET DANS LA FONCTION EST_BOUCLE *)
(* ============================================================================================================= *)

module Position_joueur = struct
  type t = position_joueur

  let compare ((dir1, (i1, j1)) : position_joueur)
      ((dir2, (i2, j2)) : position_joueur) =
    let dir_to_int = function NORD -> 0 | EST -> 1 | SUD -> 2 | OUEST -> 3 in
    if i1 - i2 = 0 then
      if j1 - j2 = 0 then dir_to_int dir1 - dir_to_int dir2 else j1 - j2
    else i1 - i2
end

module Position_joueur_Set = Set.Make (Position_joueur)

(** teste si on a une boucle, check_freq permet de décider la fréquence à laquelle
    on teste la condition de validation de boucle (améliore les performances) *)
let est_boucle reader (pos : position_joueur) =
  let rec est_boucle_aux it (pos : position_joueur)
      (chemin : Position_joueur_Set.t) : bool =
    let new_pos = next_pos_joueur reader pos in
    if est_dehors reader new_pos then false
    else if Position_joueur_Set.find_opt new_pos chemin != None then true
    else
      est_boucle_aux (it + 1) new_pos (Position_joueur_Set.add new_pos chemin)
  in
  est_boucle_aux 0 pos Position_joueur_Set.empty

let day6_part2 str =
  let reader, n, m = get_reader str in
  let first_pos = get_pos_joueur reader n m |> Option.get in
  let old_chemin =
    trouver_chemin reader first_pos [ first_pos ] |> pos_uniques
  in
  List.map
    (fun (_, (i, j)) -> est_boucle (add_wall reader i j) first_pos)
    old_chemin
  |> List.filter (fun x -> x)
  |> List.length

let () = Printf.printf "res=%d\n" (day6_part2 test_input)
let () = Printf.printf "res=%d\n" (day6_part2 (readfile "input6.txt"))
