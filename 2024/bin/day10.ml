let test_input =
  {|89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732|}

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

type location = int * int * int
(** location = i, j, height_value *)

(** Retourne tous les positions avec des 9 ateignables depuis la position i, j *)
let rec trailhead reader_int i j expected_height : location list =
  if reader_int i j = expected_height then
    if expected_height = 9 then [ (i, j, 9) ]
    else
      trailhead reader_int i (j - 1) (expected_height + 1)
      @ trailhead reader_int i (j + 1) (expected_height + 1)
      @ trailhead reader_int (i - 1) j (expected_height + 1)
      @ trailhead reader_int (i + 1) j (expected_height + 1)
  else []

let list_sum = List.fold_left ( + ) 0

let day10_part1 str =
  let reader, n, m = get_reader str in
  let reader_int x y = reader x y |> fun x -> int_of_char x - int_of_char '0' in
  List.init (n * m) (fun k ->
      trailhead reader_int (k / m) (k mod m) 0
      |> List.sort_uniq (fun (x1, y1, _) (x2, y2, _) ->
             if x1 - x2 = 0 then y1 - y2 else x1 - x2)
      |> List.length)
  |> list_sum

let () = Printf.printf "res=%d\n" (day10_part1 test_input)
let () = Printf.printf "res=%d\n" (day10_part1 (readfile "input10.txt"))

(* On supprime juste le List.sort_uniq par rapport à la partie 1 *)
let day10_part2 str =
  let reader, n, m = get_reader str in
  let reader_int x y = reader x y |> fun x -> int_of_char x - int_of_char '0' in
  List.init (n * m) (fun k ->
      trailhead reader_int (k / m) (k mod m) 0 |> List.length)
  |> list_sum

let () = Printf.printf "res=%d\n" (day10_part2 test_input)
let () = Printf.printf "res=%d\n" (day10_part2 (readfile "input10.txt"))
