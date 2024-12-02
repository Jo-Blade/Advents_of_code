let test_input =
  {|....XXMAS.
.SAMXMS...
...S..A...
..A.A.MS.X
XMASAMX.MM
X.....XA.A
S.S.S.S.SS
.A.A.A.A.A
..M.M.M.MM
.X.X.XMASX|}

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

let rec check_pattern (reader : int -> int -> 'a) (pattern : 'a list)
    (d_i : int) (d_j : int) (i : int) (j : int) =
  match pattern with
  | [] -> true
  | h :: t ->
      if reader i j = h then check_pattern reader t d_i d_j (i + d_i) (j + d_j)
      else false

let full_checker reader pattern i j =
  let directions =
    [ (1, 0); (0, 1); (-1, 0); (0, -1); (1, 1); (-1, -1); (1, -1); (-1, 1) ]
  in
  List.map
    (fun (d_i, d_j) -> check_pattern reader pattern d_i d_j i j)
    directions

let day4_part1 str =
  let reader, n, m = get_reader str in
  let checker = full_checker reader [ 'X'; 'M'; 'A'; 'S' ] in
  List.init (n * m) (fun k -> checker (k / m) (k mod m))
  |> List.flatten
  |> List.filter (fun x -> x)
  |> List.length

let () = Printf.printf "res=%d\n" (day4_part1 test_input)

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

let () = Printf.printf "res=%d\n" (day4_part1 (readfile "input4.txt"))

let test_input2 =
  {|.M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
..........|}

let x_checker reader pattern i j =
  let partial_checker = check_pattern reader pattern in
  (partial_checker 1 1 (i - 1) (j - 1)
  || partial_checker (-1) (-1) (i + 1) (j + 1))
  && (partial_checker (-1) 1 (i + 1) (j - 1)
     || partial_checker 1 (-1) (i - 1) (j + 1))

let day4_part2 str =
  let reader, n, m = get_reader str in
  let checker = x_checker reader [ 'M'; 'A'; 'S' ] in
  List.init (n * m) (fun k -> checker (k / m) (k mod m))
  |> List.filter (fun x -> x)
  |> List.length

let () = Printf.printf "res=%d\n" (day4_part2 test_input2)
let () = Printf.printf "res=%d\n" (day4_part2 (readfile "input4.txt"))
