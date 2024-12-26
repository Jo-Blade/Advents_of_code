let test_input =
  {|#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####|}

let list_sum = List.fold_left ( + ) 0

module Parser = struct
  open Angstrom

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

  let grid_to_level_list str =
    let reader, _, _ = get_reader str '.' in
    let is_lock = reader 0 0 = '#' in
    let count_column j =
      List.init 5 (fun i -> if reader (1 + i) j = '#' then 1 else 0) |> list_sum
    in
    (is_lock, List.init 5 count_column)

  let grid = take ((6 * 6) + 5) >>| grid_to_level_list
  let parse = sep_by (string "\n\n") grid

  let apply f input (printer : 'a -> unit) =
    parse_string ~consume:Prefix (parse >>| f) input |> function
    | Ok x -> printer x
    | Error _ -> Printf.printf "Error: parsing failed\n"
end

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

let to_string (b, l) =
  Printf.sprintf "%s:(%s)"
    (match b with true -> "LOCK" | false -> "KEY")
    (List.map string_of_int l |> String.concat ",")

let test_overflow max_level lock key =
  List.fold_left
    (fun acc (a, b) -> acc || a + b > max_level)
    false (List.combine lock key)

let day25_part1 (l : (bool * int list) list) =
  let locks, keys = List.partition (fun (b, _) -> b) l in
  let locks = List.map snd locks and keys = List.map snd keys in
  List.map
    (fun lock -> List.map (fun key -> test_overflow 5 lock key |> not) keys)
    locks
  |> List.flatten |> List.map Bool.to_int |> list_sum

let () =
  Parser.apply (List.map to_string) test_input
    (List.iter (Printf.printf "debug:%s\n"))

let () = Parser.apply day25_part1 test_input (Printf.printf "result:%d\n")

let () =
  Parser.apply day25_part1 (readfile "input25.txt")
    (Printf.printf "result:%d\n")
