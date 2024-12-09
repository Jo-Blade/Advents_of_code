open Angstrom

let test_input = {|2333133121414131402|}
let is_digit = function '0' .. '9' -> true | _ -> false
let digit = satisfy is_digit >>| fun n -> int_of_char n - int_of_char '0'

type disk_part =
  | File of int (* un fichier et sa taille *)
  | Free of int (* un espace libre et sa taille *)

let parse_full =
  many
    (lift2
       (fun a b -> [ a; b ])
       (digit >>| fun n -> File n)
       (digit >>| fun n -> Free n)
    <|> (digit
        >>| (fun n -> [ File n ])
        <* skip_many (char '\n')
        <* end_of_input))
  >>| List.flatten

(** A partir de l qui est le résultat du parseur, on renvoie 2 listes:
  - list_files: la liste des blocs qui contiennent un morceau de fichier
    identifiés par leur position et l'id du fichier
  - list_free: la liste des blocs qui sont libres identifiés par leur position

 ATTENTION: Les deux listes sont renvoyées dans l'ordre décroissant des positions *)
let rec separate curr_pos curr_id (l : disk_part list)
    (list_files : (int * int) list) (list_free : int list) =
  match l with
  | [] -> (list_files, list_free)
  | File n :: t ->
      separate (curr_pos + n) (curr_id + 1) t
        (List.init n (fun i -> (curr_pos + n - i - 1, curr_id)) @ list_files)
        list_free
  | Free n :: t ->
      separate (curr_pos + n) curr_id t list_files
        (List.init n (fun i -> curr_pos + n - i - 1) @ list_free)

(** ATTENTION !! list_files doit etre triee dans l'ordre decroissant
   des positions et list_free doit etre triee
   dans l'ordre croissant des positions *)
let rec defrag list_files list_free =
  match (list_files, list_free) with
  | [], _ -> []
  | _, [] -> list_files
  | (pos_file, id_file) :: t_files, pos_free :: t_free ->
      if pos_file > pos_free then (pos_free, id_file) :: defrag t_files t_free
      else list_files

let list_sum = List.fold_left ( + ) 0

let day9_part1 l =
  let list_files_dec, list_free_dec = separate 0 0 l [] [] in
  let list_free_croissant = List.rev list_free_dec in
  defrag list_files_dec list_free_croissant
  |> List.map (fun (pos, id) -> pos * id)
  |> list_sum

let apply f p input =
  parse_string ~consume:Prefix p input |> function
  | Ok l -> f l |> Printf.printf "result=%d\n"
  | Error x -> failwith ("parse_error=" ^ x)

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

let () = apply day9_part1 parse_full test_input
let () = apply day9_part1 parse_full (readfile "input9.txt")

(** Les deux listes sont renvoyées dans l'ordre décroissant des positions,
   cette fois-ci on ne découpe pas les fichiers et les espaces libres en blocs de taille 1
   - liste des files: pos, taille, id
   - liste des free: pos, taille
*)
let rec separate_part2 curr_pos curr_id (l : disk_part list)
    (list_files : (int * int * int) list) (list_free : (int * int) list) =
  match l with
  | [] -> (list_files, list_free)
  | File n :: t ->
      separate_part2 (curr_pos + n) (curr_id + 1) t
        (if n = 0 then list_files else (curr_pos, n, curr_id) :: list_files)
        list_free
  | Free n :: t ->
      separate_part2 (curr_pos + n) curr_id t list_files
        (if n = 0 then list_free else (curr_pos, n) :: list_free)

let rec pop_first_fit n list_free : int option * (int * int) list =
  match list_free with
  | [] -> (None, [])
  | (pos, taille) :: t ->
      if taille > n then (Some pos, (pos + n, taille - n) :: t)
      else if taille = n then (Some pos, t)
      else
        let res, new_list_free = pop_first_fit n t in
        (res, (pos, taille) :: new_list_free)

(** ATTENTION !! list_files doit etre triee dans l'ordre decroissant
   des positions et list_free doit etre triee
   dans l'ordre croissant des positions *)
let rec defrag_part2 list_files list_free =
  match list_files with
  | [] -> []
  | (pos_file, taille_file, id_file) :: t_files -> (
      match pop_first_fit taille_file list_free with
      | None, _ ->
          (pos_file, taille_file, id_file) :: defrag_part2 t_files list_free
      | Some pos, new_list_free ->
          if pos < pos_file then
            (pos, taille_file, id_file) :: defrag_part2 t_files new_list_free
          else
            (pos_file, taille_file, id_file) :: defrag_part2 t_files list_free)

let checksum_part2 l =
  List.map
    (fun (pos, taille, id) ->
      (* formule pour calculer la somme:
         (pos * id) + ((pos + 1) * id) + ((pos + 2) * id) + ... + ((pos + taille - 1) * id)
      *)
      ((taille * (taille - 1) / 2) + (taille * pos)) * id)
    l
  |> list_sum

let day9_part2 l =
  let list_files_dec, list_free_dec = separate_part2 0 0 l [] [] in
  let list_free_croissant = List.rev list_free_dec in
  defrag_part2 list_files_dec list_free_croissant |> checksum_part2

let () = apply day9_part2 parse_full test_input
let () = apply day9_part2 parse_full (readfile "input9.txt")
