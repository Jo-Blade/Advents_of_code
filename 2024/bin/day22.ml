let test_input = {|1
10
100
2024|}

module Parser = struct
  open Angstrom

  let is_digit = function '0' .. '9' -> true | _ -> false
  let integer = take_while1 is_digit >>| int_of_string
  let parse = sep_by (char '\n') integer

  let apply f input (printer : 'a -> unit) =
    parse_string ~consume:Prefix (parse >>| f) input |> function
    | Ok x -> printer x
    | Error _ -> Printf.printf "Error: parsing failed\n"
end

let mix value secret_number = value lxor secret_number
let prune secret_number = secret_number mod 16777216
let step0 secret_number = mix (secret_number * 64) secret_number |> prune
let step1 secret_number = mix (secret_number / 32) secret_number |> prune
let step2 secret_number = mix (secret_number * 2048) secret_number |> prune
let next_secret_number secret_number = step0 secret_number |> step1 |> step2

let rec suite value = function
  | 0 -> value
  | n -> suite (next_secret_number value) (n - 1)

let () = Printf.printf "%d\n" (suite 1 2000)
let list_sum = List.fold_left ( + ) 0
let day22_part1 l = List.map (fun n -> suite n 2000) l |> list_sum
let () = Parser.apply day22_part1 test_input (Printf.printf "result:%d\n")

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
  Parser.apply day22_part1 (readfile "input22.txt")
    (Printf.printf "result:%d\n")

let get_diffs (a, b, c, d) next_value =
  let a = a mod 10
  and b = b mod 10
  and c = c mod 10
  and d = d mod 10
  and next_value = next_value mod 10 in
  (b - a, c - b, d - c, next_value - d)

module Vec4 = struct
  type t = int * int * int * int

  (*
  let sign (a, b) = if a = 0 then b else a

  let compare (a, b, c, d) (e, f, g, h) =
    let i = Int.compare a e
    and j = Int.compare b f
    and k = Int.compare c g
    and l = Int.compare d h in
    sign (i, sign (j, sign (k, l)))
    *)
end

(*
module Vec4Map = Map.Make (Vec4)
*)

let get_suite_part2 n start_value : (Vec4.t * int) Seq.t =
  let memo = Hashtbl.create 0 in
  let rec suite ((_, b, c, d) as prevs) n =
    if n <= 0 then ()
    else
      let next_value = next_secret_number d in
      let diffs = get_diffs prevs next_value in
      (match Hashtbl.find_opt memo diffs with
      | Some _ -> ()
      | None -> Hashtbl.replace memo diffs (next_value mod 10));
      suite (b, c, d, next_value) (n - 1)
  in
  let a = start_value in
  let b = next_secret_number a in
  let c = next_secret_number b in
  let d = next_secret_number c in
  suite (a, b, c, d) (n - 3);
  Hashtbl.to_seq memo

let day22_part2 l =
  let memo = Hashtbl.create 0 in
  let sum_map (seq : (Vec4.t * int) Seq.t) =
    Seq.iter
      (fun (key, value) ->
        let old_value =
          match Hashtbl.find_opt memo key with None -> 0 | Some x -> x
        in
        Hashtbl.replace memo key (old_value + value))
      seq
  in
  List.iter (fun n -> sum_map (get_suite_part2 2000 n)) l;
  Hashtbl.to_seq memo
  |> Seq.fold_left
       (fun (a, b) (c, d) -> if b < d then (c, d) else (a, b))
       ((0, 0, 0, 0), min_int)
  |> snd

let test_input2 = {|1
2
3
2024|}

let () = Parser.apply day22_part2 test_input2 (Printf.printf "result:%d\n")

let () =
  Parser.apply day22_part2 (readfile "input22.txt")
    (Printf.printf "result:%d\n")
