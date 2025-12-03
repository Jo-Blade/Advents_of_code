module P = Parsercombinators.Parsers

(** Remove the first reversal in a list - e.g the first number that is smaller
    than the next one *)
let rec remove_first_rev = function
  | x1 :: x2 :: t ->
      if x1 < x2 then x2 :: t else x1 :: (remove_first_rev @@ (x2 :: t))
  | [ _ ] -> []
  | [] -> failwith "list is empty"

(** General solution for any n digits length joltages *)
let rec max_joltage_n_digits n l =
  let len = List.length l in
  if len < n then failwith "list too short"
  else if len = n then l
  else remove_first_rev @@ (List.hd l :: max_joltage_n_digits n (List.tl l))

(** Convert a list of decimal digits to an int *)
let int_of_digits l =
  let rec __int_of_digits acc = function
    | [] -> acc
    | h :: t -> __int_of_digits ((acc * 10) + h) t
  in
  __int_of_digits 0 l

(** Sum the max joltages of each bank for part 1 *)
let sum_max_joltages_n_digits n (input : int Seq.t Seq.t) =
  Seq.map
    (fun xs -> max_joltage_n_digits n @@ List.of_seq xs |> int_of_digits)
    input
  |> Seq.fold_left ( + ) 0

(** The parser for today's input *)
let p =
  let open P in
  repeat
  @@ repeat
       (map_next (function
         | '0' .. '9' as c -> Some (Char.code c - Char.code '0')
         | _ -> None))
     +<- char '\n'

let () =
  sum_max_joltages_n_digits 2 @@ P.run_on_argv1 p
  |> Printf.printf "solution part1 (2nd method) = %d\n"

let () =
  sum_max_joltages_n_digits 12 @@ P.run_on_argv1 p
  |> Printf.printf "solution part2 = %d\n"
