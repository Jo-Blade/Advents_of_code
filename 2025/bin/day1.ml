module P = Parsercombinators.Parsers

(** Parser that returns the list of signed numbers (L = negative, R = positive)
*)
let p =
  let open P in
  repeat
  @@ combine2 ( * )
       (map_next (function 'L' -> Some (-1) | 'R' -> Some 1 | _ -> None))
       uint
     +<- ws

(** Mathematical modulus, that always returns a positive integer *)
let true_mod x y = ((x mod y) + y) mod y

(** Compute all successives positions *)
let all_pos xs =
  snd
  @@ Seq.fold_left
       (fun (sum, l) n ->
         let s = true_mod (sum + n) 100 in
         (s, s :: l))
       (50, []) xs

(* Solution for the part1 *)
let () =
  P.run_on_argv1 p |> all_pos
  |>
  (* count the number of 0 *)
  List.filter (fun x -> x = 0)
  |> List.length
  |>
  (* print result *)
  Printf.printf "solution part1 = %d\n"

(** Compute the number of overflows for each move and sum them *)
let all_overflows xs =
  snd
  @@ Seq.fold_left
       (fun (pos, res) n ->
         let pos' = true_mod (pos + n) 100
         and nb_0 =
           if n >= 0 then (n + pos) / 100
           else (100 - (if pos = 0 then 100 else pos) - n) / 100
         in
         (pos', nb_0 + res))
       (50, 0) xs

(* Solution for the part2 *)
let () =
  P.run_on_argv1 p |> all_overflows
  |>
  (* print result *)
  Printf.printf "solution part2 = %d\n"
