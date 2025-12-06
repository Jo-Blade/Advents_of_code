module P = Aoc25.Parsers

type sized_int = int * int
(** an int with the first power of 10 higher of itself *)

(** Just increase the int and don't forget to increase the power of 10 if n >=
    p10 *)
let next_sized_int ((n, p10) : sized_int) =
  if n + 1 >= p10 then (n + 1, 10 * p10) else (n + 1, p10)

(** Build the infinite sequence of the increasing sized_int following n
    @param n the first sized_int of the sequence *)
let rec sized_int_seq_builder n : sized_int Seq.t =
 fun () -> Seq.Cons (n, sized_int_seq_builder @@ next_sized_int n)

(** Build the infinite sequence of the increasing invalid ids from the sequence
    of sized_int. An invalid id is a number whose decimal representation is the
    concatenation of a smaller number twice. As with sized_int we know the next
    power of ten, the construction of invalid ids from them is trivial *)
let invalid_ids_seq : int Seq.t =
  Seq.map (fun (n, p10) -> (n * p10) + n) @@ sized_int_seq_builder (1, 10)

let invalid_ids_part1 max_inter_val =
  Seq.take_while (fun x -> x <= max_inter_val) invalid_ids_seq

(** A type to represent the intervals *)
type interval = Interval of int * int

(** in_interval x inter returns true if x belongs to interval inter *)
let in_interval (x : int) (Interval (n1, n2)) = x >= n1 && x <= n2

(** Keep only the invalid ids that belong to at least one interval and sum them
*)
let sum_invalid_ids (invalid_ids_seq : int -> int Seq.t)
    (inter_seq : interval Seq.t) =
  let inter_l = List.of_seq inter_seq in
  let
      (* the max possible value of all intervals *)
      max_inter_val =
    List.fold_left Int.max min_int
    @@ List.map (fun (Interval (_, n)) -> n) inter_l
  in
  Seq.filter_map (fun x ->
      List.find_map (fun i -> if in_interval x i then Some x else None) inter_l)
  @@ invalid_ids_seq max_inter_val
  |> Seq.fold_left ( + ) 0

(** Compute the finite sequence of all concatenations of n < p10 that are
    smaller than max
    @param max the maximum value to stop the sequence
    @param n the first part of the sized_int (n, p10)
    @param n the second part of the sized_int (n, p10)
    @param acc the accumulator, MUST BE INITIALIZED TO N *)
let rec all_concat_n_seq max n p10 acc =
 fun () ->
  if acc <= max / p10 then
    Seq.Cons ((acc * p10) + n, all_concat_n_seq max n p10 ((acc * p10) + n))
  else Seq.Nil

(** For each sized_int compute all its possible concatenations and then remove
    doubles *)
let invalid_ids_part2 max_inter_val : int Seq.t =
  Seq.flat_map (fun (n, p10) -> all_concat_n_seq max_inter_val n p10 n)
  @@ Seq.take_while (fun (n, p10) -> n <= max_inter_val / p10)
  @@ sized_int_seq_builder (1, 10)
  |>
  (* Remove doubles in the result *)
  fun xs ->
  List.sort_uniq Int.compare @@ List.of_seq xs
  |>
  (* Match function signature *)
  List.to_seq

(** The parser for today's input *)
let p =
  let open P in
  sep_by (char ',')
  @@ combine2 (fun n1 n2 -> Interval (n1, n2)) (uint +<- char '-') uint

let () =
  sum_invalid_ids invalid_ids_part1 @@ P.run_on_argv1 p
  |> Printf.printf "solution part1 = %d\n"

let () =
  sum_invalid_ids invalid_ids_part2 @@ P.run_on_argv1 p
  |> Printf.printf "solution part1 = %d\n"
