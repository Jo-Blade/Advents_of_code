type 'a parser = (unit -> char Seq.t) -> 'a * (unit -> char Seq.t)
(** A parser has an output of type 'a and a sequence of chars to continue
    parsing. The lazy_t is an optimization to not be forced to compute the
    parser if the tail in not read *)

(** Syntax sugar *)
let ( !. ) a = a ()

exception Failed_To_Parse
(** Exception raised when a parser get an unexpected input *)

(** Run a parser on a file
    @param p the parser to execute
    @param filename the path to the file that will be read
    @return the output of the parser *)
let run_on_file (p : 'a parser) (filename : string) : 'a =
  let rec seq_of_chan f : char Seq.t =
   fun () ->
    try Seq.Cons (input_char f, seq_of_chan f) with End_of_file -> Seq.Nil
  in
  fst @@ p (fun () -> seq_of_chan (open_in filename) |> Seq.memoize)

(** Very often, I just need to pass the file as first argument, so this function
    is a shortcut to that *)
let run_on_argv1 (p : 'a parser) : 'a =
  if Array.length Sys.argv < 2 then (
    Printf.printf "Error: missing argument\nUsage: %s <path_to_aoc_input>\n"
      Sys.argv.(0);
    exit 1)
  else run_on_file p Sys.argv.(1)

(** Parser that take next char and map it through f
    @param f the map function that return None for unexpecte values
    @return the parser of the mapped char
    @raise Failed_To_Parse
      if an unsupported value is encountered (f returned None) *)
let map_next (f : char -> 'a option) : 'a parser =
 fun xs ->
  let x, ys =
    match Seq.uncons !.xs with Some x -> x | None -> raise Failed_To_Parse
  in
  ((f x |> function Some x -> x | None -> raise Failed_To_Parse), fun () -> ys)

(** `take_while f p` is the parser that takes the longest string of consecutives
    chars where every char `x` satisfies `f x`. *)
let take_while (f : char -> bool) : string parser =
 fun xs ->
  (String.of_seq @@ Seq.take_while f !.xs, fun () -> Seq.drop_while f !.xs)

(** `is_digit c` returns true if c is a digit char *)
let is_digit = function '0' .. '9' -> true | _ -> false

(** `map f p` apply function f to the output of p` *)
let map f p = fun xs -> p xs |> function out, ys -> (f out, ys)

(** uint is the parser that take the longest sequence of digits and read it as
    an unsigned integer *)
let uint = take_while is_digit |> map int_of_string

(** Execute the parsers in order and combine their outputs using f
    @param f the function to combine outputs
    @param p1 the first parser to be executed
    @param p2 the second parser to be executed *)
let combine2 (f : 'a -> 'b -> 'c) (p1 : 'a parser) (p2 : 'b parser) : 'c parser
    =
 fun xs ->
  let o1, ys = p1 xs in
  let o2, zs = p2 ys in
  (f o1 o2, zs)

(** Repeat a parser between, at least 0 time
    @param p the parser to repeat
    @return the parser whose output is the sequence of the outputs of p *)
let repeat (p : 'a parser) : 'a Seq.t parser =
  let trail = ref Seq.empty in
  let rec loop xs =
   fun () ->
    try
      let o, ys = p xs in
      let rest = loop ys in
      Seq.Cons (o, rest)
    with Failed_To_Parse ->
      trail := xs ();
      Seq.Nil
  in
  fun xs ->
    let out_seq = Seq.memoize @@ loop xs in
    ( out_seq,
      fun () ->
        Seq.iter (fun _ -> ()) out_seq;
        !trail )

(** Every type of whitespace char *)
let is_whitespace = function ' ' | '\n' | '\r' | '\t' -> true | _ -> false

(** Match 0 to many whitespaces *)
let ws = take_while is_whitespace

(** Skip result of next parser *)
let ( +<- ) p1 p2 = combine2 (fun a _ -> a) p1 p2

(** Skip result of previous parser *)
let ( +-> ) p1 p2 = combine2 (fun _ a -> a) p1 p2

(** A parser that match an unique char *)
let char c = map_next (fun c' -> if c = c' then Some c else None)

(** sep_by p_sep p repeats the parser p but interleave p_sep (whose output is
    ignored) between each match
    @param p_sep the parser of separator elements
    @param p the parser for matched elements *)
let sep_by (p_sep : _ parser) (p : 'a parser) : 'a Seq.t parser =
  combine2 (fun x xs -> fun () -> Seq.Cons (x, xs)) p @@ repeat (p_sep +-> p)
