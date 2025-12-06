type 'a parser = (unit -> char Seq.t) -> 'a option * (unit -> char Seq.t)
(** A parser has an output of type 'a and a sequence of chars to continue
    parsing. The lazy_t is an optimization to not be forced to compute the
    parser if the tail in not read *)

(** Syntax sugar *)
let ( !. ) a = a ()

(** Run a parser on a file
    @param p the parser to execute
    @param filename the path to the file that will be read
    @return the output of the parser *)
let run_on_file (p : 'a parser) (filename : string) : 'a =
  let rec seq_of_chan f : char Seq.t =
   fun () ->
    try Seq.Cons (input_char f, seq_of_chan f) with End_of_file -> Seq.Nil
  in
  Option.get @@ fst
  @@ p (fun () -> seq_of_chan (open_in filename) |> Seq.memoize)

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
  match Seq.uncons !.xs with
  | None -> (None, xs)
  | Some (x, ys) -> (f x, fun () -> ys)

(** `take_while f p` is the parser that takes the longest string of consecutives
    chars where every char `x` satisfies `f x`. *)
let take_while (f : char -> bool) : string parser =
 fun xs ->
  ( Some (String.of_seq @@ Seq.take_while f !.xs),
    fun () -> Seq.drop_while f !.xs )

(** `is_digit c` returns true if c is a digit char *)
let is_digit = function '0' .. '9' -> true | _ -> false

(** `map f p` apply function f to the output of p` *)
let map (f : 'a -> 'b) (p : 'a parser) : 'b parser =
 fun xs ->
  match p xs with Some out, ys -> (Some (f out), ys) | None, ys -> (None, ys)

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
  match o1 with
  | None -> (None, ys)
  | Some o1 -> (
      let o2, zs = p2 ys in
      match o2 with None -> (None, zs) | Some o2 -> (Some (f o1 o2), zs))

(** Repeat a parser between, at least 0 time
    @param p the parser to repeat
    @return the parser whose output is the sequence of the outputs of p *)
let repeat (p : 'a parser) : 'a Seq.t parser =
  let trail = ref Seq.empty in
  let rec loop xs =
   fun () ->
    let o, ys = p xs in
    match o with
    | Some o -> Seq.Cons (o, loop ys)
    | None ->
        trail := xs ();
        Seq.Nil
  in
  fun xs ->
    let out_seq = Seq.memoize @@ loop xs in
    ( Some out_seq,
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

(** Repeat a parser exactly n times
    @param n the number of repetitions
    @param p the parser to repeat
    @return the parser whose output is the sequence of the outputs of p *)
let repeat_n n (p : 'a parser) : 'a Seq.t parser =
  let rec loop n xs : 'a list option * (unit -> char Seq.t) =
    if n = 0 then (Some [], xs)
    else
      let o, ys = p xs in
      match o with
      | None -> (None, ys)
      | Some o -> (
          match loop (n - 1) ys with
          | Some rest, zs -> (Some (o :: rest), zs)
          | None, zs -> (None, zs))
  in
  fun xs ->
    match loop n xs with
    | Some l, ys -> (Some (List.to_seq l), ys)
    | None, ys -> (None, ys)

(** chain_map p f executes the parser p and then pass its output to f to parse
    the rest of the input
    @param p the first parser to execute
    @param f
      the function to generate the second parser to execute using the output of
      p
    @return a new parser that executes in order p and f (output of p) *)
let chain_map (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
 fun xs ->
  let o, ys = p xs in
  match o with
  | None -> (None, ys)
  | Some o ->
      let p2 = f o in
      p2 ys

(** Syntax sugar for chain_map *)
let ( >>= ) = chain_map

(** Parse a grid like there is in some AOC challenges
    @param f
      a function to map chars of the grid to any type, it must return None for
      any char that is not valid in the grid
    @return
      a parser that output the double sequence to iterate through the full grid
*)
let grid (f : char -> 'a option) : 'a Seq.t Seq.t parser =
  let p = map_next f in
  (* compute first line to know grid width *)
  repeat p +<- char '\n'
  >>=
  (* compute following lines *)
  fun first_line_seq ->
  sep_by (char '\n') @@ repeat_n (Seq.length first_line_seq) p
  |>
  (* prepend first line to result *)
  map (fun xss -> fun () -> Seq.Cons (first_line_seq, xss))

(** Parse a grid like using the "grid" parser and create a function that return
    any element of the grid given coordinates
    @param oob the default value when coordinates are outside the grid
    @param f
      a function to map chars of the grid to any type, it must return None for
      any char that is not valid in the grid
    @return
      the size of the grid and a function g such as g (i,j) return f(c_i_j)
      where c_i_j is the char of the grid at line i and column j *)
let grid_fun ~oob f =
  let create_fun (xss : 'a Seq.t Seq.t) =
    let lines = Seq.length xss
    and columns =
      match Seq.uncons xss with
      | Some (xs, _) -> Seq.length xs
      | None -> failwith "grid is empty"
    in
    ( (lines, columns),
      let buffer = Array.make_matrix lines columns oob in
      (* fill the matrix *)
      Seq.iteri (fun i xs -> Seq.iteri (fun j x -> buffer.(i).(j) <- x) xs) xss;
      (* generate the grid function *)
      fun (i, j) ->
        if (i >= 0 && i < lines) && j >= 0 && j < columns then buffer.(i).(j)
        else oob )
  in
  map create_fun (grid f)
