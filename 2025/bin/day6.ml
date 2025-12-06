open Aoc25

type operations = Sum | Product

(** Transposition of a matrix represented with 'a list list source:
    https://rosettacode.org/wiki/Matrix_transposition *)
let rec transpose m =
  assert (m <> []);
  if List.mem [] m then []
  else List.map List.hd m :: transpose (List.map List.tl m)

(** The parser for today's input *)
let p =
  let open Parsers in
  combine2
    (fun vals operators -> List.combine (List.of_seq operators) vals)
    (map (fun xss -> transpose @@ List.of_seq @@ (Seq.map List.of_seq) xss)
    @@ grid (spaces +-> uint +<- spaces))
    (char '\n'
    +-> repeat
          (spaces
          +-> map_next (function
                | '+' -> Some Sum
                | '*' -> Some Product
                | _ -> None)))

let execute_op (op, vals) =
  List.fold_left
    (match op with Sum -> ( + ) | Product -> ( * ))
    (match op with Sum -> 0 | Product -> 1)
    vals

let () =
  List.map execute_op @@ Parsers.run_on_argv1 p
  |> List.fold_left ( + ) 0
  |> Printf.printf "solution part1 = %d\n"
