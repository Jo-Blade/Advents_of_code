let test_input =
  {|x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj|}

module StrMap = Map.Make (String)

type operator = bool -> bool -> bool
type wire_tree = In of bool | Gate of wire_tree * wire_tree * operator
type wire_tree_builder = In_b of bool | Gate_b of string * string * operator
type parse_wires = wire_tree_builder StrMap.t

let rec calc_tree (wire_tree : wire_tree) =
  match wire_tree with
  | In b -> b
  | Gate (w1, w2, op) -> op (calc_tree w1) (calc_tree w2)

let rec build_tree (wire_tree_builder : parse_wires) (node : string) =
  match StrMap.find_opt node wire_tree_builder with
  | None -> Error node
  | Some x -> (
      match x with
      | In_b b -> Ok (In b)
      | Gate_b (s1, s2, op) -> (
          match
            (build_tree wire_tree_builder s1, build_tree wire_tree_builder s2)
          with
          | Ok g1, Ok g2 -> Ok (Gate (g1, g2, op))
          | Error err, _ -> Error err
          | _, Error err -> Error err))

let xor = (( <> ) : bool -> bool -> bool)

module Parser = struct
  open Angstrom

  let token = take 3

  let parse_bool_of_int =
    char '1' >>| (fun _ -> true) <|> (char '0' >>| fun _ -> false)

  let parse_in =
    both (token <* string ": ") (parse_bool_of_int >>| fun b -> In_b b)

  let parse_op =
    string "OR"
    >>| (fun _ -> ( || ))
    <|> (string "AND" >>| fun _ -> ( && ))
    <|> (string "XOR" >>| fun _ -> xor)

  let parse_op_line =
    lift4
      (fun tok1 op tok2 outok -> (outok, Gate_b (tok1, tok2, op)))
      (token <* char ' ')
      (parse_op <* char ' ')
      (token <* string " -> ")
      token

  let parse_all_in = sep_by (char '\n') parse_in >>| StrMap.of_list

  let parse_all_op in_map =
    sep_by (char '\n') parse_op_line
    >>| List.fold_left
          (fun (acc_map, acc_z) (tok, tree_builder) ->
            ( StrMap.add tok tree_builder acc_map,
              if String.get tok 0 = 'z' then tok :: acc_z else acc_z ))
          (in_map, [])

  let parse = parse_all_in <* string "\n\n" >>= parse_all_op

  let apply f input (printer : 'a -> unit) =
    parse_string ~consume:Prefix (parse >>| f) input |> function
    | Ok x -> printer x
    | Error _ -> Printf.printf "Error: parsing failed\n"
end

let day24_part1 ((l_builder, l_z) : wire_tree_builder StrMap.t * string list) =
  List.fold_left
    (fun acc z ->
      build_tree l_builder z |> Result.get_ok |> fun t ->
      (2 * acc) + Bool.to_int (calc_tree t))
    0
    (List.sort String.compare l_z |> List.rev)

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

let () = Parser.apply day24_part1 test_input (Printf.printf "result:%d\n")

let () =
  Parser.apply day24_part1 (readfile "input24.txt")
    (Printf.printf "result:%d\n")
