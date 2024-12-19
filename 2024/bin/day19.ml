let test_input =
  {|r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb|}

module Parser = struct
  open Angstrom

  let is_whitespace = function
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false

  let whitespace = take_while is_whitespace

  let towels =
    sep_by (string ", ")
      (take_till (function ',' | '\n' -> true | _ -> false))

  let designs = sep_by (char '\n') (take_till is_whitespace)
  let parse = both (towels <* whitespace) designs

  let apply f input (printer : 'a -> unit) =
    parse_string ~consume:Prefix (parse >>| f) input |> function
    | Ok x -> printer x
    | Error _ -> Printf.printf "Error: parsing failed\n"
end

let is_prefix design towel =
  let nd = String.length design in
  let n = String.length towel in
  if n > nd then None
  else if String.sub design 0 n = towel then Some (String.sub design n (nd - n))
  else None

let count_possibilities =
  let memoire = Hashtbl.create 1000 in
  let rec is_possible_aux towels design =
    match Hashtbl.find_opt memoire (towels, design) with
    | Some x -> x
    | None ->
        if design = "" then 1
        else
          let x =
            List.filter_map (is_prefix design) towels
            |> List.fold_left (fun acc x -> acc + is_possible_aux towels x) 0
          in
          Hashtbl.replace memoire (towels, design) x;
          x
  in
  is_possible_aux

let day19_part1 (towels, designs) =
  List.map (fun s -> count_possibilities towels s) designs
  |> List.filter (fun x -> x > 0)
  |> List.length

let () = Parser.apply day19_part1 test_input (Printf.printf "result:%d\n")

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
  Parser.apply day19_part1 (readfile "input19.txt")
    (Printf.printf "result:%d\n")

let list_sum = List.fold_left ( + ) 0

let day19_part2 (towels, designs) =
  List.map (fun s -> count_possibilities towels s) designs |> list_sum

let () = Parser.apply day19_part2 test_input (Printf.printf "result:%d\n")

let () =
  Parser.apply day19_part2 (readfile "input19.txt")
    (Printf.printf "result:%d\n")
