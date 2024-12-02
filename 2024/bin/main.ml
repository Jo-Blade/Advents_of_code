open Angstrom

let test_input =
  {|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4|}

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false

let endline = take_till (fun c -> c == '\n') <* take 1
let whitespace = take_while is_whitespace
let word = take_while (fun c -> not (is_whitespace c))
let is_digit = function '0' .. '9' -> true | _ -> false
let integer = take_while1 is_digit >>| int_of_string
let int_ws = integer <* whitespace

let test_parser parser str =
  parse_string ~consume:Prefix parser str |> function
  | Ok s -> print_endline ("result=" ^ s)
  | Error s -> failwith ("error=" ^ s)

let int_list = many int_ws

let () =
  test_parser
    (word *> whitespace *> int_list
    >>| List.map string_of_int >>| String.concat ",")
    test_input

let inter_map = lift3 (fun a b c -> (a, b, c)) int_ws int_ws int_ws
let parse_func = endline *> many inter_map
let parse_full = endline *> endline *> many parse_func

let _ =
  parse_string ~consume:Prefix parse_full test_input |> function
  | Ok s ->
      List.iter
        (fun l ->
          print_endline "----";
          List.iter
            (fun (n1, n2, n3) -> Printf.printf "inter=%d|%d|%d\n" n1 n2 n3)
            l)
        s
  | Error s -> failwith ("error=" ^ s)
