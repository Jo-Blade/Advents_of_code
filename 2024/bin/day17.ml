open Angstrom

let test_input =
  {|Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0|}

let test_input2 =
  {|Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0|}

type registers = { a : int; b : int; c : int }
type computer_channel = int list

type computer_state = registers * int * computer_channel
(** registre * instruction_pointer * stdout (list of outputs) *)

type full_instr = computer_state -> computer_state
type programme = int -> full_instr option

let prog_of_list l : programme =
  let tab = Array.of_list l in
  fun n -> if n < Array.length tab then Some tab.(n) else None

let rec execution (prog : programme) ((_, ip, _) as curr_state : computer_state)
    : computer_state =
  match prog ip with
  | None -> curr_state
  | Some inst -> execution prog (inst curr_state)

let ip_inc (regs, ip, stdout) = (regs, ip + 1, stdout)

let adv op (({ a = n; _ } as regs), ip, stdout) =
  ({ regs with a = n / (1 lsl op regs) }, ip, stdout) |> ip_inc

let bxl op (({ b = n; _ } as regs), ip, stdout) =
  ({ regs with b = n lxor op regs }, ip, stdout) |> ip_inc

let bst op (regs, ip, stdout) =
  ({ regs with b = op regs mod 8 }, ip, stdout) |> ip_inc

let jnz op (({ a = n; _ } as regs), ip, stdout) =
  (regs, (if n = 0 then ip + 1 else op regs), stdout)

let bxc _ (({ b = nb; c = nc; _ } as regs), ip, stdout) =
  ({ regs with b = nb lxor nc }, ip, stdout) |> ip_inc

let append chan n = n :: chan
let to_string chan = String.concat "," (List.rev (List.map string_of_int chan))

let out op (regs, ip, stdout) =
  let ( >> ) = append in
  stdout >> op regs mod 8 |> fun stdout -> (regs, ip, stdout) |> ip_inc

let bdv op (({ a = n; _ } as regs), ip, stdout) =
  ({ regs with b = n / (1 lsl op regs) }, ip, stdout) |> ip_inc

let cdv op (({ a = n; _ } as regs), ip, stdout) =
  ({ regs with c = n / (1 lsl op regs) }, ip, stdout) |> ip_inc

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

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false

let whitespace = take_while is_whitespace
let is_digit = function '0' .. '9' -> true | _ -> false
let integer = take_while1 is_digit >>| int_of_string

let reg_parse =
  string "Register " *> any_char *> string ": " *> integer <* whitespace

let regs_parse = lift3 (fun a b c -> { a; b; c }) reg_parse reg_parse reg_parse
let literal_operand_of_int n _ = n

let combo_operand_of_int (x : int) =
  match x with
  | 0 -> fun _ -> 0
  | 1 -> fun _ -> 1
  | 2 -> fun _ -> 2
  | 3 -> fun _ -> 3
  | 4 -> fun { a = n; _ } -> n
  | 5 -> fun { b = n; _ } -> n
  | 6 -> fun { c = n; _ } -> n
  | _ -> failwith "invalid litteral_operand"

let instruction_of_ints (opcode : int) (operand : int) =
  match opcode with
  | 0 -> adv (combo_operand_of_int operand)
  | 1 -> bxl (literal_operand_of_int operand)
  | 2 -> bst (combo_operand_of_int operand)
  | 3 -> jnz (literal_operand_of_int operand)
  | 4 -> bxc (literal_operand_of_int operand)
  | 5 -> out (combo_operand_of_int operand)
  | 6 -> bdv (combo_operand_of_int operand)
  | 7 -> cdv (combo_operand_of_int operand)
  | _ -> failwith "invalid opcode"

let parse_instr = both (integer <* char ',') integer
let parse_prog = string "Program: " *> sep_by (char ',') parse_instr
let parse_full = both regs_parse parse_prog

let day17_part1 input =
  parse_string ~consume:Prefix parse_full input |> function
  | Ok (regs, prog) ->
      execution
        (prog_of_list (List.map (fun (a, b) -> instruction_of_ints a b) prog))
        (regs, 0, [])
      |> fun (_, _, stdout) -> Printf.printf "result=%s\n" (to_string stdout)
  | Error x -> failwith ("parse_error=" ^ x)

let rec test n = function [] -> n | h :: t -> test ((n lsl 3) lor h) t

let rec same_prefixe l1 l2 n =
  if n = 0 then true
  else
    match (l1, l2) with
    | h1 :: t1, h2 :: t2 ->
        if h1 = h2 then same_prefixe t1 t2 (n - 1) else false
    | _ -> false

let rec bruteforce prog init_regs rev_expected_list prefixe =
  let n = List.length rev_expected_list in
  let curr_n = List.length prefixe in
  if n = curr_n then test 0 prefixe
  else
    List.filter_map
      (fun x ->
        let full_rega_list =
          prefixe @ [ x ] @ List.init (n - curr_n - 1) (fun _ -> 0)
        in
        execution prog ({ init_regs with a = test 0 full_rega_list }, 0, [])
        |> fun (_, _, stdout) ->
        if same_prefixe stdout rev_expected_list (curr_n + 1) then
          Some (prefixe @ [ x ])
        else None)
      (List.init 8 (fun x -> x))
    |> List.fold_left
         (fun acc pref ->
           min (bruteforce prog init_regs rev_expected_list pref) acc)
         max_int

let day17_part2 input =
  parse_string ~consume:Prefix parse_full input |> function
  | Ok (regs, prog) ->
      let programme =
        prog_of_list (List.map (fun (a, b) -> instruction_of_ints a b) prog)
      in
      let rev_exp_l =
        List.fold_left (fun acc (i, j) -> [ j; i ] @ acc) [] prog
      in
      bruteforce programme regs rev_exp_l [] |> Printf.printf "result=%d\n"
  | Error x -> failwith ("parse_error=" ^ x)

let () = day17_part1 test_input
let () = day17_part1 (readfile "input17.txt")
let () = day17_part2 test_input2
let () = day17_part2 (readfile "input17.txt")
