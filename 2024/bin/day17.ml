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

type registers = { a : int; b : int; c : int; spy_reg : int * int * int }

module type ComputerChannel = sig
  type t
end

module Computer =
functor
  (CC : ComputerChannel)
  ->
  struct
    type computer_channel = CC.t

    type computer_state = registers * int * computer_channel
    (** registre * instruction_pointer * stdout (list of outputs) *)

    type 'err full_instr = computer_state -> (computer_state, 'err) result
    type 'err programme = int -> 'err full_instr option

    let prog_of_list l : 'a programme =
      let tab = Array.of_list l in
      fun n -> if n < Array.length tab then Some tab.(n) else None

    let rec execution (prog : 'a programme)
        ((_, ip, _) as curr_state : computer_state) :
        (computer_state, 'a) result =
      match prog ip with
      | None -> Ok curr_state
      | Some inst -> (
          match inst curr_state with
          | Ok new_state -> execution prog new_state
          | x -> x)
  end

let ip_inc (regs, ip, stdout) = (regs, ip + 1, stdout)

let adv op (({ a = n; _ } as regs), ip, stdout) =
  Ok (({ regs with a = n / (1 lsl op regs) }, ip, stdout) |> ip_inc)

let bxl op (({ b = n; _ } as regs), ip, stdout) =
  Ok (({ regs with b = n lxor op regs }, ip, stdout) |> ip_inc)

let bst op (regs, ip, stdout) =
  Ok (({ regs with b = op regs mod 8 }, ip, stdout) |> ip_inc)

let jnz op (({ a = n; _ } as regs), ip, stdout) =
  Ok (regs, (if n = 0 then ip + 1 else op regs), stdout)

let bxc _ (({ b = nb; c = nc; _ } as regs), ip, stdout) =
  Ok (({ regs with b = nb lxor nc }, ip, stdout) |> ip_inc)

module ComputerChannel1 = struct
  type t = string list

  let empty = []
  let append chan n = Ok (string_of_int n :: chan)
  let to_string chan = String.concat "," (List.rev chan)
end

let out op (regs, ip, stdout) =
  let ( >> ) = ComputerChannel1.append in
  stdout >> op regs mod 8 |> function
  | Ok stdout -> Ok ((regs, ip, stdout) |> ip_inc)
  | Error x -> Error x

let bdv op (({ a = n; _ } as regs), ip, stdout) =
  Ok (({ regs with b = n / (1 lsl op regs) }, ip, stdout) |> ip_inc)

let cdv op (({ a = n; _ } as regs), ip, stdout) =
  Ok (({ regs with c = n / (1 lsl op regs) }, ip, stdout) |> ip_inc)

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

let regs_parse =
  lift3
    (fun a b c -> { a; b; c; spy_reg = (0, 0, 0) })
    reg_parse reg_parse reg_parse

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

module Computer1 = struct
  open Computer (ComputerChannel1)

  let day17_part1 input =
    parse_string ~consume:Prefix parse_full input |> function
    | Ok (regs, prog) -> (
        execution
          (prog_of_list (List.map (fun (a, b) -> instruction_of_ints a b) prog))
          (regs, 0, ComputerChannel1.empty)
        |> function
        | Ok (_, _, stdout) ->
            Printf.printf "result=%s\n" (ComputerChannel1.to_string stdout)
        | _ -> failwith "program exception")
    | Error x -> failwith ("parse_error=" ^ x)
end

let () = Computer1.day17_part1 test_input

let () =
  Computer1.day17_part1 (readfile "input17.txt");
  flush stdout

(** the spy_reg should contain the sum of all combo operands used
    when doing an (a|b|c)dv instruction.
    current function is a wrapper to help this purpose *)
let incr_spy_rega op (({ spy_reg = a, b, c; _ } as regs), ip, stdout) =
  ({ regs with spy_reg = (a + op regs, b, c) }, ip, stdout)

let incr_spy_regb op (({ spy_reg = a, b, c; _ } as regs), ip, stdout) =
  ({ regs with spy_reg = (a, b + op regs, c) }, ip, stdout)

let incr_spy_regc op (({ spy_reg = a, b, c; _ } as regs), ip, stdout) =
  ({ regs with spy_reg = (a, b, c + op regs) }, ip, stdout)

module ComputerChannel2 = struct
  type t = int list

  let append (chan : t) (n : int) =
    match chan with [] -> Error 1 | h :: t -> if h = n then Ok t else Error 1
end

let out op (regs, ip, stdout) =
  let ( >> ) = ComputerChannel2.append in
  stdout >> op regs mod 8 |> function
  | Ok stdout -> Ok ((regs, ip, stdout) |> ip_inc)
  | Error _ -> Error (regs, ip, stdout)

let instruction_of_ints_part2 opcode operand =
  match opcode with
  | 0 ->
      fun a ->
        incr_spy_rega (combo_operand_of_int operand) a
        |> adv (combo_operand_of_int operand)
  | 1 -> bxl (literal_operand_of_int operand)
  | 2 -> bst (combo_operand_of_int operand)
  | 3 -> jnz (literal_operand_of_int operand)
  | 4 -> bxc (literal_operand_of_int operand)
  | 5 -> out (combo_operand_of_int operand)
  | 6 ->
      fun a ->
        incr_spy_regb (combo_operand_of_int operand) a
        |> bdv (combo_operand_of_int operand)
  | 7 ->
      fun a ->
        incr_spy_regc (combo_operand_of_int operand) a
        |> cdv (combo_operand_of_int operand)
  | _ -> failwith "invalid opcode"

module Computer2 = struct
  open Computer (ComputerChannel2)

  let rec find_reg_a prog ((({ a = n; _ } as regs), ip, stdout) as state) =
    match execution prog state with
    | Ok (_, _, []) -> n
    | Ok ({ spy_reg = a, b, c; _ }, _, _)
    | Error ({ spy_reg = a, b, c; _ }, _, _) ->
        let x = max a (max b c) in
        if n < 119000 then Printf.printf "debug:%d + %d\n" n x;
        find_reg_a prog ({ regs with a = n + (1 lsl max 0 (x - 3)) }, ip, stdout)

  let day17_part2 input =
    parse_string ~consume:Prefix parse_full input |> function
    | Ok (regs, prog) ->
        find_reg_a
          (prog_of_list
             (List.map (fun (a, b) -> instruction_of_ints_part2 a b) prog))
          ( { regs with a = 0 },
            0,
            List.map (fun (a, b) -> [ a; b ]) prog |> List.flatten )
        |> Printf.printf "result=%d\n"
    | Error x -> failwith ("parse_error=" ^ x)
end

let () =
  Computer2.day17_part2 test_input2;
  flush stdout

let () = Computer2.day17_part2 (readfile "input17.txt")
