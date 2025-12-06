open Aoc25

(** The parser for today's input *)
let p =
  let open Parsers in
  let p_ranges =
    sep_by (char '\n')
    @@ combine2
         (fun mi ma : Intervals.contiguous_interval -> (mi, ma))
         (uint +<- char '-')
         uint
  and p_ids = sep_by (char '\n') uint in
  combine2
    (fun ranges ids ->
      ( Seq.fold_left (fun acc a -> Intervals.union a acc) Intervals.empty ranges,
        ids ))
    (p_ranges +<- ws) p_ids

let count_memberships (inter_tree, xs) =
  Seq.filter (fun x -> Intervals.mem x inter_tree) xs |> Seq.length

let () =
  count_memberships @@ Parsers.run_on_argv1 p
  |> Printf.printf "solution part1 = %d\n"
