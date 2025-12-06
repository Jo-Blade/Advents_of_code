type t = int * int

let compare (i0, j0) (i1, j1) =
  match Int.compare i0 i1 with 0 -> Int.compare j0 j1 | n -> n

let cross ((i, j) : t) : t list =
  [ (i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1) ]

let diag ((i, j) : t) : t list =
  [ (i - 1, j - 1); (i + 1, j + 1); (i + 1, j - 1); (i - 1, j + 1) ]

(** Return the list of all positions in the square delimited by (i0,j0) and (i1,
    j1) *)
let square (i0, j0) (i1, j1) =
  List.concat
  @@ List.init (i1 - i0) (fun i ->
         List.init (j1 - j0) (fun j -> (i + i0, j + j0)))
