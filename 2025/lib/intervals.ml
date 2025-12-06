type contiguous_interval = int * int
(** A contiguous interval is defined by its min and max values (inclusives) *)

type interval_tree =
  | Red of contiguous_interval * interval_tree * interval_tree
  | Black of contiguous_interval * interval_tree * interval_tree
  | Leaf
  | Empty  (** Red-Black tree to represent interval unions *)
(* we must use empty instead of leaf when there is no content because the first union MUST choose black color *)

(** An empty interval_tree *)
let empty = Empty

(** mem x inter_tree test if x belongs to any contiguous_interval in inter_tree
*)
let rec mem x = function
  | Empty | Leaf -> false
  | Red ((mi, ma), a, b) | Black ((mi, ma), a, b) ->
      (* nodes are sorted by their lower bound *)
      if x < mi then (* we must go left in the tree *)
        mem x a
      else if x <= ma then true
      else
        (* as intervals can overlap, we must continue search in both directions *)
        mem x b || mem x a

(** Helper function to balance a red-black tree *)
let balance = function
  | Black (z, Red (y, Red (x, a, b), c), d)
  | Black (z, Red (x, a, Red (y, b, c)), d)
  | Black (x, a, Red (z, Red (y, b, c), d))
  | Black (x, a, Red (y, b, Red (z, c, d))) ->
      Red (y, Black (x, a, b), Black (z, c, d))
  | x -> x

(** union x inter_tree appends the contiguous_interval "x" in the "inter_tree"
    red-black tree *)
let rec union ((x, _) as inter) = function
  | Empty -> Black (inter, Leaf, Leaf)
  | Leaf -> Red (inter, Leaf, Leaf)
  | Red (((mi, _) as i), a, b) ->
      balance
      @@ if x < mi then Red (i, union inter a, b) else Red (i, a, union inter b)
  | Black (((mi, _) as i), a, b) ->
      balance
      @@
      if x < mi then Black (i, union inter a, b) else Black (i, a, union inter b)
