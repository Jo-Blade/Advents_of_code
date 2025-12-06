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
        (* as intervals don't overlap, go right *)
        mem x b

(** union x inter_tree appends the contiguous_interval "x" in the "inter_tree"
    red-black tree *)
let rec union i = function
  | Empty -> Black (i, Leaf, Leaf)
  | Leaf -> Red (i, Leaf, Leaf)
  | Red (i', a, b) ->
      balance
      @@
      let a, b = update_children i i' a b in
      Red (i', a, b)
  | Black (i', a, b) ->
      balance
      @@
      let a, b = update_children i i' a b in
      Black (i', a, b)

and balance =
  (* Helper function to balance a red-black tree *)
  function
  | Black (z, Red (y, Red (x, a, b), c), d)
  | Black (z, Red (x, a, Red (y, b, c)), d)
  | Black (x, a, Red (z, Red (y, b, c), d))
  | Black (x, a, Red (y, b, Red (z, c, d))) ->
      Red (y, Black (x, a, b), Black (z, c, d))
  | x -> x

and update_children (mi, ma) (mi', ma') a b =
  (* Helper function to compute the update children of a node *)
  ( (if mi < mi' then
       (* cut upper part of the interval to remove overlap *)
       let ma = min ma (mi' - 1) in
       if mi <= ma then union (mi, ma) a else a
     else a),
    (* cut lower part of the interval to remove overlap *)
    if ma' < ma then
      let mi = max mi (ma' + 1) in
      if mi <= ma then union (mi, ma) b else b
    else b )

(** Convert the interval_tree into a sorted seq of non overlapping intervals *)
let rec seq_of_tree = function
  | Leaf | Empty -> Seq.empty
  | Red (i, a, b) | Black (i, a, b) ->
      Seq.append (seq_of_tree a) @@ Seq.cons i (seq_of_tree b)

(** Length of a contiguous_interval *)
let length_contiguous (mi, ma) = ma - mi + 1

(** Number of element inside an interval_tree *)
let length inter_tree =
  Seq.map length_contiguous @@ seq_of_tree inter_tree |> Seq.fold_left ( + ) 0
