type 'a naive_grid = int * int -> 'a
(** Simple type to represent a grid, the type does not define the grid container
    so we can't implement very performants algorithm to edit a naive_grid
    content *)

(** naive_set pos v grid returns a new naive_grid where the value at position
    pos equals to v This algorithm is naive because each call to naive_set
    increases the complexity of the new generated naive_grid function *)
let naive_set ((i, j) : int * int) (new_val : 'a) (grid : 'a naive_grid) :
    'a naive_grid =
 fun ((i', j') as pos) -> if i' = i && j' = j then new_val else grid pos

module Vec2Map = Map.Make (Vec2)
(** Create a Vec2Map module to make grid that use a map as container *)

type 'a map_grid = 'a naive_grid * 'a Vec2Map.t
(** More efficient grid implementation that uses a map to save grid content
    modifications. The naive_grid serves the default content of this grid *)

(** Create a new map_grid from a naive_grid *)
let init_mapgrid (default : 'a naive_grid) : 'a map_grid =
  (default, Vec2Map.empty)

(** Convert a mapgrid back to a naive_grid *)
let naive_of_mapgrid ((default, overrides) : 'a map_grid) : 'a naive_grid =
 fun pos ->
  match Vec2Map.find_opt pos overrides with Some x -> x | None -> default pos

(** Efficient set function for map_grid, it creates a new grid where the value
    as position pos equals to v and any other position is left unchanged *)
let set pos new_val ((default, overrides) : 'a map_grid) =
  (default, Vec2Map.add pos new_val overrides)
