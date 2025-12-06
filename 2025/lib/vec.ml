type vec2 = int * int

let cross ((i, j) : vec2) : vec2 list =
  [ (i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1) ]

let diag ((i, j) : vec2) : vec2 list =
  [ (i - 1, j - 1); (i + 1, j + 1); (i + 1, j - 1); (i - 1, j + 1) ]
