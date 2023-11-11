type t = int

let null = 0

(* /!\ ARBITRARY VALUES /!\ *)
(* They'll need to be determined by testing the algorithm. *)
let ci = 12
let cd = 12
let cc = 73
let cg = 73
let cm = 28
let ct = Stdlib.max cm (Stdlib.max cc cg)

let compare (x: t) (y: t) : int =
  Stdlib.compare x y

let int_to_cost (v: int) : t = v
