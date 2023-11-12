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

let f_conditional_move (f: 'a -> 'a -> t) : ('a -> 'a -> t) =
  fun x y -> let forced_move_cost = f x y in cm - forced_move_cost

let upper_bound (out: 'a -> int) (c1: 'a list) (c2: 'a list) (cond1: 'a -> 'a -> t)
      (cond2: 'a -> 'a -> t) (cw: t) (m: 'a) (n: 'a) : t=
  let left = List.fold_left (fun s m' -> s + cc * ((out m') - 1) + (cond1 m' n)) 0 c1 in
  let right = List.fold_left (fun s n' -> s + cc * ((out n') - 1) + (cond2 n' m)) 0 c2 in
  2*cw + left + right

let prune_rule_1 (lb: t) (ub1: t) (ub2: t) : bool =
  lb >= ub1 + ub2 + 4*(ct)

let prune_rule_2 (lb: t) : bool =
  lb >= 2*(cd + ci)

let lower_bound (forced1: 'a -> t) (l1: 'a list) (forced2: 'a -> t) (l2: 'a list) (cu: t)
    : t =
  let ca = List.fold_left (fun s m' -> s + (forced1 m')) 0 l1 in
  let cb = List.fold_left (fun s n' -> s + (forced2 n')) 0 l2 in
  2*cu + (Stdlib.min ca cb)

let compare (x: t) (y: t) : int =
  Stdlib.compare x y

let int_to_cost (v: int) : t = v
