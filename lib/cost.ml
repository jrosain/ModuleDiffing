type t = int

let null = 0

(* /!\ ARBITRARY VALUES /!\ *)
(* They'll need to be determined by testing the algorithm. *)
let ci = 1
let cd = 1
let cc = 2
let cg = 2
let cm = 1
let ct = Stdlib.max cm (Stdlib.max cc cg)

let f_conditional_move (f: 'a -> 'a -> t) : ('a -> 'a -> t) =
  fun x y -> let forced_move_cost = f x y in cm - forced_move_cost

let upper_bound (out: 'a -> int) (children1: 'a list) (children2: 'a list) (cond_moves1: 'a -> 'a -> t)
      (cond_moves2: 'a -> 'a -> t) (cw: t) (m: 'a) (n: 'a) : t =
  let left = List.fold_left (fun s m' -> s + cc * ((out m') - 1) + (cond_moves1 m' n)) 0 children1 in
  let right = List.fold_left (fun s n' -> s + cc * ((out n') - 1) + (cond_moves2 m n')) 0 children2 in
  2*cw + left + right

let prune_rule_1 (lb: t) (ub1: t) (ub2: t) : bool =
  lb >= ub1 + ub2 + 4*(ct)

let prune_rule_2 (lb: t) : bool =
  lb >= 2*(cd + ci)

let prune_rule_3 (lb: t) (ciocd: t) : bool =
  lb < 2*ciocd

let lower_bound (forced1: 'a -> t) (l1: 'a list) (forced2: 'a -> t) (l2: 'a list) (cu: t)
    : t =
  let ca = List.fold_left (fun s m' -> s + (forced1 m')) 0 l1 in
  let cb = List.fold_left (fun s n' -> s + (forced2 n')) 0 l2 in
  2*cu + (Stdlib.min ca  cb)

let compare (x: t) (y: t) : int =
  Stdlib.compare x y

let int_to_cost (v: int) : t = v
