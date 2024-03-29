type t = int

let null = 0

(* /!\ ARBITRARY VALUES /!\ *)
(* They'll need to be determined by testing the algorithm. *)
let max = 21474836471
let ci = 1
let cd = 1
let cc = 2
let cg = 2
let cm = 1
let ct = Stdlib.max cm (Stdlib.max cc cg)

let lb_ci () = 2*ci
let lb_cd () = 2*cd
let ub_ci () = 2*ci
let ub_cd () = 2*cd
let prune_rule_3 () = 2*(ci + cd)

let f_conditional_move (f: 'a -> 'a -> t) : ('a -> 'a -> t) =
  fun x y -> cm - (f x y)

let upper_bound (out: 'a -> int) (children1: 'a list) (children2: 'a list) (cond_moves1: 'a -> 'a -> t)
      (cond_moves2: 'a -> 'a -> t) (cw: t) (m: 'a) (n: 'a) : t =
  let left = List.fold_left (fun s m' -> s + cc * (Stdlib.max ((out m') - 1) 0) + (cond_moves1 m' n)) 0 children1 in
  let right = List.fold_left (fun s n' -> s + cg * (Stdlib.max ((out n') - 1) 0) + (cond_moves2 m n')) 0 children2 in
  2*cw + left + right

let prune_rule_1 (lb: t) (ub1: t) (ub2: t) : bool =
  lb >= ub1 + ub2 + 4*(ct)

let prune_rule_2 (lb: t) : bool =
  lb >= 2*(cd + ci)

(* Below are some edge cases that do not appear in the MH-DIFF paper:
   * If one of the node has no children, then take the cost of the forced moves of the other.
   * Otherwise, take the minimum.
   It makes sense as we want an additionnal weigth coming with the link between a node with children
   and a node without children. *)
let lower_bound (forced1: 'a -> t) (l1: 'a list) (forced2: 'a -> t) (l2: 'a list) (cu: t)
    : t =
  let ca = List.fold_left (fun s m' -> s + (forced1 m')) 0 l1 in
  let cb = List.fold_left (fun s n' -> s + (forced2 n')) 0 l2 in
  let co =
    if (List.length l1) = 0 then cb
    else if (List.length l2) = 0 then ca
    else (Stdlib.min ca cb) in
  2*cu + co

let compare (x: t) (y: t) : int =
  Stdlib.compare x y

let of_int (v: int) : t = v

let to_int (v: t) : int = v

let sub (x: t) (y: t) : int = x - y

let add (x: t) (y: t) : int = x + y