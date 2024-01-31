(** This file defines the different costs used in the diffing algorithm.
    
    In particular, it defines the following constant costs:
    * The insertion cost `ci`.
    * The deletion cost `cd`.
    * The copy cost `cc`.
    * The glue cost `cg`.
    * The move cost `cm`.

    It also defines the function that is used to compute the update cost. *)

(** The type of the costs. *)
type t

(** The default value that should take an edge if no cost has been computed. *)
val null : t

(** The cost of the insertion of a node in the AST. *)
val ci : t

(** The cost of the deletion of a node in the AST. *)
val cd : t

(** The cost of the copy of a subtree in the AST. *)
val cc : t

(** The cost of the glue of a subtree in the AST. *)
val cg : t

(** The cost of the movement of a subtree in the AST. *)
val cm : t

(* The costs of insertion / deletion for the lower-bound and upper-bound computations. *)
val lb_ci : unit -> t
val lb_cd : unit -> t
val ub_ci : unit -> t
val ub_cd : unit -> t

(** Returns a function that computes the conditional move of a given edge using a
    function that computes the forced move of this very edge. *)
val f_conditional_move : ('a -> 'a -> t) -> ('a -> 'a -> t)
  
(** Computes the upper-bound cost based on functions that give:
    (i) The degree of the considered element.
    (ii) The children in the input tree of the considered element.
    (iii) The cost of a conditional move for an edge (obtained using f_conditional_move). 
 *)
val upper_bound : ('a -> int) -> 'a list -> 'a list -> ('a -> 'a -> t) -> ('a -> 'a -> t)
                  -> t -> 'a -> 'a -> t

(** Computes the lower-buond cost based on functions that give the forced move cost of 
    edges. *)
val lower_bound : ('a -> t) -> 'a list -> ('a -> t) -> 'a list -> t -> t

(** Returns true when the lower-bound (first argument) can be pruned by the values of the
    upper-bounds (second and third arguments). *)
val prune_rule_1 : t -> t -> t -> bool

(** Returns true when the lower-bound is greater than an insertion and a deletion. *)
val prune_rule_2 : t -> bool

(** Compares two costs. *)
val compare : t -> t -> int

val int_to_cost : int -> t
(* The update cost is given by the data-structure. *)
