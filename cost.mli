(** This file defines the different costs used in the diffing algorithm.
    
    In particular, it defines the following constant costs:
    * The insertion cost `ci`.
    * The deletion cost `cd`.
    * The copy cost `cc`.
    * The glue cost `cg`.
    * The move cost `cm`.

    It also defines the function that is used to compute the update cost. *)

(** The type of the costs. *)
type t = int

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

(** The maximum between cm, cc and cg. *)
val ct : t

(** Compares two costs. *)
val compare : t -> t -> int

val int_to_cost : int -> t
(* The update cost is given by the data-structure. *)
