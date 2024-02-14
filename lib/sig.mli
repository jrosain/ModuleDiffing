(** This file defines the signatures of the different modules that are used in the module diffing 
    algorithm. 

    It defines:
    * The type of edges that works with costs.
    * The module type that any (tree) structure given to the algorithm should at least implement.
*)

module type INPUT = sig
  type i (* The type of the input data-structure. *)
  type t (* The type of the object manipulated. *)
  type v (* The type of the internal representation of an element (in i). 
            It should be comparable using Stdlib.compare, hashable by Hashtbl.hash and
            compared with (=). *)
  type node (* The internal type of an element. *)

  val create : i -> t
  val parent : t -> node -> node option (* Returns the parent of the given element in the original
                                           data-structure. *)
  val children : t -> node -> node list (* Returns the children of the given element in the original
                                           data-structure. *)
  val elements : t -> node list (* Returns a list with all the elements. *)
  val compare : node -> node -> Cost.t (* Returns the cost that is needed to update the first
                                          element as the second one (should be symmetric)*)

  val label : node -> string
  val value : node -> v
end

module Edge : sig
  type t = Cost.t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val default : t
end

module type Node = sig 
  module Input : INPUT

  type t = Original of Input.node | Minus | Plus 
  val mk : Input.node -> t
  val minus : unit -> t
  val plus : unit -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end

module type G = Graph.Sig.I
