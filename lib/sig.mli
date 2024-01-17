(** This file defines the signatures of the different modules that are used in the module diffing 
    algorithm. 

    It defines:
    * The type of edges that works with costs.
    * The module type that any (tree) structure given to the algorithm should at least implement.
*)

module Edge : sig
  type t = Label.t * Cost.t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val default : t
end

module type G = Graph.Sig.I 





module type INPUT = sig

  (** The type of the input data-structure. *)
  type i 


  (** The type of the object manipulated. *)
  type t 

  (** The type of the internal representation of an element. 
             It should be comparable using Stdlib.compare, hashable by Hashtbl.hash and
             compared with (=). *)
  type v 

  val create : i -> t


  (** Returns the parent of the given element in the original
                                      data-structure. *)
  val parent : t -> v -> v option
  
  (** Returns the children of the given element in the original
                                      data-structure. *)
  val children : t -> v -> v list 

  (** Returns a list with all the elements. *)
  val elements : t -> v list 

  (** Returns the cost that is needed to update the first
                                          element as the second one (should be symmetric)*)
  val compare : t -> v -> v -> Cost.t 
end



module type Node = sig 
  module Input : INPUT
  type t = Original of Input.v | Minus | Plus 
  val compare : t -> t -> int
  (* val equal : t -> t -> bool *)
  val hash : t -> int
  val equal : t -> t -> bool
end

