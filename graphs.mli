(** This file defines the necessary modules to instantiate the type that we use for our graph. 
    
    In essence, it defines:
      * The module for the nodes that are indices (integer).
      * The module for the edges that are labelled by integer (float?) weigth.
      * The different modules that are needed to do the operations of the algorithm on the graph.
*)

module Node : sig
  type t
  
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
end

module Edge : sig
  type t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val default : Cost.t
end

module G : Graph.Sig.I
