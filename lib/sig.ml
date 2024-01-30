(* We need to duplicate otherwise it doesn't compile. *)
module type INPUT = sig
  type i
  type t
  type v

  val create : i -> t
  val parent : t -> v -> v option
                          
  val children : t -> v -> v list
                                 
  val elements : t -> v list
  val compare : t -> v -> v -> Cost.t
  val print_v : v -> unit
end

module Edge = struct
  type t = Cost.t
  let compare = Stdlib.compare
  let equal = (=)
  let default = Cost.null
end

module type Node = sig 
  module Input : INPUT

  type t = Original of Input.v | Minus | Plus 
  val mk : Input.v -> t
  val minus : unit -> t
  val plus : unit -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end

module type G = Graph.Sig.I
