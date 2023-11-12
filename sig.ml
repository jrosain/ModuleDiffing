module Edge = struct
  type t = Label.t * Cost.t
  let compare = Stdlib.compare
  let equal = (=)
  let default = Label.default, Cost.default
end

module type G = Graph.Sig.I

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
  
end


module type Node = sig 
  module Input : INPUT
  type t = Original of Input.v | Minus | Plus 
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val equal : t -> t -> bool
end

