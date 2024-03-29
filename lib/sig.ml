(* We need to duplicate otherwise it doesn't compile. *)
module type INPUT = sig
  type i
  type t
  type v
  type node

  val create : i -> t
  val parent : t -> node -> node option 
  val children : t -> node -> node list 
  val elements : t -> node list
  val compare : node -> node -> Cost.t

  val label : node -> string
  val value : node -> v
  val root  : t -> node
end

module Edge = struct
  type t = Cost.t
  let compare = Stdlib.compare
  let equal = (=)
  let default = Cost.null
end

module type Node = sig 
  module Input : INPUT

  type t = Original of Input.node | Minus | Plus | Dummy of t 
  val mk : Input.node -> t
  val minus : unit -> t
  val plus : unit -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
end

module type G = Graph.Sig.I

type 'a patch = ('a Label.t) list
