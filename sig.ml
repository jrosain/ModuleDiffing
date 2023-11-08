(* TODO: do we define a generic type for Node.t? *)
module Node = struct
  type t = int
  let compare = Stdlib.compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module Edge = struct
  type t = Cost.t
  let compare = Stdlib.compare
  let equal = (=)
  let default = Cost.default
end

module G = Graph.Imperative.Graph.ConcreteLabeled(Node)(Edge)

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
