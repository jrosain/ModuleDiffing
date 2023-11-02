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
