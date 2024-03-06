module type Node = sig
  type t
  
  (* The copy should not be in the graph (it should be addable) *)
  val mk_copy : t -> t
end

(** This file exposes the main function for the edge covering of a graph. *)
module Make(N : Node)(G: Sig.G with type V.t = N.t with type E.label = Cost.t) : sig
  val bipartite_min_edge_cover : G.t -> G.t

  (* -*-*-*-*-*-PRIVATE FUNCTIONS: DO NOT CALL-*-*-*-*-*-PRIVATE FUNCTIONS: DO NOT CALL-*-*-*-*-**-*)
  val create_edge_cover_graph : G.t -> G.t
end 