(** This file exposes the main function for the edge covering of a graph. *)
module Make(G: Sig.G with type E.label = Cost.t) : sig
  val bipartite_min_edge_cover : G.t -> G.t
end