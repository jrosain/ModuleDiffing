(** This file exposes the main function for the edge covering of a graph. *)
module Make(I: Sig.INPUT)(N: Sig.Node with module Input = I)(G: Sig.G with type V.label = N.t and type E.label = Cost.t) : sig
    val bipartite_min_edge_cover : G.t -> G.t
end 