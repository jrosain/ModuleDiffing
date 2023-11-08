(** This file exposes the main function for the pruning of edges in the induced graph. *)
module Make(I: Sig.INPUT)(G: Sig.G) : sig
  val prune : I.t -> I.t -> G.t -> G.t
end
