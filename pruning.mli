(** This file exposes the main function for the pruning of edges in the induced graph. *)
module Make(I: Sig.INPUT)(N: Sig.Node with module Input = I)(G: Sig.G with type V.t = N.t) : sig
  val prune : I.t -> I.t -> G.t -> G.t
end
