(** This file exposes the main function for the pruning of edges in the induced graph. *)
module Make(I: Sig.INPUT)(N: Sig.Node with module Input = I)(G: Sig.G with type V.t = N.t
                                                                       and type E.label = Cost.t)
       : sig
  val prune : I.t -> I.t -> G.t -> G.t

  (* -*-*-*-*-*-PRIVATE FUNCTIONS: DO NOT CALL-*-*-*-*-*-PRIVATE FUNCTIONS: DO NOT CALL-*-*-*-*-**-*)
  val forced_move_left : I.t -> G.t -> I.v -> I.v -> Cost.t
end
