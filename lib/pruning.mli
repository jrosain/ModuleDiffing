(* -*-*-*-*-*-PRIVATE MODULES: DO NOT CALL-*-*-*-*-*-PRIVATE MODULES: DO NOT CALL-*-*-*-*-*-*-*-*)
module CostTable : sig
  type 'a t
  val create : int -> 'a t
  val add : 'a t -> 'a -> 'a -> Cost.t -> unit
  val get : 'a t -> 'a -> 'a -> Cost.t
end

(** This file exposes the main function for the pruning of edges in the induced graph. *)
module Make(I: Sig.INPUT)(N: Sig.Node with module Input = I)(G: Sig.G with type V.t = N.t
                                                                       and type E.label = Cost.t)
       : sig
  val prune : I.t -> I.t -> G.t -> G.t

  (* -*-*-*-*-*-PRIVATE FUNCTIONS: DO NOT CALL-*-*-*-*-*-PRIVATE FUNCTIONS: DO NOT CALL-*-*-*-*-**-*)
  val forced_move_left : I.t -> G.t -> I.v -> I.v -> Cost.t
  val forced_move_right : I.t -> G.t -> I.v -> I.v -> Cost.t
  val lower_bound : G.V.t CostTable.t -> G.t -> I.t -> I.t -> G.V.t -> G.V.t -> Cost.t
  val compute_update_costs : I.t -> I.t -> int -> G.V.t CostTable.t 
  val compute_upper_bound : G.V.t -> G.V.t -> G.V.t CostTable.t -> I.t -> I.t -> G.t -> Cost.t
end
