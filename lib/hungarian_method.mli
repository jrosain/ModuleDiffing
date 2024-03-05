(** This file exposes the main function for the hungarian mathod. *)
module Make(G: Sig.G with type E.label = Cost.t) : sig
  type covering = (G.vertex, G.vertex) Hashtbl.t

  val make_graph_balanced_complete : G.t -> (int -> G.vertex) -> G.t
  val make : G.t -> covering

  (* -*-*-*-*-*-PRIVATE FUNCTIONS: DO NOT CALL-*-*-*-*-*-PRIVATE FUNCTIONS: DO NOT CALL-*-*-*-*-**-*)
  module VertexSet : sig
    type t
    type elt = G.V.t

    val find_opt : elt -> t -> elt option
    val is_empty : t -> bool
  end
  
  type bip = (G.vertex list)
  type dual_vars = (G.vertex, Cost.t) Hashtbl.t
  type primal_vars = ((G.vertex * G.vertex), int) Hashtbl.t
  type mates = (G.vertex, G.vertex option) Hashtbl.t
  type bip_node = S (* | T *) | V of G.vertex
  type labels = (G.vertex, bip_node option) Hashtbl.t
  type l_set = VertexSet.t ref
  type path_costs = (G.vertex, Cost.t) Hashtbl.t
  type path_preds = (G.vertex, G.vertex option) Hashtbl.t
  type path = (bip_node option) ref

  val get_bipartite : G.t -> (bip * bip)
  val dual_init : G.t -> bip -> bip -> (dual_vars * dual_vars)
  val primal_init : G.t -> bip -> bip -> dual_vars -> dual_vars -> (primal_vars * mates * int)
  val path_initialization : G.t -> bip -> bip -> mates -> dual_vars -> dual_vars -> (l_set * labels * path_costs * path_preds)
  val label_propagation : G.t -> bip -> bip -> mates -> dual_vars -> dual_vars -> l_set -> labels -> path_costs -> path_preds -> path -> unit
  val dual_iteration : bip -> bip -> dual_vars -> dual_vars -> l_set -> labels -> path_costs -> path_preds -> unit
  val primal_iteration : mates -> labels -> primal_vars -> path -> int -> int
end