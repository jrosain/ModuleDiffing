(** This file exposes the main function(s) that are used to make the diffing between two modules. *)

module Make(I: Sig.INPUT) : sig
  module G : Sig.G

  val exec : I.t -> I.t -> (I.node Sig.patch)

  val display : (I.node Sig.patch) -> unit
end
