(** This file exposes the main function(s) that are used to make the diffing between two modules. *)

module Make(I: Sig.INPUT) : sig
  type patch

  module G : Sig.G

  val exec : I.t -> I.t -> patch
end
