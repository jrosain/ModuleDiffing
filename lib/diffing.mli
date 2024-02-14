(** This file exposes the main function(s) that are used to make the diffing between two modules. *)

module Make(I: Sig.INPUT) : sig
  type patch
  type algo = Dict | Mh_diff

  module G : Sig.G

  val exec : ?algo:algo -> I.t -> I.t -> patch

  val display : patch -> unit
end
