module Make(I: Sig.INPUT) : sig
  val diffing : I.t -> I.t -> (I.node Sig.patch)
end
