
module Make(I: Sig.INPUT) = struct
  type patch = Empty

  let exec (t1: I.t) (t2: I.t) : patch =
    Empty
end
