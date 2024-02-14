module Z3_monoid : Monoid.Monoid = struct
  type t = int
  let neutral = 0 
  let product = (+)
end
;;
module Z3_group_make (M : Monoid.Monoid) = 
  struct
    type t = M.t
    let inverse = fun x -> -x
  end
;;

module Z3_groupe = Z3_group_make (Z3_monoid);;