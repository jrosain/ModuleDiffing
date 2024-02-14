module type Monoid =
  sig 
    type t
    val neutral : t
    val product : t -> t -> t
  end
;;