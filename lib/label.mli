type t =
  | Ins
  | Del
  | Nil
  | Mov
  | Cpy
  | Comb of t * update
and update =
  | Upd

val default : t
