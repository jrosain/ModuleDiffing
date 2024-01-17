type t =
  | Ins
  | Del
  | Sub
  | Nil
  | Mov
  | Cpy
  | Comb of t * update
and update =
  | Upd


val default : t