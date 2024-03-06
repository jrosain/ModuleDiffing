type 'a t =
  | Ins of 'a
  | Del of 'a
  | Nil
  | Mov of 'a * 'a
  | Cpy of 'a * 'a
  | Comb of 'a t * 'a t (* <--- second element of comb should be an update *)
  | Upd of 'a * 'a

val default : 'a t
