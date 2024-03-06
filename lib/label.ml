type 'a t =
  | Ins of 'a
  | Del of 'a
  | Nil
  | Mov of 'a * 'a
  | Cpy of 'a * 'a
  | Comb of 'a t * 'a t
  | Upd of 'a * 'a

let default = Nil
