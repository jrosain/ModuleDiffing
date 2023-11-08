type value =
    Int of int
  | String of string
  | Dict of dict

and dict = (string, value) Hashtbl.t

(* Compute equality between two values because == on Hashtbl is meh *)
let rec is_value_equal (v1: value) (v2: value) : bool =
  match (v1, v2) with
  | Int i1, Int i2 -> i1 = i2
  | String s1, String s2 -> s1 = s2
  | Dict d1, Dict d2 ->
    if Hashtbl.length d1 <> Hashtbl.length d2 then
      false
    else
      Hashtbl.fold (fun key v1' acc ->
        if (not acc) then
          false
        else
        match (Hashtbl.find_opt d2 key) with
        | None -> false
        | Some v2' -> is_value_equal v1' v2'
      ) d1 true
  | _, _ -> false

let is_dict_equal (d1: dict) (d2: dict) : bool =
  is_value_equal (Dict d1) (Dict d2)