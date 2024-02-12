(** Returns a patch which, if applied to d1, gives d2

    Multiple case
     1. the key is in d1 but not in d2, we need to remove it from d1
     2. the key is in d1 and d2, we check the type
        a. if values are the same, nothing happend
        b. if values are not the same and not both of them are dico,
           we need to change the value of d1 to the value of d2
        c. if values are not the same and both are dico, we do a patch recursion
     3. the key is in d1 and not in d1, we need to add it to d1
    
     For point 1 and 2 we just iterate over all values of d1
     For point 3, we just have to remove good keys (keys in d1 and d2) while itering over d1
     we then only have to iterate over all remaining values in d2
*)
let rec diff (d1 : Dict.dict) (d2 : Dict.dict) : Patch.patch =
  let patch = Hashtbl.create 1 in
  let d1_copy = Hashtbl.copy d1 in 
  let d2_copy = Hashtbl.copy d2 in 
  Hashtbl.iter (fun key v1 ->
    match (Hashtbl.find_opt d2_copy key) with
    (* 1. *)
    | None    -> Hashtbl.add patch key Patch.Del
    | Some v2 ->
      (* Removing for point 3 afterwards *)
      Hashtbl.remove d2_copy key;
      (* 2.a is the else case (nothing happend) *)
      if not (Dict.is_value_equal v1 v2) then
        match (v1, v2) with
        (* 2.b *)
        | Dict.Dict d1', Dict.Dict d2' ->
          let subpatch = diff d1' d2' in
          Hashtbl.add patch key (Patch.Dict subpatch)
        (* 2.c *)
        | _, _ -> Hashtbl.add patch key (Patch.Change v2) 
  ) d1_copy;
  Hashtbl.iter (fun key v ->
    (* 3. *)
    Hashtbl.add patch key (Patch.Add v)
  ) d2_copy;
  patch