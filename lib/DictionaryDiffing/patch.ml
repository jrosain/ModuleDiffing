exception ApplyPatchError

(** A modification is one change to do on a patch
    It can be a deletion, in this case we don't need anything
    It can be an addition, in this case we need to know the type to add
    It can be a change, in this case we also need the type to change *)
type modif =
    Add of Dict.value
  | Del
  | Change of Dict.value
  | Dict of patch

(** A patch is a list of modification to give to a dictionary
    It is created as a dictionary keys and nested keys are the ones to modify in the dictionary *)
and patch = (string, modif) Hashtbl.t


(** Returns a dictionnary changed according to the given patch
    
    This function can throw a ApplyPatchError exception
    if there is an error during the patch process *)
let apply_patch (d: Dict.dict) (p: patch) : Dict.dict =
  let new_d = Hashtbl.copy d in
  let rec apply_patch_rec (d: Dict.dict) (p: patch) =
    Hashtbl.iter (fun label modif ->
      match modif with
      | Del       -> Hashtbl.remove d label
      | Add d'    -> Hashtbl.add d label d'
      | Change d' -> Hashtbl.replace d label d'

      (* This is the only non trivial case.
         When the change is a nested patch (represented as a dictionary),
         we need to check if the key exists in the dico we want to patch,
         if it is not or if it does not correspond to a nested dictionary,
         the patch is not correct. *)
      | Dict p    ->
          try 
            let sub_d = (Hashtbl.find d label) in
            match sub_d with
            | Dict.Dict d' -> apply_patch_rec d' p
            | _ ->
              Printf.printf "The label %s has been given in the patch but is not a sub dictionary of the dictionary\n" label;
              raise ApplyPatchError
          with Not_found ->
            Printf.printf "The label %s has been given in the patch but is not on the dictionary\n" label;
            raise ApplyPatchError
    ) p
  in
  apply_patch_rec new_d p;
  new_d
