let rec display_tab (t : int) =
  match t with
  | 0 -> Printf.printf ""
  | _ -> Printf.printf "\t"; display_tab (t - 1)


(** Display a dict to be user-readable in the console *)
let display_dict (d : Dict.dict) : unit =
  let rec display_dict_rec (d : Dict.dict) (t : int) =
    Printf.printf "{\n";
    Hashtbl.iter (fun label value ->
      display_tab (t + 1);
      Printf.printf "%s : " label;

      begin match value with
      | Dict.Int i     -> Printf.printf "%d" i
      | Dict.String s  -> Printf.printf "%s" s
      | Dict.Dict d    -> display_dict_rec d (t + 1)
      end;

      Printf.printf ",\n") d;
    display_tab t;
    Printf.printf "}"
  in
  display_dict_rec d 0


(** Display a patch to be user-readable in the console *)
let display_patch (p : Patch.patch) : unit =
  let rec display_patch_rec (p : Patch.patch) (t : int) =
    Printf.printf "{\n";
    Hashtbl.iter (fun label modif ->
      display_tab (t + 1);
      Printf.printf "%s : " label;

      begin match modif with
      | Patch.Del       -> Printf.printf "deletion"
      | Patch.Add d     -> Printf.printf "addition"
      | Patch.Change d  -> Printf.printf "change"
      | Patch.Dict p    -> display_patch_rec p (t + 1)
      end;

      Printf.printf ",\n") p;
    display_tab t;
    Printf.printf "}"
  in
  display_patch_rec p 0