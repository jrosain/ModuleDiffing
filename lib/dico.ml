module Make(I: Sig.INPUT) = struct
  type patch = (I.node Sig.patch)

  let get_element (label: string) (elements: I.node list) : I.node option =
    List.find_opt (fun node -> (I.label node) = label) elements

  let is_value_equal (v1: I.node) (v2: I.node) : bool =
    (I.compare v1 v2) = Cost.null

  let mem_element (label: string) (elements: I.node list) : bool =
    List.exists (fun node -> (I.label node) = label) elements

  (** Returns a patch which, if applied to d1, gives d2

      Multiple case
      1. the key is in d1 but not in d2, we need to remove it from d1
      2. the key is in d1 and d2, we check the type
        a. if values are the same, nothing happend
        b. if values are not the same and not both of them are dico,
           we need to change the value of d1 to the value of d2
        c. if values are not the same and both are dico, we do a patch recursion
      3. the key is in d1 and not in d2, we need to add it to d1
      --> /!\ Maybe the insertions should be computed recursively /!\
      
      For point 1 and 2 we just iterate over all values of d1
      For point 3, we just have to remove good keys (keys in d1 and d2) while itering over d1
      we then only have to iterate over all remaining values in d2 *)
  let rec diff (ld1: I.node list) (ld2: I.node list) (d1: I.t) (d2: I.t) : patch =
    let rec traverse (dico: I.node list) (other: I.node list) (acc: patch) : patch =
      match dico with
      | [] -> acc
      | node :: t ->
         let element = get_element (I.label node) (other) in
         match element with
         | None -> traverse t (other) ((Label.Del node) :: acc)
         | Some value ->
            let c1, c2 = I.children d1 node, I.children d2 value in
            match (c1, c2) with
            | _ :: _, _ :: _  ->
               let acc = (diff c1 c2 d1 d2) @ acc in
               if is_value_equal node value then traverse t other acc
               else traverse t (other) ((Label.Upd (node, value)) :: acc)
            | _, _ -> traverse t other acc
    in
    let insertions =
      List.map (fun x -> Label.Ins x)
        (List.filter (fun node -> not (mem_element (I.label node) ld1)) ld2)
    in (traverse ld1 ld2 []) @ insertions

  let diffing (d1: I.t) (d2: I.t) : patch =
    diff [I.root d1] [I.root d2] d1 d2
end
