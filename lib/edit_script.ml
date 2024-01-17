
module Make(I: Sig.INPUT) (N: Sig.Node with module Input = I) (G: Sig.G with type V.t = N.t and type E.label = Label.t * Cost.t) = struct
  (* -------------------- Conversions -------------------- *)

  (* Convert the input structure to an OCamlgraph *)
  let convert_to_ocamlgraph (t : I.t) : G.t = 
    let transform = List.map (fun x -> N.Original x) in
    let nodes = transform (I.elements t) in
    let g = G.create ~size:(List.length nodes) () in
    List.iter (G.add_vertex g) nodes;
    g

  (* Convert a graph node to its original value *)
  let convert_node (node : N.t) : I.v = 
    match node with
      | N.Original x -> x
      | _ -> failwith "Impossible"

  (* Extract the value from an option, failing if it's None *)
  let node_exn (node : I.v option) : I.v = 
    match node with
      | Some x -> x
      | None -> failwith "Impossible"

  (* Modify the label of a graph edge *)
  let edit_edge_label (graph : G.t) (v1 : N.t) (new_label : Label.t) (v2 : N.t) : unit = 
    let prev_label = G.E.label (G.find_edge graph v1 v2) in
    let prev_cost = snd prev_label in
    let edge = G.E.create v1 (new_label, prev_cost) v2 in
    let _ = G.remove_edge graph v1 v2 in
    G.add_edge_e graph edge

  (* -------------------- Annotation -------------------- *)

  (* Annotate the bipartite graph based on two input structures *)
  let annotate (t1: I.t) (t2 : I.t) (bipartite : G.t) = 
    let t1_ext_graph = (
        let g = convert_to_ocamlgraph t1 in
        G.add_vertex g Plus;
        g
    ) in
    let t2_ext_graph = (
      let g = convert_to_ocamlgraph t2 in
      G.add_vertex g Minus;
      g
    ) in
    let t1_graph = convert_to_ocamlgraph t1 in
    let t2_graph = convert_to_ocamlgraph t2 in

    let _ = G.iter_edges (fun _ v2 -> 
      let m_nodes = G.fold_edges (fun v1' _ acc -> 
        if (G.mem_vertex t1_ext_graph v1') then
          v1'::acc
        else
          acc
      ) bipartite [] in

      let n_nodes = G.fold_edges (fun _ v2' acc -> 
        if (G.mem_vertex t2_ext_graph v2') then
          v2'::acc
        else
          acc
      ) bipartite [] in

      let case21 m l = 
        (match m with 
          | N.Plus -> 
            (* Case when m is Plus (insertion) *)
            let n_l = G.fold_vertex (fun node_t2 acc ->
              if G.V.label node_t2 = G.V.label v2 then
                node_t2::acc
              else
                acc
            ) t2_graph [] in
            if List.length n_l = 1 then
              edit_edge_label bipartite m Ins v2 
            else
              let n_l_partitions = List.map (fun v1 -> 
                List.filter (fun v2 -> 
                  let node1 = I.parent t2 (convert_node v1) |> node_exn in
                  let node2 = I.parent t2 (convert_node v2) |> node_exn in
                  (G.mem_edge bipartite Plus (N.Original node1)) &&
                  (G.mem_edge bipartite Plus (N.Original node2))
                  ) n_l
              ) n_l in
              let _ = List.iter (fun n_l_part ->
                let v1 = List.hd n_l_part in
                let _ = edit_edge_label bipartite m Ins v1 in
                List.iter (fun n -> 
                  edit_edge_label bipartite m Nil n
                  (* Do something for D *)
                ) (List.tl n_l_part)
              ) n_l_partitions in
              ()
          | _ -> 
            (* Case when m is not Plus *)
            let n1 = G.fold_vertex (fun v1 acc ->
              G.fold_vertex (fun v2 acc' ->
                let node1 = I.parent t1 (convert_node v1) |> node_exn in
                let node2 = I.parent t2 (convert_node v2) |> node_exn in
                if G.mem_edge bipartite (N.Original node1) (N.Original node2) then 
                  v2::acc'
                else
                  acc'
              ) t2_graph acc
            ) t1_graph [] in
            let n2 = G.fold_vertex (fun m' acc ->
              G.fold_vertex (fun v2 acc' ->
                let node2 = I.parent t2 (convert_node v2) |> node_exn in
                if G.mem_edge bipartite m' (N.Original node2) then
                  v2::acc'
                else
                  acc'
              ) t2_graph acc
            ) t1_graph [] in    
            let n3 = G.fold_vertex (fun v2 acc ->
              let node2 = I.parent t2 (convert_node v2) |> node_exn in
              if G.mem_edge bipartite Plus (N.Original node2) then
                v2::acc
              else
                acc
            ) t2_graph [] in
            let n1_partitions = List.map (fun v1 -> 
              List.filter (fun v2 -> 
                let node1 = I.parent t2 (convert_node v1) |> node_exn in
                let node2 = I.parent t2 (convert_node v2) |> node_exn in
                node1 = node2) n1
            ) n1 in
            
            (* Iterate over each partition *)
            let _ = List.iter (fun n1_part ->
              (* Choose the first element of the list n1_part *)
              let v1 = List.hd n1_part in
              let _ = edit_edge_label bipartite m Nil v1 in
              (* Iterate over each element of the tail of the list n1_part *)
              List.iter (fun n -> 
                edit_edge_label bipartite m Cpy n
              ) (List.tl n1_part)
            ) n1_partitions in
            
            let n2_partitions = List.map (fun v1 -> 
              let partition_with_v1 = List.filter (fun v2 -> 
                let node1 = I.parent t2 (convert_node v1) |> node_exn in
                let node2 = I.parent t2 (convert_node v2) |> node_exn in
                let lst_nodes_t1 = I.elements t1 in 
                List.exists (fun x -> 
                  (G.mem_edge bipartite (N.Original x) (N.Original node2)) &&
                  (G.mem_edge bipartite (N.Original x) (N.Original node1))
                ) lst_nodes_t1
              ) n2 in
              partition_with_v1 @ [v1]
            ) n2 in

            let n3_partitions = List.map (fun v1 -> 
                  (* Traverse up the t2 tree by applying the I.parent function recursively until None is reached *)

                  let rec aux (node1 : I.v) (node2 : I.v) : bool =
                    match I.parent t1 node1, I.parent t2 node2 with
                      | Some p1, Some p2 ->
                        if (List.exists (fun x -> 
                          (G.mem_edge bipartite (N.Original x) (N.Original p1)) &&
                          (G.mem_edge bipartite (N.Original x) (N.Original p2))
                        ) (I.elements t1)) then
                          true
                        else
                          ((G.mem_edge bipartite Plus (N.Original p1)) &&
                          (G.mem_edge bipartite Plus (N.Original p2))) &&
                          aux p1 p2
                      | _ -> false
                  in

                  let partition_with_v1 = List.filter (fun v2 -> 
                    aux (convert_node v1) (convert_node v2)
                  ) n1 in
                  partition_with_v1 @ [v1]
            ) n3 in

            let p_pm = G.fold_vertex (fun v2 acc ->
              let node1 = I.parent t2 (convert_node m) |> node_exn in
              if G.mem_edge bipartite (N.Original node1) v2 then
                v2::acc
              else
                acc
            ) t2_graph [] in

            (*
              Now we define s, which is p_pm - {p(n) : n \in l}  
            *)
            let s = List.filter (fun v2 -> 
              not (List.exists (fun v1 -> 
                let node1 = I.parent t2 (convert_node v1) |> node_exn in
                (convert_node v2) = node1
              ) l)
            ) p_pm in

            (*
                Iterate over each partition (n1_partitions, n2_partitions)
                Get the set that contains the most elements from the set s
            *)
            let c = snd (List.fold_left (fun acc x -> 
              let nb_elem = List.length (List.filter (fun y -> 
                List.exists (fun z -> 
                  (convert_node y) = (convert_node z)
                ) x
              ) s) in
              if (nb_elem = (fst acc)) then
                nb_elem, x :: (snd acc)
              else if nb_elem > (fst acc) then
                nb_elem, x :: []
              else
                acc
            ) (0, []) (n1_partitions @ n2_partitions)) in
            (*
                Iterate over each partition of the sets n1_partitions, n2_partitions, and n3_partitions.
            *)
            let _ = List.iter (fun x -> 
              (* Take the first element of x *)
              let v1 = List.hd x in 
              (* If x is included in c (c is a set of sets), then apply the Mov rule *)
              let _ = (if List.exists (fun y -> 
                List.for_all (fun z -> 
                  List.exists (fun w -> 
                    (convert_node z) = (convert_node w)
                  ) y
                ) x
              ) c then
                edit_edge_label bipartite m Mov v1
              else
                edit_edge_label bipartite m Cpy v1) in 

              let _ = List.iter (fun y -> 
                edit_edge_label bipartite m Nil y
                (* Do something for D *)
              ) (List.tl x) in
              ()
            ) (n1_partitions @ n2_partitions @ n3_partitions) in
            ())
      in 

      match m_nodes, n_nodes with
        | [v1], [v2] when v2 <> Minus && v1 = Plus ->
          edit_edge_label bipartite v1 Label.Ins v2
        | [v1], [v2] when v1 <> Plus && v2 = Minus ->
          edit_edge_label bipartite v1 Del v2
        | [v1], [v2] when v2 = Minus && v1 = Plus  ->
          edit_edge_label bipartite v1 Nil v2
        | [v1], [v2] -> 
          let node1 = I.parent t1 (convert_node v1) |> node_exn in
          let node2 = I.parent t2 (convert_node v2) |> node_exn in
          if G.mem_edge bipartite (N.Original node1) (N.Original node2) then
            edit_edge_label bipartite v1 Nil v2
          else
            edit_edge_label bipartite v1 Sub v2
        | [m], l -> 
          case21 m l     
        | l, [m] ->
          case21 m l
        | _, _ ->
          failwith "Impossible case"        
      ) bipartite
    in
    G.iter_edges (fun v1 v2 -> 
      if (G.V.label v1) <> (G.V.label v2) then
        edit_edge_label bipartite v1 (Comb ((fst (G.E.label (G.find_edge bipartite v1 v2))), Upd)) v2
    ) bipartite;

end
