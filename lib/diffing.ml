module Make(I: Sig.INPUT) = struct
  type patch = (I.node Sig.patch)

  (* -------------------- Graph definition -------------------- *)
  
  (* Creation of a generic Node from the INPUT type. *)
  module Node = struct
    module Input = I
    
    type t = Original of Input.node | Minus | Plus | Dummy of int
    let mk v = Original v
    let minus () = Minus
    let plus () = Plus
    let compare = Stdlib.compare
    let hash = Hashtbl.hash
    let equal = (=)
  end

  (* Instantiation of the module of the working graph. *)
  module G = Graph.Imperative.Graph.ConcreteLabeled(Node)(Sig.Edge)

  (* -------------------- End of Graph definition -------------------- *)
  
  (* -------------------- Graph creation -------------------- *)

  (* The goal is to create a complete bipartite graph between the nodes : (t1 *)
  (* and insertion) and (t2 and deletion). *)
  let create_bipartite (t1: I.t) (t2: I.t) : G.t =
    let transform = List.map (fun x -> Node.mk x) in
    let nodes1 = Node.plus() :: (transform (I.elements t1)) in
    let nodes2 = Node.minus() :: (transform (I.elements t2)) in
    let g = G.create ~size:((List.length nodes1) + (List.length nodes2)) () in
    List.iter (G.add_vertex g) nodes1;
    List.iter (G.add_vertex g) nodes2;
    List.iter (fun x -> List.iter (G.add_edge g x) nodes2) nodes1;
    g

  (* -------------------- End of Graph creation  -------------------- *)

  (* -------------------- Modules instantiation -------------------- *)

  module P = Pruning.Make(I)(Node)(G)
  module Dict = Dico.Make(I)
  module EC = Edge_cover.Make(I)(Node)(G)
  
  (* -------------------- End of Modules instantation -------------------- *)
  
  let mhdiff (t1: I.t) (t2: I.t) : patch =
    let graph = create_bipartite t1 t2 in
    Printf.printf "#edges before prune: %d\n" (G.nb_edges graph);
    let graph = P.prune t1 t2 graph in
    Printf.printf "#edges after prune: %d\n" (G.nb_edges graph);
    let graph = EC.bipartite_min_edge_cover graph in
    (* Then flows then patch reconstruction *)
    (* G.iter_edges *)
    (*   (fun x y -> *)
    (*     match (x, y) with *)
    (*     | Node.Plus, Node.Minus | Node.Minus, Node.Plus -> Printf.printf "-,+ " *)
    (*     | Node.Plus, Node.Original x' | Node.Original x', Node.Plus *)
    (*       -> Printf.printf "%s,+ " (I.label x') *)
    (*     | Node.Minus, Node.Original x' | Node.Original x', Node.Minus *)
    (*       -> Printf.printf "-,%s " (I.label x') *)
    (*     | Node.Original x', Node.Original y' -> *)
    (*        Printf.printf "%s,%s " (I.label x') (I.label y') *)
    (*     | _, _ -> failwith "internal error") graph; *)
    []
  
  let dictionnary (t1: I.t) (t2: I.t) : patch =
    Dict.diffing t1 t2

  let exec (t1: I.t) (t2: I.t) : patch =
    if !Opt.dico then dictionnary t1 t2
    else mhdiff t1 t2

  (* -------------------- Printing the patch -------------------- *)

  let display (patch: patch) : unit=
    let rec aux (modif: patch) : unit =
      match modif with
      | [] -> ()
      | h :: t ->
         (match h with
          | Ins node -> Printf.printf "* Insertion: %s\n" (I.label node)
          | Del node -> Printf.printf "* Deletion: %s\n" (I.label node)
          | Upd (n1, n2) -> Printf.printf "* Update: %s --> %s\n" (I.label n1) (I.label n2)
          | _ -> failwith "internal error");
         aux t
    in aux patch
end
