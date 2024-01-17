




module Make(I: Sig.INPUT) = struct
  type patch = Empty

  (* -------------------- Graph definition -------------------- *)
  
  (* Creation of a generic Node from the INPUT type. *)
  module Node = struct
    type t = Original of I.v | Minus | Plus
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
    let transform = List.map (fun x -> Node.Original x) in
    let nodes1 = Node.Plus :: (transform (I.elements t1)) in
    let nodes2 = Node.Minus :: (transform (I.elements t2)) in
    let g = G.create ~size:((List.length nodes1) + (List.length nodes2)) () in
    List.iter (G.add_vertex g) nodes1;
    List.iter (G.add_vertex g) nodes2;
    List.iter (fun x -> List.iter (G.add_edge g x) nodes1) nodes2;
    g

  (* -------------------- End of Graph creation  -------------------- *)

  (* -------------------- Modules instantiation -------------------- *)

  module P = Pruning.Make(I)(G)

  (* -------------------- End of Modules instantation -------------------- *)
  
  let exec (t1: I.t) (t2: I.t) : patch =
    let graph = create_bipartite t1 t2 in
    let _ = P.prune t1 t2 graph in
    (* Then flows then patch reconstruction *)
    Empty
end
