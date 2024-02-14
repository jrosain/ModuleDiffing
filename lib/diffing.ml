module Make(I: Sig.INPUT) = struct
  type patch = (I.node * Label.t * I.node) list
  type algo = Dict | Mh_diff

  (* -------------------- Graph definition -------------------- *)
  
  (* Creation of a generic Node from the INPUT type. *)
  module Node = struct
    module Input = I
    
    type t = Original of Input.node | Minus | Plus
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
  
  (* -------------------- End of Modules instantation -------------------- *)
  
  let mhdiff (t1: I.t) (t2: I.t) : patch =
    let graph = create_bipartite t1 t2 in
    Printf.printf "#edges before prune: %d\n" (G.nb_edges graph);
    let graph = P.prune t1 t2 graph in
    Printf.printf "#edges after prune: %d\n" (G.nb_edges graph);
    []
  
  let dictionnary (_: I.t) (_: I.t) : patch =
    []

  let exec ?(algo = Mh_diff) (t1: I.t) (t2: I.t) : patch =
    match algo with
    | Mh_diff -> mhdiff t1 t2
    | Dict    -> dictionnary t1 t2

  (* -------------------- Printing the patch -------------------- *)

  let display (patch: patch) =
    let rec aux (modif: patch) =
      match modif with
      | [] -> ()
      | _ :: t -> (* display_modif h; *) aux t
    in aux patch
end
