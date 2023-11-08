module Make(I: Sig.INPUT) = struct
  type patch = Empty

  (* Creation of a generic Node from the INPUT type. *)
  module Node = struct
    type t = Original of I.v | Minus | Plus
    let compare = Stdlib.compare
    let hash = Hashtbl.hash
    let equal = (=)
  end

  (* Instantiation of the module of the working graph. *)
  module G = Graph.Imperative.Graph.ConcreteLabeled(Node)(Sig.Edge)

  (* -------------------- PART1 -------------------- *)

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

  (* We want to prune as much edges as possible. For that, we compute *)
  let prune_edges (graph: G.t) (t1: I.t) (t2: I.t) =
    ()

  (* -------------------- END OF PART1 -------------------- *)

  (* TODO: 1 functor for every part of the algorithm taking as argument I,
  Node, and the module G. *)
  
  let exec (t1: I.t) (t2: I.t) : patch =
    let graph = create_bipartite t1 t2 in
    prune_edges graph t1 t2;
    (* Then flows then patch reconstruction *)
    Empty
end

(* By maintaining a priority queue (based on edge costs) of edges incident on *)
(* each node of the induced graph, the test to determine whether an edge may be *)
(* pruned can be performed in constant time. If the edge is pruned, removing it *)
(* from the induced graph requires constant time, while removing it from the *)
(* priority queues at each of its nodes requires O(logn) time. When an edge [m; *)
(* n] is pruned, we also record the changes to the costs cm?(m; p(n)), cm?(n; *)
(* p(m)), cmf (m; p(n)), and cmf (n; p(m)), which can be done in constant *)
(* time. Thus, pruning an edge requires O(logn) time. Since at most O(n^2) are *)
(* pruned, the total worst case cost of the pruning phase is O(n^2logn) *) 

(* Goal: precompute all the costs and keep them in a data-structure.
   Update them on the fly in O(1).

 *)
