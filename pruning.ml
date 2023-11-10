module Make(I: Sig.INPUT)(N: Sig.Node with module Input = I)(G: Sig.G with type V.t = N.t)
  = struct
  type heap = Core.Std.Heap.t
  type fmt = (G.V.t * G.V.t, Cost.t) Hashtbl.t
  type uct = (I.v * I.v, Cost.t) Hashtbl.t
  type lbt = (G.V.t, heap) Hashtbl.t
  
  (* A forced move is defined as follows: 
         C-MF(m', n) = cm if there doesn't exist n' in C(n) s.t. m'n' is an edge
                       0  otherwise (or Cost.null) *)
  let forced_move_cost (m': I.v) (n: I.v) (t1: I.t) (t2: I.t) (graph: G.t) : Cost.t =
    let pred = fun n' -> G.mem_edge graph (N.Original m') (N.Original n') in
    if List.exists (pred) (I.children t2 n) then Cost.null 
    else Cost.cm
        
  (* Computation of the "forced move" cost.

     Note that the "conditional move" cost for two nodes is the cost of a move
     minus the "forced move" cost, thus we can use the "forced move" cost to
     efficiently compute the "conditional move" cost. *)
  let compute_forced_moves (t1: I.t) (t2: I.t) (graph: G.t) : fmt =
    let table = Hashtbl.create (G.nb_edges graph) in
    let to_vertices = fun m' n -> (N.Original m', N.Original n) in
    let fmc = fun m' n -> forced_move_cost m' n t1 t2 graph in
    List.iter
      (fun m' -> List.iter (fun n -> Hashtbl.add table (to_vertices m' n) (fmc m' n)) (I.elements t2))
      (I.elements t1);
    (table)

  (* Computation of all the update costs. It is done only once and stored as-is as
     computing it every time that is needed might be costly. *)
  let compute_update_costs (t1: I.t) (t2: I.t) (n: int) : uct =
    let table = Hashtbl.create n in
    List.iter
      (fun m -> List.iter (fun n -> Hashtbl.add table (m,n) (I.compare t1 m n)) (I.elements t2))
      (I.elements t1);
    (table)

  (* Initial computation of the lower-bound for every edge.
     Every vertex of t1 stores a min-heap with the lower-bound cost on every edge. *)
  (*let compute_initial_lower_bound (t1: I.t) (t2: I.t) (table1: fmt) (table2: fmt) (ucosts:
     uct) (n: int) : lbt =
    let table = Hashtbl.create n in
    (* TODO: For each m, create a heap with Heap.create and populate it using a nice
     comparator and the computed cost (which should be accessible in O(1)) *)
    List.iter
      (fun m -> m)
      (I.elements t1);
    (table) *)
  
  
  let prune (t1: I.t) (t2: I.t) (graph: G.t) : G.t =
    let table1 = compute_forced_moves t1 t2 graph in
    let table2 = compute_forced_moves t2 t1 graph in
    let update_costs = compute_update_costs t1 t2 (G.nb_edges graph) in
    (* BEFORE TODO: compute the sums in the lower-bound to get them in nice time after. *)
    (*let lower_bound = compute_initial_lower_bound t1 t2 table1 table2 update_costs 
      (G.nb_edges graph) in *)
    graph
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
   Update them on the fly in O(1). *)

(* Steps:
   1. Compute all the costs and store them to be able to update them in O(1) in
   the future (costs at most O(n^3)).
   2. For every vertex, create a min-heap (from Core.Std) to put the
   lower-bounds. and a max-heap to put the upper-bounds.
   3. Pick a vertex at random, and see if an edge is prunable (O(logn + n)).
   4. If an edge has been pruned, update all the costs (O(n^2 log(n))).
   5. Repeat while an edge is pruned (at most O(n^2) times).
 *)
