module Make(I: Sig.INPUT)(N: Sig.Node with module Input = I)(G: Sig.G with type V.t = N.t)
  = struct
  module Heap = Pairing_heap
  type cost_table = (G.V.t * G.V.t, Cost.t) Hashtbl.t
  type heap_el = Cost.t
  type token_table = (G.V.t, heap_el Heap.Elt.t) Hashtbl.t
  type ub_table = (G.V.t, (token_table * heap_el Heap.t)) Hashtbl.t

  let mk_v (x: I.v) : N.t = N.Original x
  
  (* A forced move is defined as follows: 
         C-MF(m', n) = cm if there doesn't exist n' in C(n) s.t. m'n' is an edge
                       0  otherwise (or Cost.null) *)
  let forced_move_cost (m': I.v) (n: I.v) (t1: I.t) (t2: I.t) (graph: G.t) : Cost.t =
    let child_has_edge = fun n' -> G.mem_edge graph (mk_v m') (mk_v n') in
    if List.exists (child_has_edge) (I.children t2 n) then Cost.null 
    else Cost.cm
        
  (* Computation of the "forced move" cost.

     Note that the "conditional move" cost for two nodes is the cost of a move
     minus the "forced move" cost, thus we can use the "forced move" cost to
     efficiently compute the "conditional move" cost. *)
  let compute_forced_moves (t1: I.t) (t2: I.t) (graph: G.t) : cost_table =
    let table = Hashtbl.create (G.nb_edges graph) in
    let fmc = fun m' n -> forced_move_cost m' n t1 t2 graph in
    let iterate = fun m' n -> let cost = fmc m' n in
      Hashtbl.add table (mk_v m', mk_v n) cost;
      Hashtbl.add table (mk_v n, mk_v m') cost in
    List.iter
      (fun m' -> List.iter (iterate m') (I.elements t2))
      (I.elements t1);
    (table)

  (* Computation of all the update costs. It is done only once and stored as-is as
     computing it every time that is needed might be costly. *)
  let compute_update_costs (t1: I.t) (t2: I.t) (n: int) : cost_table =
    let table = Hashtbl.create (2*n) in
    let add =
      fun x y c ->
      Hashtbl.add table (mk_v x, mk_v y) c;
      Hashtbl.add table (mk_v y, mk_v x) c in
    List.iter
      (fun m -> List.iter (fun n -> add m n (I.compare t1 m n)) (I.elements t2))
      (I.elements t1);
    (table)

  (* Given the update costs and forced moves tables, computes the upper-bound cost of the
     edge xy. *)
  let compute_upper_bound (x: G.V.t) (y: G.V.t) (fm1: cost_table) (fm2: cost_table)
        (update_costs: cost_table) (t1: I.t) (t2: I.t) (graph: G.t) : Cost.t =
    match (x, y) with
    | N.Plus, N.Minus -> Cost.null
    | N.Plus, _ -> Cost.ci
    | _, N.Minus -> Cost.cd
    | N.Original m, N.Original n ->
       let cw = Hashtbl.find update_costs (x, y) in
       let cm1 = fun x y -> Cost.cm - (Hashtbl.find fm1 (mk_v x,y)) in
       let cm2 = fun x y -> Cost.cm - (Hashtbl.find fm2 (mk_v x,y)) in
       let left = List.fold_left (fun s m' -> s + (Cost.cc * ((G.out_degree graph (mk_v m')) - 1) +
                                                     (cm1 m' y))) 0 (I.children t1 m) in
       let right = List.fold_left (fun s n' -> s + (Cost.cc * ((G.out_degree graph (mk_v n')) - 1) +
                                                      (cm2 n' x))) 0 (I.children t2 n) in
       2*cw + left + right
    | N.Minus, _ | _, N.Plus -> assert false

  (* Inits the min-heap at node x. *)
  let init_min_heap (x: G.V.t) (succ: G.V.t list) (fm1: cost_table) (fm2: cost_table)
        (update_costs: cost_table) (t1: I.t) (t2: I.t) (graph: G.t) : (token_table * heap_el Heap.t) =
    let heap = Heap.create ~min_size:(List.length succ) ~cmp:(Cost.compare) () in
    let token_table = Hashtbl.create (List.length succ) in
    List.iter
      (fun y ->
        let token = Heap.add_removable heap (compute_upper_bound x y fm1 fm2 update_costs
                                               t1 t2 graph) in 
        Hashtbl.add token_table y token)
      succ;
    (token_table, heap)

  (* Computation of all the initial upper-bounds for each edge and storage in a min-heap
     for each vertex. *)
  let init_upper_bounds (t1: I.t) (t2: I.t) (graph: G.t) (fm1: cost_table) (fm2: cost_table)
        (update_costs: cost_table) : ub_table =
    let table = Hashtbl.create (G.nb_vertex graph) in
    let transform = List.map (fun x -> N.Original x) in
    let iterate t1 t2 fm1 fm2 v =
      Hashtbl.add table v (init_min_heap v (G.succ graph v) fm1 fm2 update_costs t1 t2
                             graph) in
    List.iter (iterate t1 t2 fm1 fm2) (transform (I.elements t1));
    List.iter (iterate t2 t1 fm2 fm1) (transform (I.elements t2));
    List.iter (iterate t1 t2 fm1 fm2) [N.Plus];
    List.iter (iterate t2 t1 fm2 fm1) [N.Minus];
    (table)

  (* First pruning rule. *)
  let is_prunable_1 (m: G.V.t) (n: G.V.t) (lb: Cost.t) (ub_table: ub_table) : bool =
    let ub1 = Heap.top_exn (snd (Hashtbl.find ub_table m)) in
    let ub2 = Heap.top_exn (snd (Hashtbl.find ub_table n)) in
    lb >= ub1 + ub2 + 4*(Cost.ct)

  (* Second pruning rule. *)
  let is_prunable_2 (lb: Cost.t) : bool =
    lb >= 2*(Cost.cd + Cost.ci)

  (* Computation of the lower-bound. *)
  let compute_lower_bound (x: G.V.t) (y: G.V.t) (fm1: cost_table) (fm2: cost_table)
        (update_costs : cost_table) (t1: I.t) (t2: I.t) (graph: G.t) : Cost.t =
    match (x, y) with
    | N.Original m, N.Original n ->
       let cm1 = fun x y -> (Hashtbl.find fm1 (mk_v x,y)) in
       let cm2 = fun x y -> (Hashtbl.find fm2 (mk_v x, y)) in
       let cu = Hashtbl.find update_costs (x,y) in
       let ca, cb =
         try 
           let ca = List.fold_left (fun s m' -> s + (cm1 m' y)) 0 (I.elements t1) in
           let cb = List.fold_left (fun s n' -> s + (cm2 n' x)) 0 (I.elements t2) in
           ca, cb
         with
           Not_found ->
           let ca = List.fold_left (fun s m' -> s + (cm1 m' y)) 0 (I.elements t2) in
           let cb = List.fold_left (fun s n' -> s + (cm2 n' x)) 0 (I.elements t1) in
           ca, cb
       in 2*cu + (Stdlib.min ca cb)
    | _, _ -> Cost.null
  
  (* Tries to prune an edge [m,n]. If it succeeds, tries to prune the updated edges. *)
  let try_prune (m: G.V.t) (n: G.V.t) (fm1: cost_table) (fm2: cost_table)
        (update_costs: cost_table) (ub_table: ub_table) (t1: I.t) (t2: I.t) (graph: G.t) : unit =
    let lb_cost = compute_lower_bound m n fm1 fm2 update_costs t1 t2 graph in
    if (is_prunable_1 m n lb_cost ub_table) || (is_prunable_2 lb_cost) then (
      let m_ds = Hashtbl.find ub_table m in
      let n_ds = Hashtbl.find ub_table n in
      let token = Hashtbl.find (fst m_ds) n in
      Heap.remove (snd m_ds) token;
      let token = Hashtbl.find (fst n_ds) m in
      Heap.remove (snd n_ds) token;
      G.remove_edge graph m n;
      () (* Update: cmf(m, p(n)) and cmf(n, p(m)) and launch try_prune on the nodes that
            make their lower-bound change. *)
    )
  
  let prune (t1: I.t) (t2: I.t) (graph: G.t) : G.t =
    let fm1 = compute_forced_moves t1 t2 graph in
    let fm2 = compute_forced_moves t2 t1 graph in
    let update_costs = compute_update_costs t1 t2 (G.nb_edges graph) in
    let upper_bound_costs = init_upper_bounds t1 t2 graph fm1 fm2 update_costs in
    G.iter_edges
      (fun m n -> try_prune m n fm1 fm2 update_costs upper_bound_costs t1 t2 graph)
      graph;
    graph
end
