(* Wrapper around the heap of core_kernel to make it easier to use. *)
module UpperBoundHeap = struct
  module Heap = Pairing_heap
  type heap_el = Cost.t
  type 'a token_table = ('a, heap_el Heap.Elt.t) Hashtbl.t
  type 'a t = ('a token_table) * heap_el Heap.t

  (* Creation of a heap of the given size capacity. It will not resize unless the
     capacity is exceeded. *)
  let create (size: int) : 'a t =
    let heap = Heap.create ~min_size:size ~cmp:(Cost.compare) () in
    let token_table = Hashtbl.create size in
    (token_table, heap)

  (* Addition of a removable element in the heap. *)
  let add (heap: 'a t) (target: 'a) (value: heap_el) : unit =
    let token = Heap.add_removable (snd heap) value in
    Hashtbl.add (fst heap) target token

  (* Access to the minimum element in constant time. *)
  let top (heap: 'a t) : heap_el =
    Heap.top_exn (snd heap)

  (* Removal of an element in O(log(n)) time. *)
  let remove (heap: 'a t) (target: 'a) : unit =
    let token = Hashtbl.find (fst heap) target in
    Heap.remove (snd heap) token

  (* Update of an element in O(log(n)) time. *)
  let update (heap: 'a t) (target: 'a) (value: heap_el) : unit =
    let token = Heap.update (snd heap) (Hashtbl.find (fst heap) target) value in
    Hashtbl.replace (fst heap) target token
end

(* Wrapper around the table that keeps the cost of different edges. *)
module CostTable = struct
  type 'a t = ('a * 'a, Cost.t) Hashtbl.t

  (* Creation of a cost table with capacity size. It will not resize unless the capacity
     is exceeded. *)
  let create (size: int) : 'a t =
    let table = Hashtbl.create (2*size) in
    table

  (* Addition of a value to the given pair in the cost table. *)
  let add (table: 'a t) (x: 'a) (y: 'a) (value: Cost.t) : unit =
    Hashtbl.add table (x, y) value;
    Hashtbl.add table (y, x) value

  (* Get the value at the given pair in the cost table. *)
  let get (table: 'a t) (x: 'a) (y: 'a) : Cost.t =
    Hashtbl.find table (x, y)

  (* Replacement of the value of the given pair in the cost table. *)
  let replace (table: 'a t) (x: 'a) (y: 'a) (value: Cost.t) : unit =
    Hashtbl.replace table (x, y) value;
    Hashtbl.replace table (y, x) value
end

module Make(I: Sig.INPUT)(N: Sig.Node with module Input = I)(G: Sig.G with type V.t = N.t)
  = struct
  type cost_table = G.V.t CostTable.t
  type heap = G.V.t UpperBoundHeap.t
  type ub_table = (G.V.t, heap) Hashtbl.t

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
    let table = CostTable.create (G.nb_edges graph) in
    let fmc = fun m' n -> forced_move_cost m' n t1 t2 graph in
    let iterate = fun m' n -> CostTable.add table (mk_v m') (mk_v n) (fmc m' n) in
    List.iter
      (fun m' -> List.iter (iterate m') (I.elements t2))
      (I.elements t1);
    (table)

  (* Computation of all the update costs. It is done only once and stored as-is as
     computing it every time that is needed might be costly. *)
  let compute_update_costs (t1: I.t) (t2: I.t) (n: int) : cost_table =
    let table = CostTable.create n in
    let add = fun x y c -> CostTable.add table (mk_v x) (mk_v y) c in
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
    | N.Plus, _ | _, N.Plus -> Cost.ci
    | _, N.Minus | N.Minus, _ -> Cost.cd
    | N.Original m, N.Original n ->
       let transform = List.map (fun x -> mk_v x) in
       let c1 = (transform (I.children t1 m)) in
       let c2 = (transform (I.children t2 n)) in  
       let cw = CostTable.get update_costs x y in
       let cm1 = Cost.f_conditional_move (fun x y -> (CostTable.get fm1 x y)) in
       let cm2 = Cost.f_conditional_move (fun x y -> (CostTable.get fm2 x y)) in
       let out_deg = fun m -> G.out_degree graph m in
       Cost.upper_bound out_deg c1 c2 cm1 cm2 cw x y
  
  (* Inits the min-heap at node x. *)
  let init_min_heap (x: G.V.t) (succ: G.V.t list) (fm1: cost_table) (fm2: cost_table)
        (update_costs: cost_table) (t1: I.t) (t2: I.t) (graph: G.t) : heap =
    let heap = UpperBoundHeap.create (List.length succ) in
    List.iter
      (fun y ->
        let ub_cost = (compute_upper_bound x y fm1 fm2 update_costs t1 t2 graph) in
        UpperBoundHeap.add heap y ub_cost)
      succ;
    (heap)

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
    let ub1 = UpperBoundHeap.top (Hashtbl.find ub_table m) in
    let ub2 = UpperBoundHeap.top (Hashtbl.find ub_table n) in
    Cost.prune_rule_1 lb ub1 ub2

  (* Second pruning rule. *)
  let is_prunable_2 = Cost.prune_rule_2

  (* Computation of the lower-bound. *)
  let compute_lower_bound (x: G.V.t) (y: G.V.t) (fm1: cost_table) (fm2: cost_table)
        (update_costs : cost_table) (t1: I.t) (t2: I.t) (graph: G.t) : Cost.t =
    let transform = List.map (fun x -> mk_v x) in
    let cm1 = fun n m' -> (CostTable.get fm1 m' n) in
    let cm2 = fun m n' -> (CostTable.get fm2 n' m) in
    let nil = fun _ -> Cost.null in
    let cma, ca, cmb, cb, cu =
      (match x, y with
       | N.Original m, N.Original n ->
          let cu = CostTable.get update_costs x y in
          (try (cm1 y), (I.children t1 m), (cm2 x), (I.children t2 n), cu
           with _ -> (cm2 y), (I.children t2 m), (cm1 x), (I.children t1 n), cu)
       | N.Plus, N.Minus -> nil, [], nil, [], Cost.null
       | N.Original a, N.Plus | N.Plus, N.Original a ->
          let allci = fun _ -> Cost.ci in
          let children = try (I.children t1 a) with _ -> (I.children t2 a) in
          nil, [], allci, children, Cost.ci
       | N.Original a, N.Minus | N.Minus, N.Original a ->
          let allcd = fun _ -> Cost.cd in
          let children = try (I.children t1 a) with _ -> (I.children t2 a) in
          nil, [], allcd, children, Cost.cd
       | _, _ -> nil, [], nil, [], Cost.null) in
    Cost.lower_bound cma (transform ca) cmb (transform cb) cu
  
  (* Tries to prune an edge [m,n]. If it succeeds, tries to prune the updated edges. *)
  let rec try_prune (m: G.V.t) (n: G.V.t) (fm1: cost_table) (fm2: cost_table)
        (update_costs: cost_table) (ub_table: ub_table) (t1: I.t) (t2: I.t) (graph: G.t) :
            unit =
    if (G.mem_edge graph m n) then
      let lb_cost = compute_lower_bound m n fm1 fm2 update_costs t1 t2 graph in
      if (is_prunable_1 m n lb_cost ub_table) || (is_prunable_2 lb_cost) then (
        (* You can't move anymore fm1 towards fm2. *)
        (try
           CostTable.replace fm1 m n Cost.null; CostTable.replace fm2 n m Cost.null
         with
           _ ->
           CostTable.replace fm2 m n Cost.null; CostTable.replace fm1 n m Cost.null);
        (* Remove the edge in the graph & in the priority queues. *)
        let m_ds = Hashtbl.find ub_table m in
        let n_ds = Hashtbl.find ub_table n in
        UpperBoundHeap.remove m_ds n;
        UpperBoundHeap.remove n_ds m;
        G.remove_edge graph m n;
        (* Update of the costs (forced move costs and upper-bound costs). *)
        match (m,n) with
        | N.Original x, N.Original y -> 
           let parent_m = I.parent t1 x in
           let parent_n = I.parent t2 y in
           (match parent_m with
            | None -> ()
            | Some pm' ->
               let pm = mk_v pm' in
               let fmc2 = (forced_move_cost y pm' t2 t1 graph) in
               CostTable.replace fm2 pm n fmc2;
               let ubpmn = compute_upper_bound pm n fm1 fm2 update_costs t1 t2 graph in
               UpperBoundHeap.update n_ds pm ubpmn;
               let pm_ds = Hashtbl.find ub_table pm in
               UpperBoundHeap.update pm_ds n ubpmn;
               try_prune pm n fm1 fm2 update_costs ub_table t1 t2 graph               
           );
           (match parent_n with
            | None -> ()
            | Some pn' ->
               let pn = mk_v pn' in
               let fmc1 = (forced_move_cost x pn' t1 t2 graph) in
               CostTable.replace fm1 m pn fmc1;
               let ubpnm = compute_upper_bound m pn fm1 fm2 update_costs t1 t2 graph in
               UpperBoundHeap.update m_ds pn ubpnm;
               let pn_ds = Hashtbl.find ub_table pn in
               UpperBoundHeap.update pn_ds m ubpnm;
               try_prune m pn fm1 fm2 update_costs ub_table t1 t2 graph
           );
        | _, _ -> ()
      )
  
  let prune (t1: I.t) (t2: I.t) (graph: G.t) : G.t =
    let fm1 = compute_forced_moves t1 t2 graph in
    let fm2 = compute_forced_moves t2 t1 graph in
    let update_costs = compute_update_costs t1 t2 (G.nb_edges graph) in
    let upper_bound_costs = init_upper_bounds t1 t2 graph fm1 fm2 update_costs in
    G.iter_edges
      (fun m n -> try_prune m n fm1 fm2 update_costs upper_bound_costs t1 t2 graph)
      graph;
    (* TODO: add lb cost to the remaining edges. *)
    graph
end
