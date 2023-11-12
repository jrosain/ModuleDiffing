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

(* Wrapper around the two forced moves tables. 
   The first cost table corresponds to the forced move from a node of the first input
   struct to the second, and the second cost-table is analogous, i.e., corresponds to the
   forced move of a node from the second struct to the first.
   These values are not symmetric, thus the need of storing both cost tables. *)
module ForcedMoves = struct
  type 'a t = 'a CostTable.t * 'a CostTable.t

  let init (tbl1: 'a CostTable.t) (tbl2: 'a CostTable.t) : 'a t =
    (tbl1, tbl2)

  let get_t1 (tbl: 'a t) (x: 'a) (y: 'a) : Cost.t =
    CostTable.get (fst tbl) x y

  let get_t2 (tbl: 'a t) (x: 'a) (y: 'a) : Cost.t =
    CostTable.get (snd tbl) x y

  let update_t1 (tbl: 'a t) (x: 'a) (y: 'a) (value: Cost.t) : unit =
    CostTable.replace (fst tbl) x y value

  let update_t2 (tbl: 'a t) (x: 'a) (y: 'a) (value: Cost.t) : unit =
    CostTable.replace (snd tbl) x y value
end

module Make(I: Sig.INPUT)(N: Sig.Node with module Input = I)(G: Sig.G with type V.t = N.t
  and type E.label = Cost.t)
  = struct
  type cost_table = G.V.t CostTable.t
  type heap = G.V.t UpperBoundHeap.t
  type ub_table = (G.V.t, heap) Hashtbl.t
  type fm_table = G.V.t ForcedMoves.t

  let mk_v (x: I.v) : N.t = N.Original x
  
  (* Transformation of a list of I.t in G.V.t. *)
  let transform = List.map (fun x -> mk_v x)
  
  (* A forced move is defined as follows: 
n         C-MF(m', n) = cm if there doesn't exist n' in C(n) s.t. m'n' is an edge
                       0  otherwise (or Cost.null) *)
  let forced_move_cost (m': I.v) (n: I.v) (other: I.t) (graph: G.t) : Cost.t =
    let child_has_edge = fun n' -> G.mem_edge graph (mk_v m') (mk_v n') in
    if List.exists (child_has_edge) (I.children other n) then Cost.null 
    else Cost.cm
        
  (* Computation of the "forced move" cost.

     Note that the "conditional move" cost for two nodes is the cost of a move
     minus the "forced move" cost, thus we can use the "forced move" cost to
     efficiently compute the "conditional move" cost. *)
  let compute_forced_moves (t1: I.t) (t2: I.t) (graph: G.t) : cost_table =
    let table = CostTable.create (G.nb_edges graph) in
    let fmc = fun m' n -> forced_move_cost m' n t2 graph in
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
  let compute_upper_bound (x: G.V.t) (y: G.V.t) (fm: fm_table) (update_costs: cost_table)
        (t1: I.t) (t2: I.t) (graph: G.t) : Cost.t =
    match (x, y) with
    | N.Plus, N.Minus -> Cost.null
    | N.Plus, _ | _, N.Plus -> Cost.ci
    | _, N.Minus | N.Minus, _ -> Cost.cd
    | N.Original m, N.Original n ->
       let c1 = (transform (I.children t1 m)) in
       let c2 = (transform (I.children t2 n)) in  
       let cw = CostTable.get update_costs x y in
       let cm1 = Cost.f_conditional_move (fun x y -> (ForcedMoves.get_t1 fm x y)) in
       let cm2 = Cost.f_conditional_move (fun x y -> (ForcedMoves.get_t2 fm x y)) in
       let out_deg = fun m -> G.out_degree graph m in
       Cost.upper_bound out_deg c1 c2 cm1 cm2 cw x y
  
  (* Inits the min-heap at node x. *)
  let init_min_heap (x: G.V.t) (succ: G.V.t list) (fm: fm_table) (update_costs: cost_table) 
        (t1: I.t) (t2: I.t) (graph: G.t) (reverse: bool) : heap =
    let heap = UpperBoundHeap.create (List.length succ) in
    List.iter
      (fun y ->
        let x, y = if reverse then y, x else x, y in
        let ub_cost = (compute_upper_bound x y fm update_costs t1 t2 graph) in
        let x, y = if reverse then y, x else x, y in
        UpperBoundHeap.add heap y ub_cost)
      succ;
    (heap)

  (* Computation of all the initial upper-bounds for each edge and storage in a min-heap
     for each vertex. *)
  let init_upper_bounds (t1: I.t) (t2: I.t) (graph: G.t) (fm: fm_table)
        (update_costs: cost_table) : ub_table =
    let table = Hashtbl.create (G.nb_vertex graph) in
    let iterate = fun reverse v -> let succ = G.succ graph v in
      Hashtbl.add table v (init_min_heap v succ fm update_costs t1 t2 graph reverse) in
    List.iter (iterate false) (N.Plus :: (transform (I.elements t1)));
    List.iter (iterate true) (N.Minus :: (transform (I.elements t2)));
    (table)

  (* First pruning rule. *)
  let is_prunable_1 (m: G.V.t) (n: G.V.t) (lb: Cost.t) (ub_table: ub_table) : bool =
    let ub1 = UpperBoundHeap.top (Hashtbl.find ub_table m) in
    let ub2 = UpperBoundHeap.top (Hashtbl.find ub_table n) in
    Cost.prune_rule_1 lb ub1 ub2

  (* Second pruning rule. *)
  let is_prunable_2 = Cost.prune_rule_2

  (* Computation of the lower-bound. 
     We should be assured that x is in V(T1) U { + } and that y is in V(T2) U { - }. *)
  let compute_lower_bound (x: G.V.t) (y: G.V.t) (fm: fm_table) (update_costs : cost_table)
        (t1: I.t) (t2: I.t) (graph: G.t) : Cost.t =
    let cm1 = fun n m' -> (ForcedMoves.get_t1 fm m' n) in
    let cm2 = fun m n' -> (ForcedMoves.get_t2 fm n' m) in
    let nil = fun _ -> Cost.null in
    let cma, ca, cmb, cb, cu =
      (match x, y with
       | N.Original m, N.Original n ->
          let cu = CostTable.get update_costs x y in
          (cm1 y), (I.children t1 m), (cm2 x), (I.children t2 n), cu
       (* TODO: Really? *)
       | _, _ -> nil, [], nil, [], Cost.null) in
    Cost.lower_bound cma (transform ca) cmb (transform cb) cu

  (* Removal of the edge [mn] in the graph and in the min-heaps of [m] and [n]. *)
  let remove_edge (m: G.V.t) (n: G.V.t) (ub_table: ub_table) (graph: G.t) : unit =
    let m_heap = Hashtbl.find ub_table m in
    let n_heap = Hashtbl.find ub_table n in
    UpperBoundHeap.remove m_heap n;
    UpperBoundHeap.remove n_heap m;
    G.remove_edge graph m n

  (* Update of the forced move after the removal of an edge. *)
  let update_forced_moves (x: I.v) (y: I.v) (fm: fm_table) (t1: I.t) (t2: I.t)
        (update_costs: cost_table) (graph: G.t) (ub_table: ub_table) : (G.V.t option * G.V.t option) =
    let m = mk_v x in
    let n = mk_v y in
    let px = I.parent t1 x in
    let py = I.parent t2 y in
    let pm =
      match px with
      | None -> None
      | Some pm' ->
         let pm = mk_v pm' in
         let pm_heap = Hashtbl.find ub_table pm in
         let n_heap = Hashtbl.find ub_table n in
         let fmc = (forced_move_cost y pm' t1 graph) in
         ForcedMoves.update_t2 fm pm n fmc;
         let upper_bound_pmn = compute_upper_bound pm n fm update_costs t1 t2 graph in
         UpperBoundHeap.update pm_heap n upper_bound_pmn;
         UpperBoundHeap.update n_heap pm upper_bound_pmn;
         Some pm in
    let pn =
      match py with
      | None -> None
      | Some pn' ->
         let pn = mk_v pn' in
         let pn_heap = Hashtbl.find ub_table pn in
         let m_heap = Hashtbl.find ub_table m in
         let fmc = (forced_move_cost x pn' t2 graph) in
         ForcedMoves.update_t1 fm m pn fmc;
         let upper_bound_mpn = compute_upper_bound m pn fm update_costs t1 t2 graph in
         UpperBoundHeap.update m_heap pn upper_bound_mpn;
         UpperBoundHeap.update pn_heap m upper_bound_mpn;
         Some pn in
    (pm, pn)
         
  (* Tries to prune an edge [m,n]. If it succeeds, tries to prune the updated edges. *)
  let rec try_prune (m: G.V.t) (n: G.V.t) (fm: fm_table) (update_costs: cost_table)
            (ub_table: ub_table) (t1: I.t) (t2: I.t) (graph: G.t) : unit =
    if (G.mem_edge graph m n) then
      let lb_cost = compute_lower_bound m n fm update_costs t1 t2 graph in
      if (is_prunable_1 m n lb_cost ub_table) || (is_prunable_2 lb_cost) then (
        remove_edge m n ub_table graph;
        (* If m & n are from t1 & t2, then we need to update the forced moves values of
           the parents. *)
        let pm, pn = (match (m, n) with
                      | N.Original x, N.Original y -> update_forced_moves x y fm t1 t2
                                                        update_costs graph ub_table
                      | _, _ -> None, None) in

        (* TODO: what if we remove [+,N.Original z] or something..? *)
        (match pm with
         | None -> ()
         | Some parent -> try_prune parent n fm update_costs ub_table t1 t2 graph);
        (match pn with
         | None -> ()
         | Some parent -> try_prune m parent fm update_costs ub_table t1 t2 graph))

  (* Iterate the pruning on elements of t1.
     By doing it like this, we *assure* that m is gonna be in t1 and n is gonna be in t2.
     It is important for some parts of the algorithm. *)
  let iterate_pruning (t1: I.t) (t2: I.t) (graph: G.t) (fm: fm_table)
        (update_costs: cost_table) (ub_table: ub_table) : unit  =
    List.iter
      (fun m -> G.iter_succ
                  (fun n -> try_prune m n fm update_costs ub_table t1 t2 graph)
                  graph m)
      (N.Plus :: (transform (I.elements t1)))

  (* Creates a graph that has the right labels on each edge. *)
  let update_labels (t1: I.t) (t2: I.t) (graph: G.t) (fm: fm_table)
        (update_costs: cost_table) : G.t =
    let t1_vertices = (transform (I.elements t1)) in
    let t2_vertices = (transform (I.elements t2)) in
    let g = G.create ~size:((List.length t1_vertices) + (List.length t2_vertices) + 2) () in
    List.iter
      (fun m ->
        List.iter
          (fun n ->
            if G.mem_edge graph m n then
              let lower_bound_cost = compute_lower_bound m n fm update_costs t1 t2 graph in
              let edge = G.E.create m (lower_bound_cost) n in
              G.add_edge_e g edge)
       (N.Minus :: t2_vertices))
      (N.Plus :: t1_vertices);
    (g)
  
  let prune (t1: I.t) (t2: I.t) (graph: G.t) : G.t =
    let fm1 = compute_forced_moves t1 t2 graph in (* Edges from t1 to t2. *)
    let fm2 = compute_forced_moves t2 t1 graph in (* Edges from t2 to t1. *)
    let fm = ForcedMoves.init fm1 fm2 in
    let update_costs = compute_update_costs t1 t2 (G.nb_edges graph) in
    let upper_bound_costs = init_upper_bounds t1 t2 graph fm update_costs in
    iterate_pruning t1 t2 graph fm update_costs upper_bound_costs;
    update_labels t1 t2 graph fm update_costs
end
