(* This file implements the pruning phase of the MH-DIFF algorithm described in "Meaningful Change
   Detection in Structured Data" by Sudarshan S. Chawathe and Hector Garcia-Molina. The paper is 
   available at: 
     * https://www.seas.upenn.edu/~zives/03s/cis650/P026.PDF (published version)
     * http://aturing.umcs.maine.edu/~sudarshan.chawathe/opubs/mhdiff-ext1.pdf (extended version) *)

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
  (*let update (heap: 'a t) (target: 'a) (value: heap_el) : unit =
    let token = Heap.update (snd heap) (Hashtbl.find (fst heap) target) value in
    Hashtbl.replace (fst heap) target token*)
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
    try
      Hashtbl.find table (x, y)
    with Not_found -> failwith "internal error"
end

module Make(I: Sig.INPUT)(N: Sig.Node with module Input = I)(G: Sig.G with type V.t = N.t and type E.label = Cost.t) = struct
  type cost_table = G.V.t CostTable.t      (* Type to store update costs. *)
  type heap = G.V.t UpperBoundHeap.t       (* Type of the heaps. *)
  type ub_table = (G.V.t, heap) Hashtbl.t  (* Type to store the heaps w.r.t. the vertices of G. *)

  (* Transformation of a list of I.t in G.V.t. *)
  let transform = List.map (fun x -> N.mk x)
  let t1_vertices (t1: I.t) = N.plus() :: transform (I.elements t1)
  let t2_vertices (t2: I.t) = N.minus() :: transform (I.elements t2)

  (* A forced move is defined as follows: 
         C-MF(m', n) = 0  if there exists n' in C(n) s.t. m'n' is an edge
                       cm otherwise (or Cost.null) *)
  let forced_move_left (other: I.t) (graph: G.t) (m': I.v) (n: I.v) : Cost.t =
    (* TODO: memoization *)
    let child_has_edge = fun n' -> G.mem_edge graph (N.mk m') (N.mk n') in
    if List.exists (child_has_edge) (I.children other n) then Cost.null
    else Cost.cm

  let forced_move_right (other: I.t) (graph: G.t) (m: I.v) (n': I.v) : Cost.t =
    (* TODO: memoization *)
    let child_has_edge = fun m' -> G.mem_edge graph (N.mk m') (N.mk n') in
    if List.exists (child_has_edge) (I.children other m) then Cost.null
    else Cost.cm

  (* Computation of the lower-bound. 
     We should be assured that x is in V(T1) U { + } and that y is in V(T2) U { - }.
     It is a bit different from the one in the paper --- we add the cases V(T1), - and V(T2), +. *)
  let lower_bound (update_costs : cost_table) (graph: G.t) (t1: I.t) (t2: I.t) (x: G.V.t)
        (y: G.V.t)  : Cost.t =
    let cm1 = forced_move_left t2 graph in
    let cm2 = forced_move_right t1 graph in
    match (x, y) with
    | N.Original m, N.Original n ->
          let cu = CostTable.get update_costs x y in
          Cost.lower_bound (cm1 n) (I.children t1 m) (cm2 m) (I.children t2 n) cu
    | _, N.Plus | N.Minus, _ -> failwith "internal error"
    | N.Plus, N.Minus -> Cost.null
    | N.Plus, _  -> Cost.lb_ci()
    | _, N.Minus -> Cost.lb_cd()

  (* Computation of all the update costs. It is done only once and stored as-is as
     computing it every time that is needed might be costly. *)
  let compute_update_costs (t1: I.t) (t2: I.t) (size: int) : cost_table =
    let table = CostTable.create size in
    let add = fun x y c -> CostTable.add table (N.mk x) (N.mk y) c in
    List.iter
      (fun m -> List.iter (fun n -> add m n (I.compare t1 m n)) (I.elements t2))
      (I.elements t1);
    (table)

  (* Given the update costs, computes the upper-bound cost of the edge xy. *)
  let compute_upper_bound (x: G.V.t) (y: G.V.t) (update_costs: cost_table) (t1: I.t) (t2: I.t)
        (graph: G.t) : Cost.t =
    match (x, y) with
    | N.Plus, N.Minus -> Cost.null
    | N.Plus, _ | _, N.Plus -> Cost.ub_ci()
    | _, N.Minus | N.Minus, _ -> Cost.ub_cd()
    | N.Original m, N.Original n ->
       (* Get the children of m in its tree & the children of n in its tree. *)
       let c1 = (I.children t1 m) in
       let c2 = (I.children t2 n) in  
       (* Get the update cost between the vertices. *)
       let cw = CostTable.get update_costs x y in
       (* The conditional move is the forced move, reversed: it should cost cm if there exists n' in 
          C(n) s.t. m'n' is an edge (where m' is in C(m)).
          /!\ This is not the conditional move defined in the paper : indeed, in the paper, the 
          conditional move is defined as C-MF(m', n) = 0 if there exists n' in C(n) s.t. m'n' is an
          edge, BUT we think it is a mistake as the forced cost is defined similarly. Furthermore,
          it indeed makes the upper-bound decrease & lower-bound increase. *)
       let cm1 = Cost.f_conditional_move (forced_move_left t2 graph) in
       let cm2 = Cost.f_conditional_move (forced_move_right t1 graph) in
       (* Computing the upper bound cost needs the total number of edges of a vertex. *)
       let out_deg = fun m -> G.out_degree graph (N.mk m) in
       Cost.upper_bound out_deg c1 c2 cm1 cm2 cw m n
  
  (* Inits the min-heap at node x. *)
  let init_min_heap (x: G.V.t) (succ: G.V.t list) (update_costs: cost_table) (t1: I.t) (t2: I.t)
        (graph: G.t) (reverse: bool) : heap =
    let heap = UpperBoundHeap.create (List.length succ) in
    List.iter
      (fun y -> let ub_cost =
                  (* We have to be careful here as the upper bound needs x to be an element of t1
                     and y to be an element of t2. *)
                  if reverse then (compute_upper_bound y x update_costs t1 t2 graph)
                  else (compute_upper_bound x y update_costs t1 t2 graph) in
                UpperBoundHeap.add heap y ub_cost)
      succ;
    (heap)

  (* Computation of all the initial upper-bounds for each edge and storage in a min-heap
     for each vertex. *)
  let init_upper_bounds (t1: I.t) (t2: I.t) (graph: G.t) (update_costs: cost_table) : ub_table =
    let table = Hashtbl.create (G.nb_vertex graph) in
    let iterate =
      (fun reverse v -> let succ = G.succ graph v in
                        Hashtbl.add table v (init_min_heap v succ update_costs t1 t2 graph reverse)) in
    List.iter (iterate false) (t1_vertices t1);
    List.iter (iterate true) (t2_vertices t2);
    (table)

  (* Removal of the edge [mn] in the graph and in the min-heaps of [m] and [n]. *)
  let remove_edge (m: G.V.t) (n: G.V.t) (ub_table: ub_table) (graph: G.t) : unit =
    let m_heap = Hashtbl.find ub_table m in
    let n_heap = Hashtbl.find ub_table n in
    UpperBoundHeap.remove m_heap n;
    UpperBoundHeap.remove n_heap m;
    G.remove_edge graph m n;
    ()
         
  (* First pruning rule. *)
  let is_prunable_1 (m: G.V.t) (n: G.V.t) (lb: Cost.t) (ub_table: ub_table) : bool =
    let ub1 = UpperBoundHeap.top (Hashtbl.find ub_table m) in
    let ub2 = UpperBoundHeap.top (Hashtbl.find ub_table n) in
    Cost.prune_rule_1 lb ub1 ub2

  (* Second pruning rule. *)
  let is_prunable_2 = Cost.prune_rule_2

  (* Tries to prune an edge [m,n]. If it succeeds, tries to prune the updated edges. *)
  let try_prune (m: G.V.t) (n: G.V.t) (update_costs: cost_table) (ub_table: ub_table)
        (t1: I.t) (t2: I.t) (graph: G.t) : bool =
    if (not (G.mem_edge graph m n)) then false
    else
      let lb_cost = lower_bound update_costs graph t1 t2 m n in
      if ((is_prunable_1 m n lb_cost ub_table) || (is_prunable_2 lb_cost)) then
        (remove_edge m n ub_table graph; true)
      else false

  (* Iterate the pruning on elements of t1.
     By doing it like this, we *assure* that m is gonna be in t1 and n is gonna be in t2.
     It is important for some parts of the algorithm. *)
  let run_pruning (t1: I.t) (t2: I.t) (graph: G.t) (update_costs: cost_table)
        (ub_table: ub_table) : bool  =
    let pruned = ref false in
    List.iter
      (fun m -> G.iter_succ
                  (fun n -> pruned := !pruned || (try_prune m n update_costs ub_table t1 t2 graph))
                  graph m)
      (N.plus() :: (transform (I.elements t1)));
    !pruned

  (* Creates a graph that has the right labels on each edge. *)
  let update_labels (t1: I.t) (t2: I.t) (graph: G.t) (update_costs: cost_table) : G.t =
    let mk_edge = fun m n -> G.E.create m (lower_bound update_costs graph t1 t2 m n) n in
    let x = t1_vertices t1 in
    let y = t2_vertices t2 in
    let g = G.create ~size:((List.length x) + (List.length y) + 2) () in
    List.iter (G.add_vertex g) x;
    List.iter (G.add_vertex g) y;
    List.iter
      (fun m -> (List.iter (fun n -> if G.mem_edge graph m n then G.add_edge_e g (mk_edge m n)) y))
      x;
    (g)
  
  let try_remove_edge_left (graph: G.t) (t1: I.t) (t2: I.t) (update: cost_table) (m: G.V.t) : unit =
    match m with
    | N.Plus -> ()
    | N.Original _ ->
       if List.exists
            (fun n ->
              if n = (N.minus()) then false
              else (not (G.mem_edge graph (N.plus()) n)) ||
                     (Cost.compare (lower_bound update graph t1 t2 m n) (Cost.lb_cd())) <= 0)
            (G.succ graph m)
       then G.remove_edge graph (N.minus()) m
       else ()
    | _ -> failwith "Minus node cannot link with itself"

  let try_remove_edge_right (graph: G.t) (t1: I.t) (t2: I.t) (update: cost_table) (n: G.V.t) : unit =
    match n with
    | N.Minus -> ()
    | N.Original _ ->
       if List.exists
            (fun m ->
              if m = (N.plus()) then false
              else (not (G.mem_edge graph (N.minus()) m)) ||
                     (Cost.compare (lower_bound update graph t1 t2 m n) (Cost.lb_ci())) <= 0)
            (G.succ graph n)
       then G.remove_edge graph (N.plus()) n
       else ()
    | _ -> failwith "Plus node cannot link with itself"

  (* Remove the edges to (+) and (-) if there exists a lower bound that is better than deletion+insertion. *)
  let prune_useless_plus_minus (graph: G.t) (t1: I.t) (t2: I.t) (update: cost_table) : G.t =
    List.iter (try_remove_edge_left graph t1 t2 update) (G.succ graph (N.minus()));
    List.iter (try_remove_edge_right graph t1 t2 update) (G.succ graph (N.plus()));
    List.iter (try_remove_edge_left graph t1 t2 update) (G.succ graph (N.minus()));
    graph

  (* Main function of pruning. *)
  let prune (t1: I.t) (t2: I.t) (graph: G.t) : G.t =
    let update_costs = compute_update_costs t1 t2 (G.nb_edges graph) in
    let rec aux (graph: G.t) =
      (* TODO: memoize this? *)
      let upper_bound_costs = init_upper_bounds t1 t2 graph update_costs in
      let pruned = run_pruning t1 t2 graph update_costs upper_bound_costs in
      let graph' = update_labels t1 t2 graph update_costs in
      if pruned then aux graph'
      else graph'
    in prune_useless_plus_minus (aux graph) t1 t2 update_costs
end
