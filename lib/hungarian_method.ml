(* Implementation of the hungarian algorithm taken here 
   https://homes.di.unimi.it/righini/Didattica/OttimizzazioneCombinatoria/MaterialeOC/11%20-%20Min%20cost%20bipartite%20matching.pdf *)
module Make(G: Sig.G with type E.label = Cost.t)
= struct
  module VertexSet = Set.Make(G.V)

  (* This is the result of the algorithm, the edge cover, it has the property cover(i) = j <=> cover(j) = i *)
  type covering = (G.vertex, G.vertex) Hashtbl.t

  (* bipartite set of a graph *)
  type bip = (G.vertex list)
  (* variables x_ij and their value *)
  type primal_vars = ((G.vertex * G.vertex), int) Hashtbl.t
  (* variables u_i and v_j and their value *)
  type dual_vars = (G.vertex, Cost.t) Hashtbl.t
  (* mate for a vertex (= its matching), it needs to have the property mate(i) = j <=> mate(j) = i *)
  type mates = (G.vertex, G.vertex option) Hashtbl.t
  (* type to manage source, target or any other vertex of the graph *)
  type bip_node = S (* | T *) | V of G.vertex (* T represents the target node, but its never used *)
  (* labels of a vertex *)
  type labels = (G.vertex, bip_node option) Hashtbl.t
  (* set of labels to be used to generate others *)
  type l_set = VertexSet.t ref
  (* labels of a vertex *)
  type path_costs = (G.vertex, Cost.t) Hashtbl.t
  (* labels of a vertex *)
  type path_preds = (G.vertex, G.vertex option) Hashtbl.t
  (* the path *)
  type path = (bip_node option) ref

  (* The given graph is considered bipartite. O(n) *)
  let get_bipartite (graph : G.t) : (bip * bip) =
    let colored = (Hashtbl.create 1) in
    let diff = ref 0 in

    (* Colors the graph until every node is colored, this is why we have a Hashtbl: to keep information. O(n) *)
    let rec bipartite_rec (actual_node : G.vertex) (actual_color : int) : unit =
      G.iter_succ (fun v ->
        match Hashtbl.find_opt colored v with
        | Some _ -> ()
        | None ->
          diff := !diff + actual_color;
          Hashtbl.add colored v actual_color;
          bipartite_rec v (actual_color * -1)
      ) graph actual_node
    in

    (* We take one vertex to be the root of the DFS, maybe there exist better methods in O(1), this is in O(n) *)
    G.iter_vertex (fun v ->
      match (Hashtbl.find_opt colored v) with
      | Some _ -> ()
      | None -> bipartite_rec v (if !diff < 0 then 1 else -1)
    ) graph;

    (* Create the two bipartite lists from the Hashtbl. O(n) *)
    Hashtbl.fold (fun node color bips ->
      let bip1, bip2 = bips in 
      match color with
      | -1 -> (node::bip1, bip2) 
      | 1  -> (bip1, node::bip2)
      | _  -> failwith "5Internal error"
    ) colored ([], [])

  (* Step 1: O(n²) *)
  (* create the init feasible dual solution *)
  let dual_init (graph : G.t) (bip1 : bip) (bip2 : bip) : (dual_vars * dual_vars) =
    let u = (Hashtbl.create 1) in
    let v = (Hashtbl.create 1) in

    (* Init ui for every node i of the first set of the bipartite *)
    List.iter (fun i ->
      let min_i = G.fold_succ (fun j min_cost ->
        let edge_ij = G.find_edge graph i j in
        let cost = G.E.label edge_ij in
        if Cost.compare cost min_cost < 0 then cost else min_cost
      ) graph i Cost.max in
      Hashtbl.add u i min_i;
    ) bip1;

    (* Init vj for every node j of the second set of the bipartite *)
    List.iter (fun j ->
      let min_j = G.fold_succ (fun i min_cost ->
        let edge_ij = G.find_edge graph i j in
        let cost_edge = G.E.label edge_ij in
        let ui = Hashtbl.find u i in
        let cost = Cost.sub cost_edge ui in 
        if Cost.compare cost min_cost < 0 then cost else min_cost
      ) graph j Cost.max in
      Hashtbl.add v j min_j;
    ) bip2;

    (u, v)

  (* Step 2: O(n²) *)
  (* return the init unfeasible primal solution *)
  let primal_init (graph : G.t) (bip1 : bip) (bip2 : bip) (u : dual_vars) (v : dual_vars) : (primal_vars * mates * int) =
    let mate = (Hashtbl.create 1) in
    let x = (Hashtbl.create 1) in
    let card = ref 0 in
    List.iter(fun i -> Hashtbl.add mate i None) bip1;
    List.iter(fun i -> Hashtbl.add mate i None) bip2;
    List.iter(fun i ->
      List.iter(fun j ->
        Hashtbl.add x (i, j) 0;
        let edge_ij = G.find_edge graph i j in
        let cij = G.E.label edge_ij in
        let ui = Hashtbl.find u i in
        let vj = Hashtbl.find v j in
        let first_cond = (Cost.sub (Cost.sub cij ui) vj) = Cost.null in
        let second_cond = (Hashtbl.find mate i) = None in
        let third_cond = (Hashtbl.find mate j) = None in
        if (first_cond && second_cond && third_cond) then begin
          Hashtbl.replace x (i, j) 1;
          card := !card + 1;
          Hashtbl.replace mate i (Some j);
          Hashtbl.replace mate j (Some i);
        end
      ) bip2  
    ) bip1;
    (x, mate, !card)

  (* Step 3.1: O(n²) *)
  (* return all we need for the path  *)
  let path_initialization (graph : G.t) (bip1 : bip) (bip2 : bip) (mate : mates) (u : dual_vars) (v : dual_vars) : (l_set * labels * path_costs * path_preds) =
    let l = ref (VertexSet.empty) in
    let label = (Hashtbl.create 1) in
    let p = (Hashtbl.create 1) in
    let pi = (Hashtbl.create 1) in
    List.iter(fun k -> Hashtbl.add label k None) bip1;
    List.iter(fun k -> Hashtbl.add label k None) bip2;
    List.iter(fun j ->
      Hashtbl.add p j Cost.max;
      Hashtbl.add pi j None
    ) bip2;
    List.iter(fun i ->
      if (Hashtbl.find mate i) = None then begin
        Hashtbl.replace label i (Some S);
        l := VertexSet.add i !l;
        List.iter(fun j ->
          (* I don't understand this condition since all Label[j] = None at this point *)
          if (Hashtbl.find label j) = None then begin
            let edge_ij = G.find_edge graph i j in
            let cij = G.E.label edge_ij in
            let ui = Hashtbl.find u i in
            let vj = Hashtbl.find v j in
            let cost = (Cost.sub (Cost.sub cij ui) vj) in
            let pj = Hashtbl.find p j in
            if Cost.compare cost pj < 0 then begin
              Hashtbl.replace p j cost;
              Hashtbl.replace pi j (Some i)
            end
          end
        ) bip2;
      end 
    ) bip1;
    (l, label, p, pi)

  (* Step 3.2: O(n) *)
  let label_propagation (graph : G.t) (bip1 : bip) (bip2 : bip) (mate : mates) (u : dual_vars) (v : dual_vars) (l : l_set) (label : labels) (p : path_costs) (pi : path_preds) (path : path) : unit =
    let k = VertexSet.choose !l in
    l := VertexSet.remove k !l;
    if (List.mem k bip1) then begin
      List.iter(fun j ->
        let edge_kj = G.find_edge graph k j in
        let ckj = G.E.label edge_kj in
        let uk = Hashtbl.find u k in
        let vj = Hashtbl.find v j in
        let cost = (Cost.sub (Cost.sub ckj uk) vj) in
        let first_cond = (Cost.compare cost Cost.null) = 0 in
        let second_cond = (Hashtbl.find label j) = None in
        if first_cond && second_cond then begin
          Hashtbl.replace label j (Some (V k));
          l := VertexSet.add j !l
        end
      ) bip2
    end
    else begin
      let matek = Hashtbl.find mate k in
      match matek with
      | None -> path := Some (V k)
      | Some matek -> begin
        if (Hashtbl.find label matek) = None then begin
          Hashtbl.replace label matek (Some (V k));
          l := VertexSet.add matek !l;
          List.iter(fun j ->
            let edge_mkj = G.find_edge graph matek j in
            let cmkj = G.E.label edge_mkj in
            let umk = Hashtbl.find u matek in
            let vj = Hashtbl.find v j in
            let cost = (Cost.sub (Cost.sub cmkj umk) vj) in
            let pj = Hashtbl.find p j in
            let first_cond = (Cost.compare cost pj) < 0 in
            let second_cond = (Hashtbl.find label j) = None in
            if first_cond && second_cond then begin
              Hashtbl.replace p j cost;
              Hashtbl.replace pi j (Some matek)
            end
          ) bip2
        end
      end
    end

  (* Step 4: O(n) *)
  let dual_iteration (bip1 : bip) (bip2 : bip) (u : dual_vars) (v : dual_vars) (l : l_set) (label : labels) (p : path_costs) (pi : path_preds) : unit =
    let delta = List.fold_left (fun min j ->
      match (Hashtbl.find label j) with
      | Some _ -> min
      | None ->
        let pj = Hashtbl.find p j in
        if (Cost.compare pj min) < 0 then pj else min
    ) Cost.max bip2 in
    List.iter (fun i ->
      if (Hashtbl.find label i) <> None then begin
        let ui = Hashtbl.find u i in
        let new_ui = Cost.add ui delta in
        Hashtbl.replace u i new_ui
      end
    ) bip1;
    List.iter (fun j ->
      if (Hashtbl.find label j) <> None then begin
        let vj = Hashtbl.find v j in
        let new_vj = Cost.sub vj delta in
        Hashtbl.replace v j new_vj
      end
    ) bip2;
    List.iter (fun j ->
      if (Hashtbl.find label j) = None then begin
        let pj = Hashtbl.find p j in
        let new_pj = Cost.sub pj delta in
        Hashtbl.replace p j new_pj
      end
    ) bip2;
    List.iter (fun j ->
      if (Hashtbl.find label j) = None then begin
        let pj = Hashtbl.find p j in
        if (Cost.compare pj Cost.null) = 0 then begin
          match Hashtbl.find pi j with
          | Some pij ->
            Hashtbl.replace label j (Some (V pij));
            l := VertexSet.add j !l
          | None -> failwith "4Internal error"
        end
      end
    ) bip2
    
  (* Step 5: O(n) *)
  let primal_iteration (mate : mates) (label : labels) (x : primal_vars) (path : path) (card : int) : int =
    let rec repeat j =
      let i = match Hashtbl.find label j with
      | (Some (V i')) -> i'
      | _ -> failwith "3Internal error"
      in

      Hashtbl.replace mate i (Some j);
      Hashtbl.replace mate j (Some i);
      Hashtbl.replace x (i, j) 1;

      match Hashtbl.find label i with
      | (Some (V j)) ->
        Hashtbl.replace x (i, j) 0;
        repeat j
      | (Some S) -> card + 1 (* I didn't increment and decrement card each time since we know we will just ended up incremented by 1*)
      | _ -> failwith "2Internal error"
    in
    match !path with
    | (Some (V j)) -> repeat j
    | _ -> failwith "1Internal error"

  
  (********************)
  (* PUBLIC FUNCTIONS *)
  (********************)

  (* main function: O(n³)
    The graph is supposed to be complete and balanced.
    To be sure your graph is complete and balanced, call `make_graph_balanced_complete` before
  *)
  let make (graph : G.t) : covering =
    let bip1, bip2 = get_bipartite graph in                 (* O(n) *)
    let u, v = dual_init graph bip1 bip2 in                 (* O(n²) *)
    let x, mate, card = primal_init graph bip1 bip2 u v in  (* O(n²) *)
    let n = List.length bip1 in

    let rec while_feasible (card : int) : unit =
      (* we stop when the primal is feasible, it means that every nodes are matched *)
      if card <> n then begin
        let l, label, p, pi = path_initialization graph bip1 bip2 mate u v in
        let path = ref None in

        let rec while_path_nil () =
          if !path = None then begin
            let rec while_path_nil_L () =
              if !path = None && not (VertexSet.is_empty !l) then begin
                label_propagation graph bip1 bip2 mate u v l label p pi path;
                while_path_nil_L ()
              end
            in while_path_nil_L ();
            if !path = None then begin
              dual_iteration bip1 bip2 u v l label p pi;
            end;
            while_path_nil ()
          end
        in while_path_nil ();
        let new_card = primal_iteration mate label x path card in
        while_feasible new_card
      end
    in
    while_feasible card;

    (* Create an easily usable hashtable for the edge cover *)
    let covering = Hashtbl.create 1 in
    List.iter (fun i ->
      match Hashtbl.find mate i with
      | Some j ->
        Hashtbl.replace covering i j;
        Hashtbl.replace covering j i
      | None -> failwith "0Internal error"
    ) bip1;
    covering

  (* O(n²) make the bipartite graph balanced and complete *)
  let make_graph_balanced_complete (graph : G.t) (mk_dummy : int -> G.vertex) : G.t =
    let bip1, bip2 = get_bipartite graph in
    let new_graph = G.create () in
    let rec fill_bip bip n =
      if n <= 0 then bip
      else begin
        let new_vertex = mk_dummy n in
        new_vertex::(fill_bip bip (n-1))
      end
    in 
    let diff = (List.length bip1) - (List.length bip2) in
    let bip1 = fill_bip bip1 (-diff) in
    let bip2 = fill_bip bip2 diff in
    List.iter (G.add_vertex new_graph) bip1;
    List.iter (G.add_vertex new_graph) bip2;
    List.iter (fun x ->
      List.iter (fun y ->
        match G.mem_edge graph x y with
        | true -> G.add_edge_e new_graph (G.find_edge graph x y)
        | false ->
          (* add an edge with maximum cost if no edge *)
          (* the max is divided by 2 because in the dual iteration, we add and sub delta from u(i), v(j) and p(j)
             when the graph possesses singletons it is problematic, we could remove these singleton or we could say that
             the input graph needs to be connected, but I don't have the time to do that *)
          let max = (Cost.of_int ((Cost.to_int Cost.max) / 2)) in
          let max_cost_edge = G.E.create x max y in
          G.add_edge_e new_graph max_cost_edge
      ) bip2
    ) bip1;
    new_graph
end