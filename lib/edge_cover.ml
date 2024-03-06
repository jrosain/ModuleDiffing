module type Node = sig
  type t
    
  (* The copy should not be in the graph (it should be addable) *)
  val mk_copy : t -> t
end

(* Edge Cover problem describes in theorem 7.4 here: 
   https://doc.lagout.org/science/0_Computer%20Science/2_Algorithms/Algorithms%20and%20Theory%20of%20Computation%20Handbook%20%5BAtallah%201998-11-23%5D.pdf *)
module Make(N : Node)(G: Sig.G with type V.t = N.t with type E.label = Cost.t)
= struct
  module HM = Hungarian_method.Make(G)

  let new_graph_from_mates (graph : G.t) (covering : HM.covering) : G.t =
    let new_graph = G.create () in
    G.iter_vertex (G.add_vertex new_graph) graph;
    G.iter_vertex (fun v ->
      let u = (Hashtbl.find covering v) in
      match G.mem_vertex graph u with
      (* We keep edges from the initial graph *)
      | true  -> G.add_edge_e new_graph (G.find_edge graph u v)
      (* If the second node was on the copy, we pick the smallest edge to cover this vertex (both edges have the same cost) *)
      | false ->
        begin
          let edge = G.fold_succ_e (fun e acc ->
            match acc with
            | None -> Some e
            | Some e' -> if Cost.compare (G.E.label e') (G.E.label e) < 0 then (Some e') else (Some e)
          ) graph v None in
          match edge with
          | None -> failwith "Internal error"
          | Some edge -> G.add_edge_e new_graph edge
        end
    ) graph;
    new_graph
  
  let create_edge_cover_graph (graph : G.t) : G.t =
    (* Affiliate one node to its copy *)
    let copy = Hashtbl.create 1 in
    G.iter_vertex (fun v -> Hashtbl.add copy v (N.mk_copy v)) graph;

    (* Create the resulting graph copy *)
    let new_graph = G.create () in
    G.iter_vertex (fun v ->
      let u = (Hashtbl.find copy v) in
      G.add_vertex new_graph v;
      G.add_vertex new_graph u;
      let link_cost = G.fold_succ (fun sv min_cost ->
        let su = (Hashtbl.find copy sv) in
        let cost = G.E.label (G.find_edge graph v sv) in
        let cost_copy = (Cost.of_int 0) in
        let edge = G.E.create v cost sv in
        let edge_copy = G.E.create u cost_copy su in
        G.add_edge_e new_graph edge;
        G.add_edge_e new_graph edge_copy;
        if Cost.compare cost min_cost < 0 then cost else min_cost
      ) graph v (Cost.max) in
      let edge_link = G.E.create u link_cost v in
      G.add_edge_e new_graph edge_link
    ) graph;
    new_graph


  (* graph is supposed to be bipartite *)
  let bipartite_min_edge_cover (graph : G.t) : G.t = 
    let edge_cover_graph = create_edge_cover_graph graph in
    let complete_graph = HM.make_graph_balanced_complete edge_cover_graph (fun _ -> failwith "Internal error") in
    let covering = HM.make complete_graph in
    let graph_covered = new_graph_from_mates graph covering in
    graph_covered
end
  