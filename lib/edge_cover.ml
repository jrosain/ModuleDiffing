module Make(I: Sig.INPUT)(N: Sig.Node with module Input = I)(G: Sig.G with type V.label = N.t and type E.label = Cost.t)
= struct
  module HM = Hungarian_method.Make(G)

  let mk_dummy (n : int) : G.vertex =
    G.V.create (N.Dummy n)

  let new_graph_from_mates (graph : G.t) (covering : HM.covering) : G.t =
    let new_graph = G.create () in
    G.iter_vertex (G.add_vertex new_graph) graph;
    G.iter_vertex (fun v ->
      let u = (Hashtbl.find covering v) in
      match G.V.label u with
      | N.Dummy _ -> () (* Nothing to do, this is a false edge added because the hungarian method need a complete graph *)
      | _ -> G.add_edge new_graph v u
    ) graph;
    (* TODO: The line above will add uv and vu, maybe its not correct. *)
    new_graph


  (* graph is supposed to be bipartite *)
  let bipartite_min_edge_cover (graph : G.t) : G.t = 
    let complete_graph = HM.make_graph_balanced_complete graph mk_dummy in
    let covering = HM.make complete_graph in
    let graph_covered = new_graph_from_mates graph covering in
    graph_covered
end
  