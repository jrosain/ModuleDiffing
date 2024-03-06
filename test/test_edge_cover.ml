open Alcotest
open ModuleDiffing

(* ---------------------------------------------------------------------------------------------- *)
(* SETUP *)

module Node = struct
  type t = int
  let mk_copy v = v + 1000

  let compare = Stdlib.compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module G = Graph.Imperative.Graph.ConcreteLabeled(Node)(Sig.Edge)
module EC = Edge_cover.Make(Node)(G)

(* ---------------------------------------------------------------------------------------------- *)
(* SAMPLES TO TEST AND RESULT EXPECTED *)

let simple_example () =
  let g = G.create () in
  let _ = G.add_vertex g (G.V.create 1) in
  let _ = G.add_vertex g (G.V.create 2) in
  let _ = G.add_vertex g (G.V.create 3) in
  let _ = G.add_vertex g (G.V.create 4) in
  let _ = G.add_vertex g (G.V.create 5) in
  let _ = G.add_edge_e g (G.E.create 1 (Cost.of_int 1) 2) in 
  let _ = G.add_edge_e g (G.E.create 1 (Cost.of_int 1) 4) in 
  let _ = G.add_edge_e g (G.E.create 2 (Cost.of_int 2) 3) in 
  let _ = G.add_edge_e g (G.E.create 2 (Cost.of_int 2) 4) in 
  let _ = G.add_edge_e g (G.E.create 2 (Cost.of_int 2) 5) in 
  g

let bipartite_simple_example () =
  let g = G.create () in
  let _ = G.add_vertex g (G.V.create 1) in
  let _ = G.add_vertex g (G.V.create 2) in
  let _ = G.add_vertex g (G.V.create 3) in
  let _ = G.add_vertex g (G.V.create 4) in
  let _ = G.add_vertex g (G.V.create 5) in
  let _ = G.add_edge_e g (G.E.create 2 (Cost.of_int 1) 1) in 
  let _ = G.add_edge_e g (G.E.create 2 (Cost.of_int 2) 3) in 
  let _ = G.add_edge_e g (G.E.create 2 (Cost.of_int 2) 5) in 
  let _ = G.add_edge_e g (G.E.create 4 (Cost.of_int 1) 3) in 
  let _ = G.add_edge_e g (G.E.create 4 (Cost.of_int 1) 5) in 
  g

(* ---------------------------------------------------------------------------------------------- *)
(* TESTS *)

let test_edge_cover_graph () =
  let g = EC.create_edge_cover_graph (simple_example ()) in
  Alcotest.(check int)
    ("Number of vertices in edge cover graph")
    (10)
    (G.nb_vertex g);
  Alcotest.(check int)
    ("Number of edges in edge cover graph")
    (15)
    (G.nb_edges g);
  let check_edge u v c =
    let u = (G.V.create u) in
    let v = (G.V.create v) in
    let e = try G.find_edge g u v with
      | _ -> Alcotest.fail "Two nodes don't have edge in the edge cover graph"
    in
    Alcotest.(check int)
      ("Cost of edge")
      (c)
      (Cost.to_int (G.E.label e))
  in
  check_edge 1 4 1;
  check_edge 2 1 1;
  check_edge 2 3 2;
  check_edge 2 4 2;
  check_edge 2 5 2;
  check_edge 1001 1004 0;
  check_edge 1002 1001 0;
  check_edge 1002 1003 0;
  check_edge 1002 1004 0;
  check_edge 1002 1005 0;
  check_edge 1001 1 1;
  check_edge 1002 2 1;
  check_edge 1003 3 2;
  check_edge 1004 4 1;
  check_edge 1005 5 2

let test_bipartite_min_edge_cover () =
  let result = EC.bipartite_min_edge_cover (bipartite_simple_example ()) in
  Alcotest.(check int)
    ("Number of match in bipartite cover")
    (3)
    (G.nb_edges result);
  let check_edge u v c =
    let u = (G.V.create u) in
    let v = (G.V.create v) in
    let e = try G.find_edge result u v with
      | _ -> Alcotest.fail "Two nodes don't have edge in the bipartite cover"
    in
    Alcotest.(check int)
      ("Cost of edge")
      (c)
      (Cost.to_int (G.E.label e))
  in
  check_edge 1 2 1;
  check_edge 3 4 1;
  check_edge 5 4 1

let () =
  Alcotest.run "Edge cover" [
    "edge_cover_graph", [
      test_case "edge_cover_graph" `Quick test_edge_cover_graph;
    ];
    "bipartite_edge_cover", [
      test_case "bipartite_min_edge_cover" `Quick test_bipartite_min_edge_cover;
    ];
  ]

