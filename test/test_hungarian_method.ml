open Alcotest
open ModuleDiffing

(* ---------------------------------------------------------------------------------------------- *)
(* SETUP *)

module Node = struct
  type t = int

  let compare = Stdlib.compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module G = Graph.Imperative.Graph.ConcreteLabeled(Node)(Sig.Edge)
module HM = Hungarian_method.Make(G)

(* Consider our graph test wont have more thant 1000 nodes *)
let mk_dummy (n : int) : G.V.t =
  G.V.create (1000 + n)

(* ---------------------------------------------------------------------------------------------- *)
(* CHECK TYPES *)

(* List of vertices *)
let vertex_list_pp ppf vl =
  let s = List.fold_left (fun acc v ->
    String.concat "," [acc; string_of_int (G.V.label v)]
  ) "" vl in
  Fmt.pf ppf "[%s]" s

let vertex_list_equal (la: G.vertex list) (lb: G.vertex list) : bool =
  let a_in_b = List.fold_left (fun acc va ->
    acc && List.fold_left (fun acc vb ->
      acc || ((G.V.label va) = (G.V.label vb))
    ) false lb
  ) true la in
  let b_in_a = List.fold_left (fun acc vb ->
    acc && List.fold_left (fun acc va ->
      acc || ((G.V.label va) = (G.V.label vb))
    ) false la
  ) true lb in
  a_in_b && b_in_a

let vertex_list_t = Alcotest.testable vertex_list_pp vertex_list_equal

(* Vertex *)
let vertex_pp ppf v = Fmt.pf ppf "Node(%d)" (G.V.label v)
let vertex_equal (va: G.V.t) (vb: G.V.t) : bool = (G.V.label va) = (G.V.label vb)
let vertex_t = Alcotest.testable vertex_pp vertex_equal

(* Vertex option *)
let vertex_opt_pp ppf v =
  match v with
  | None -> Fmt.pf ppf "Node (opt) None"
  | Some v -> Fmt.pf ppf "Node (opt) Some(%d)" (G.V.label v)
let vertex_opt_equal (va: G.V.t option) (vb: G.V.t option) : bool =
  match va, vb with
  | None, None -> true
  | Some va, Some vb -> (G.V.label va) = (G.V.label vb)
  | _ -> false
let vertex_opt_t = Alcotest.testable vertex_opt_pp vertex_opt_equal

(* Node or S or T *)
let bip_node_opt_pp ppf v =
  match v with
  | None -> Fmt.pf ppf "Bip Node (opt) None"
  | Some HM.S -> Fmt.pf ppf "Bip Node (opt) Source"
  | Some (HM.V v) -> Fmt.pf ppf "Bip Node (opt) Some(%d)" (G.V.label v)
let bip_node_opt_equal (va: HM.bip_node option) (vb: HM.bip_node option) : bool =
  match va, vb with
  | None, None -> true
  | Some HM.S, Some HM.S -> true
  | Some (HM.V va), Some (HM.V vb) -> (G.V.label va) = (G.V.label vb)
  | _ -> false
let bip_node_opt_t = Alcotest.testable bip_node_opt_pp bip_node_opt_equal

(* Costs *)
let cost_pp ppf c = Fmt.pf ppf "Cost(%d)" (Cost.to_int c)
let cost_equal (ca: Cost.t) (cb: Cost.t) : bool = ((Cost.compare ca cb) = 0)
let cost_t = Alcotest.testable cost_pp cost_equal

(* ---------------------------------------------------------------------------------------------- *)
(* SAMPLES TO TEST AND RESULT EXPECTED *)

let simple_bipartite () =
  let g = G.create () in
  let _ = G.add_vertex g (G.V.create 1) in
  let _ = G.add_vertex g (G.V.create 2) in
  let _ = G.add_vertex g (G.V.create 3) in
  let _ = G.add_vertex g (G.V.create 4) in
  let _ = G.add_vertex g (G.V.create 5) in
  let _ = G.add_edge g 1 2 in 
  let _ = G.add_edge g 1 3 in 
  let _ = G.add_edge g 1 4 in 
  let _ = G.add_edge g 5 2 in 
  let _ = G.add_edge g 5 3 in 
  g

let simple_bip1 () = [(G.V.create 1); (G.V.create 5)]
let simple_bip2 () = [(G.V.create 2); (G.V.create 3); (G.V.create 4)]

(* from here https://homes.di.unimi.it/righini/Didattica/OttimizzazioneCombinatoria/MaterialeOC/11%20-%20Min%20cost%20bipartite%20matching.pdf *)
let lecture_example () =
  let g = G.create () in
  let _ = G.add_vertex g (G.V.create 1) in
  let _ = G.add_vertex g (G.V.create 2) in
  let _ = G.add_vertex g (G.V.create 3) in
  let _ = G.add_vertex g (G.V.create 4) in
  let _ = G.add_vertex g (G.V.create 5) in
  let _ = G.add_vertex g (G.V.create 6) in
  let _ = G.add_vertex g (G.V.create 7) in
  let _ = G.add_vertex g (G.V.create 8) in
  let _ = G.add_edge_e g (G.E.create 1 (Cost.of_int 15) 5) in 
  let _ = G.add_edge_e g (G.E.create 1 (Cost.of_int 22) 6) in 
  let _ = G.add_edge_e g (G.E.create 1 (Cost.of_int 13) 7) in 
  let _ = G.add_edge_e g (G.E.create 1 (Cost.of_int 4) 8) in 
  let _ = G.add_edge_e g (G.E.create 2 (Cost.of_int 12) 5) in 
  let _ = G.add_edge_e g (G.E.create 2 (Cost.of_int 21) 6) in 
  let _ = G.add_edge_e g (G.E.create 2 (Cost.of_int 15) 7) in 
  let _ = G.add_edge_e g (G.E.create 2 (Cost.of_int 7) 8) in
  let _ = G.add_edge_e g (G.E.create 3 (Cost.of_int 16) 5) in 
  let _ = G.add_edge_e g (G.E.create 3 (Cost.of_int 20) 6) in 
  let _ = G.add_edge_e g (G.E.create 3 (Cost.of_int 22) 7) in 
  let _ = G.add_edge_e g (G.E.create 3 (Cost.of_int 6) 8) in
  let _ = G.add_edge_e g (G.E.create 4 (Cost.of_int 6) 5) in 
  let _ = G.add_edge_e g (G.E.create 4 (Cost.of_int 11) 6) in 
  let _ = G.add_edge_e g (G.E.create 4 (Cost.of_int 8) 7) in 
  let _ = G.add_edge_e g (G.E.create 4 (Cost.of_int 5) 8) in
  g

let lecture_bip1 () = [(G.V.create 1); (G.V.create 2); (G.V.create 3); (G.V.create 4)]
let lecture_bip2 () = [(G.V.create 5); (G.V.create 6); (G.V.create 7); (G.V.create 8)]

(* ---------------------------------------------------------------------------------------------- *)
(* GRAPH BIPARTITE FORM *)

(* Check if the two bipartite of a connected graph are good *)
let test_bipartite () = 
  (* simple *)
  let b1, b2 = HM.get_bipartite (simple_bipartite ()) in
  let expected1 = simple_bip1 () in
  let expected2 = simple_bip2 () in
  Alcotest.(check vertex_list_t)
    ("Expected bipartite")
    (expected1)
    (b1);
  Alcotest.(check vertex_list_t)
    ("Expected bipartite")
    (expected2)
    (b2);

  (* lecture test *)
  let b1, b2 = HM.get_bipartite (lecture_example ()) in
  let expected2 = lecture_bip1 () in
  let expected1 = lecture_bip2 () in
  Alcotest.(check vertex_list_t)
    ("Expected bipartite")
    (expected1)
    (b1);
  Alcotest.(check vertex_list_t)
    ("Expected bipartite")
    (expected2)
    (b2);

  (* empty graph *)
  let b1, b2 = HM.get_bipartite (G.create ()) in
  let expected2 = [] in
  let expected1 = [] in
  Alcotest.(check vertex_list_t)
    ("Expected bipartite")
    (expected1)
    (b1);
  Alcotest.(check vertex_list_t)
    ("Expected bipartite")
    (expected2)
    (b2)

(* Check the completeness of the graph *)
let test_bipartite_graph_complete () =
  (* simple *)
  let old_g = simple_bipartite () in
  let g = HM.make_graph_balanced_complete old_g mk_dummy in
  let expected_edge_cost x y c =
    let e = try G.find_edge g x y with
      | _ -> Alcotest.fail "Two nodes don't have edge in the complete graph"
    in
    Alcotest.(check cost_t)
      ("Expected cost")
      (c)
      (G.E.label e)
  in
  expected_edge_cost 1 2 Cost.null;
  expected_edge_cost 1 3 Cost.null;
  expected_edge_cost 1 4 Cost.null;
  expected_edge_cost 5 2 Cost.null;
  expected_edge_cost 5 3 Cost.null;
  expected_edge_cost 5 4 Cost.max;
  expected_edge_cost 1001 2 Cost.max;
  expected_edge_cost 1001 3 Cost.max;
  expected_edge_cost 1001 4 Cost.max;

  (* LThe old graph shouldn't be modyfied, it should create a copy *)
  Alcotest.(check bool)
    ("Expected not modifying graph")
    (false)
    (G.mem_vertex old_g (G.V.create 1001));


  (* The graph needs to be complete *)
  let b1, b2 = HM.get_bipartite g in
  let expected1 = (G.V.create 1001) :: (simple_bip1 ()) in
  let expected2 = simple_bip2 () in
  Alcotest.(check vertex_list_t)
    ("Expected bipartite")
    (expected1)
    (b1);
  Alcotest.(check vertex_list_t)
    ("Expected bipartite")
    (expected2)
    (b2);

  (* lecture test *)
  let g = HM.make_graph_balanced_complete (lecture_example ()) mk_dummy in
  let expected_edge_cost x y c =
    let e = try G.find_edge g x y with
      | _ -> Alcotest.fail "Two nodes don't have edge in the complete graph"
    in
    Alcotest.(check cost_t)
      ("Expected cost")
      (Cost.of_int c)
      (G.E.label e)
  in
  expected_edge_cost 1 5 15;
  expected_edge_cost 1 6 22;
  expected_edge_cost 1 7 13;
  expected_edge_cost 1 8 4;
  expected_edge_cost 2 5 12;
  expected_edge_cost 2 6 21;
  expected_edge_cost 2 7 15;
  expected_edge_cost 2 8 7;
  expected_edge_cost 3 5 16;
  expected_edge_cost 3 6 20;
  expected_edge_cost 3 7 22;
  expected_edge_cost 3 8 6;
  expected_edge_cost 4 5 6;
  expected_edge_cost 4 6 11;
  expected_edge_cost 4 7 8;
  expected_edge_cost 4 8 5;

  (* The graph was already complete *)
  let b1, b2 = HM.get_bipartite g in
  let expected1 = lecture_bip2 () in
  let expected2 = lecture_bip1 () in
  Alcotest.(check vertex_list_t)
    ("Expected bipartite")
    (expected1)
    (b1);
  Alcotest.(check vertex_list_t)
    ("Expected bipartite")
    (expected2)
    (b2)

(* ---------------------------------------------------------------------------------------------- *)
(* HUNGARIAN STEPS *)

let rec while_path_l g bip1 bip2 mate u v l label p pi path =
  if !path = None && not (HM.VertexSet.is_empty !l) then begin
    HM.label_propagation g bip1 bip2 mate u v l label p pi path;
    while_path_l g bip1 bip2 mate u v l label p pi path
  end

let if_path_nil bip1 bip2 u v l label p pi path =
  if !path = None then begin
    HM.dual_iteration bip1 bip2 u v l label p pi
  end

let rec while_path g bip1 bip2 mate u v l label p pi path =
  if !path = None then begin
    while_path_l g bip1 bip2 mate u v l label p pi path;
    if_path_nil bip1 bip2 u v l label p pi path;
    while_path g bip1 bip2 mate u v l label p pi path
  end

let rec while_feasible g bip1 bip2 u v x mate card =
  let n = List.length bip1 in
  if card <> n then begin
    let l, label, p, pi = HM.path_initialization g bip1 bip2 mate u v in
    let path = ref None in
    while_path g bip1 bip2 mate u v l label p pi path;
    let card = HM.primal_iteration mate label x path card in
    while_feasible g bip1 bip2 u v x mate card
  end

(* FUNCTIONS TO CALL FOR TESTS *)
let step_1 () =
  let g = lecture_example () in
  let bip1 = lecture_bip1 () in
  let bip2 = lecture_bip2 () in
  let u, v = HM.dual_init g bip1 bip2 in
  (g, bip1, bip2, u, v) 

let step_2 () =
  let g, bip1, bip2, u, v = step_1 () in
  let x, mate, card = HM.primal_init g bip1 bip2 u v in
  (g, bip1, bip2, u, v, x, mate, card)

let step_3_1 () =
  let g, bip1, bip2, u, v, x, mate, card = step_2 () in
  let l, label, p, pi = HM.path_initialization g bip1 bip2 mate u v in
  (g, bip1, bip2, u, v, x, mate, card, l, label, p, pi)

let step_3_2 () =
  let g, bip1, bip2, u, v, x, mate, card, l, label, p, pi = step_3_1 () in
  let path = ref None in
  while_path_l g bip1 bip2 mate u v l label p pi path;
  (g, bip1, bip2, u, v, x, mate, card, l, label, p, pi, path)

let step_4 () =
  let g, bip1, bip2, u, v, x, mate, card, l, label, p, pi, path = step_3_2 () in
  if_path_nil bip1 bip2 u v l label p pi path;
  (g, bip1, bip2, u, v, x, mate, card, l, label, p, pi, path)

let before_step_5 () =
  let g, bip1, bip2, u, v, x, mate, card, l, label, p, pi, path = step_4 () in
  while_path g bip1 bip2 mate u v l label p pi path;
  (g, bip1, bip2, u, v, x, mate, card, l, label, p, pi, path)

let step_5 () =
  let g, bip1, bip2, u, v, x, mate, card, l, label, p, pi, path = before_step_5 () in
  let card = HM.primal_iteration mate label x path card in
  (g, bip1, bip2, u, v, x, mate, card, l, label, p, pi, path)

let hung_algo () =
  let g, bip1, bip2, u, v, x, mate, card = step_2 () in
  while_feasible g bip1 bip2 u v x mate card;
  (u, v, x, mate)


(* Step 1 *)
let test_hung_step_1 () =
  let _, _, _, u, v = step_1 () in
  let expected_dual_val dual i c =
    let c' = try (Hashtbl.find dual (G.V.create i)) with
      | _ -> Alcotest.fail "A dual variable is not initiate in step 1"
    in
    Alcotest.(check cost_t)
      ("Expected value of dual variable in step 1")
      (Cost.of_int c)
      (c')
  in
  expected_dual_val u 1 4;
  expected_dual_val u 2 7;
  expected_dual_val u 3 6;
  expected_dual_val u 4 5;
  expected_dual_val v 5 1;
  expected_dual_val v 6 6;
  expected_dual_val v 7 3;
  expected_dual_val v 8 0

(* Step 2 *)
let test_hung_step_2 () =
  let _, _, _, _, _, x, mate, card = step_2 () in
  Alcotest.(check int)
    ("Expected cardinal")
    (2)
    (card);
  let expected_primal_val i j c =
    let c' = try (Hashtbl.find x ((G.V.create i), (G.V.create j))) with
      | _ -> Alcotest.fail "A primal variable is not initiate in step 2"
    in
    Alcotest.(check int)
      ("Expected value of primal variable in step 2")
      (c)
      (c')
  in
  expected_primal_val 1 5 0;
  expected_primal_val 1 6 0;
  expected_primal_val 1 7 0;
  expected_primal_val 1 8 1;
  expected_primal_val 2 5 0;
  expected_primal_val 2 6 0;
  expected_primal_val 2 7 0;
  expected_primal_val 2 8 0;
  expected_primal_val 3 5 0;
  expected_primal_val 3 6 0;
  expected_primal_val 3 7 0;
  expected_primal_val 3 8 0;
  expected_primal_val 4 5 1;
  expected_primal_val 4 6 0;
  expected_primal_val 4 7 0;
  expected_primal_val 4 8 0;
  let expected_mate i j =
    let expected = match j with
    | 0 -> None
    | j' -> Some (G.V.create j')
    in
    let node = try (Hashtbl.find mate (G.V.create i)) with
      | _ -> Alcotest.fail "A primal variable is not initiate in step 2"
    in
    Alcotest.(check vertex_opt_t)
      ("Expected mate")
      (expected)
      (node)
  in
  expected_mate 1 8;
  expected_mate 2 0;
  expected_mate 3 0;
  expected_mate 4 5;
  expected_mate 5 4;
  expected_mate 6 0;
  expected_mate 7 0;
  expected_mate 8 1
  
(* Step 3.1 *)
let test_hung_step_3_1 () =
  let _, _, _, _, _, _, _, _, l, label, p, pi = step_3_1 () in
  let expected_l i inside =
    let isinside = HM.VertexSet.find_opt (G.V.create i) !l in
    Alcotest.(check bool)
      ("Expected in L")
      (inside)
      (isinside <> None)
  in
  expected_l 1 false;
  expected_l 2 true;
  expected_l 3 true;
  expected_l 4 false;
  let expected_p i c =
    let c' = try (Hashtbl.find p (G.V.create i)) with
      | _ -> Alcotest.fail "A p is not initiate in step 3.1"
    in
    Alcotest.(check cost_t)
      ("Expected p")
      (Cost.of_int c)
      (c')
  in
  expected_p 5 4;
  expected_p 6 8;
  expected_p 7 5;
  expected_p 8 0;
  let expected_pi i j =
    let expected = match j with
    | 0 -> None
    | j' -> Some (G.V.create j')
    in
    let node_j = try (Hashtbl.find pi (G.V.create i)) with
      | _ -> Alcotest.fail "A pi is not initiate in step 3.1"
    in
    Alcotest.(check vertex_opt_t)
      ("Expected p")
      (expected)
      (node_j)
  in
  expected_pi 5 2;
  expected_pi 6 2;
  expected_pi 7 2;
  expected_pi 8 2;
  let expected_label i j =
    let expected = match j with
    | 0 -> None
    | _ -> Some HM.S
    in
    let node_j = try (Hashtbl.find label (G.V.create i)) with
      | _ -> Alcotest.fail "A label is not initiate in step 3.1"
    in
    Alcotest.(check bip_node_opt_t)
      ("Expected p")
      (expected)
      (node_j)
  in
  (* In init only vertices from S have a label to "S" the source vertex. And only vertices with no mates have a label. *)
  (* -1 = "S" vertex *)
  expected_label 1 0;
  expected_label 2 (-1);
  expected_label 3 (-1);
  expected_label 4 0;
  expected_label 5 0;
  expected_label 6 0;
  expected_label 7 0;
  expected_label 8 0

(* Step 3.2 *)
let test_hung_step_3_2 () =
  let _, _, _, _, _, _, _, _, l, label, _, _, _ = step_3_2 () in
  let empty = HM.VertexSet.is_empty !l in
  Alcotest.(check bool)
    ("Expected empty set end set 3.2")
    (true)
    (empty);
  let expected_label i j =
    let expected = match j with
    | 0 -> None
    | -1 -> Some HM.S
    | j' -> Some (HM.V (G.V.create j'))
    in
    let node_j = try (Hashtbl.find label (G.V.create i)) with
      | _ -> Alcotest.fail "A label is not initiate in step 3.2"
    in
    Alcotest.(check bip_node_opt_t)
      ("Expected p")
      (expected)
      (node_j)
  in
  (* -1 = "S" vertex *)
  expected_label 1 8;
  expected_label 2 (-1);
  expected_label 3 (-1);
  expected_label 4 0;
  expected_label 5 0;
  expected_label 6 0;
  expected_label 7 0;
  expected_label 8 2

(* Step 4 *)
let test_hung_step_4 () =
  let _, _, _, u, v, _, _, _, l, label, p, _, _ = step_4 () in
  let expected_dual_val dual i c =
    let c' = try (Hashtbl.find dual (G.V.create i)) with
      | _ -> Alcotest.fail "A dual variable is not initiate in step 1"
    in
    Alcotest.(check cost_t)
      ("Expected value of dual variable in step 1")
      (Cost.of_int c)
      (c')
  in
  expected_dual_val u 1 8;
  expected_dual_val u 2 11;
  expected_dual_val u 3 10;
  expected_dual_val u 4 5;
  expected_dual_val v 5 1;
  expected_dual_val v 6 6;
  expected_dual_val v 7 3;
  expected_dual_val v 8 (-4);
  let expected_p i c =
    let c' = try (Hashtbl.find p (G.V.create i)) with
      | _ -> Alcotest.fail "A p is not initiate in step 3.1"
    in
    Alcotest.(check cost_t)
      ("Expected p")
      (Cost.of_int c)
      (c')
  in
  expected_p 5 0;
  expected_p 6 4;
  expected_p 7 1;
  expected_p 8 0;
  let expected_l i inside =
    let isinside = HM.VertexSet.find_opt (G.V.create i) !l in
    Alcotest.(check bool)
      ("Expected in L")
      (inside)
      (isinside <> None)
  in
  expected_l 1 false;
  expected_l 2 false;
  expected_l 3 false;
  expected_l 4 false;
  expected_l 5 true;
  expected_l 6 false;
  expected_l 7 false;
  expected_l 8 false;
  let expected_label i j =
    let expected = match j with
    | 0 -> None
    | -1 -> Some HM.S
    | j' -> Some (HM.V (G.V.create j'))
    in
    let node_j = try (Hashtbl.find label (G.V.create i)) with
      | _ -> Alcotest.fail "A label is not initiate in step 3.2"
    in
    Alcotest.(check bip_node_opt_t)
      ("Expected p")
      (expected)
      (node_j)
  in
  (* -1 = "S" vertex *)
  expected_label 1 8;
  expected_label 2 (-1);
  expected_label 3 (-1);
  expected_label 4 0;
  expected_label 5 2;
  expected_label 6 0;
  expected_label 7 0;
  expected_label 8 2

(* Verify before Step 5 *)
let test_hung_before_step_5 () =
  let _, _, _, _, _, _, _, _, _, label, p, pi, path = before_step_5 () in
  let expected_p i c =
    let c' = try (Hashtbl.find p (G.V.create i)) with
      | _ -> Alcotest.fail "A p is not initiate in step 3.1"
    in
    Alcotest.(check cost_t)
      ("Expected p")
      (Cost.of_int c)
      (c')
  in
  expected_p 5 0;
  expected_p 6 0;
  expected_p 7 0;
  expected_p 8 0;
  let expected_pi i j =
    let expected = match j with
    | 0 -> None
    | j' -> Some (G.V.create j')
    in
    let node_j = try (Hashtbl.find pi (G.V.create i)) with
      | _ -> Alcotest.fail "A pi is not initiate in step 3.1"
    in
    Alcotest.(check vertex_opt_t)
      ("Expected p")
      (expected)
      (node_j)
  in
  expected_pi 5 2;
  expected_pi 6 4;
  expected_pi 7 4;
  expected_pi 8 2;
  let expected_label i j =
    let expected = match j with
    | 0 -> None
    | -1 -> Some HM.S
    | j' -> Some (HM.V (G.V.create j'))
    in
    let node_j = try (Hashtbl.find label (G.V.create i)) with
      | _ -> Alcotest.fail "A label is not initiate in step 3.2"
    in
    Alcotest.(check bip_node_opt_t)
      ("Expected p")
      (expected)
      (node_j)
  in
  (* -1 = "S" vertex *)
  expected_label 1 8;
  expected_label 2 (-1);
  expected_label 3 (-1);
  expected_label 4 5;
  expected_label 5 2;
  expected_label 6 4;
  expected_label 7 4;
  expected_label 8 2;
  Alcotest.(check bip_node_opt_t)
    ("Expected path")
    (Some (HM.V (G.V.create 6)))
    (!path)

(* Step 5 *)
let test_hung_step_5 () =
  let _, _, _, _, _, x, mate, card, _, _, _, _, _ = step_5 () in
  Alcotest.(check int)
    ("Expected cardinal")
    (3)
    (card);
  let expected_primal_val i j c =
    let c' = try (Hashtbl.find x ((G.V.create i), (G.V.create j))) with
      | _ -> Alcotest.fail "A primal variable is not initiate in step 2"
    in
    Alcotest.(check int)
      ("Expected value of primal variable in step 2")
      (c)
      (c')
  in
  expected_primal_val 1 5 0;
  expected_primal_val 1 6 0;
  expected_primal_val 1 7 0;
  expected_primal_val 1 8 1;
  expected_primal_val 2 5 1;
  expected_primal_val 2 6 0;
  expected_primal_val 2 7 0;
  expected_primal_val 2 8 0;
  expected_primal_val 3 5 0;
  expected_primal_val 3 6 0;
  expected_primal_val 3 7 0;
  expected_primal_val 3 8 0;
  expected_primal_val 4 5 0;
  expected_primal_val 4 6 1;
  expected_primal_val 4 7 0;
  expected_primal_val 4 8 0;
  let expected_mate i j =
    let expected = match j with
    | 0 -> None
    | j' -> Some (G.V.create j')
    in
    let node = try (Hashtbl.find mate (G.V.create i)) with
      | _ -> Alcotest.fail "A primal variable is not initiate in step 2"
    in
    Alcotest.(check vertex_opt_t)
      ("Expected mate")
      (expected)
      (node)
  in
  expected_mate 1 8;
  expected_mate 2 5;
  expected_mate 3 0;
  expected_mate 4 6;
  expected_mate 5 2;
  expected_mate 6 4;
  expected_mate 7 0;
  expected_mate 8 1

(* All the algo *)
let test_hung_all () =
  let _, _, _, mate = hung_algo () in
  let expected_mate i j =
    let expected = match j with
    | 0 -> None
    | j' -> Some (G.V.create j')
    in
    let node = try (Hashtbl.find mate (G.V.create i)) with
      | _ -> Alcotest.fail "A primal variable is not initiate in step 2"
    in
    Alcotest.(check vertex_opt_t)
      ("Expected mate")
      (expected)
      (node)
  in
  expected_mate 1 7;
  expected_mate 2 5;
  expected_mate 3 8;
  expected_mate 4 6;
  expected_mate 5 2;
  expected_mate 6 4;
  expected_mate 7 1;
  expected_mate 8 3

(* All the algo but with the make function *)
let test_hung_make () =
  let g = lecture_example () in
  let covering = HM.make g in
  let expected_covering i j =
    let expected = G.V.create j in
    let node = try (Hashtbl.find covering (G.V.create i)) with
      | _ -> Alcotest.fail "A primal variable is not initiate in step 2"
    in
    Alcotest.(check vertex_t)
      ("Expected covering")
      (expected)
      (node)
  in
  expected_covering 1 7;
  expected_covering 2 5;
  expected_covering 3 8;
  expected_covering 4 6;
  expected_covering 5 2;
  expected_covering 6 4;
  expected_covering 7 1;
  expected_covering 8 3

(* ---------------------------------------------------------------------------------------------- *)
(* RUNNING TESTS *)

let () =
  Alcotest.run "Hungarian Method" [
      "bipartite-tests", [
        test_case "bipartite" `Quick test_bipartite;
        test_case "bipartite_graph_complete" `Quick test_bipartite_graph_complete;
      ];
      "hungarian-steps", [
        test_case "hung_step_1" `Quick test_hung_step_1;
        test_case "hung_step_2" `Quick test_hung_step_2;
        test_case "hung_step_3_1" `Quick test_hung_step_3_1;
        test_case "hung_step_3_2" `Quick test_hung_step_3_2;
        test_case "hung_step_4" `Quick test_hung_step_4;
        test_case "hung_before_step_5" `Quick test_hung_before_step_5;
        test_case "hung_step_5" `Quick test_hung_step_5;
        test_case "hung_all" `Quick test_hung_all;
        test_case "hung_make" `Quick test_hung_make;
      ];
    ]

