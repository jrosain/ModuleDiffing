open Alcotest
open ModuleDiffing

(* Setup: the input type.
   For the tests cases, we will implement a simple binary tree, where each node is identified by a
   unique integer.

   Furthermore, we will keep labels in the input type. *)
module BinaryTree = struct
  type t = Leaf | Node of t * int * t
  
  let child_value (tree: t) : int =
    match tree with
    | Leaf -> -1
    | Node (_, value, _) -> value

  let rec parent (tree: t) (index: int) : int option =
    match tree with
    | Leaf -> None
    | Node (left, value, right) ->
       let val_left, val_right = (child_value left), (child_value right) in
       if val_left = index || val_right = index then Some value
       else
         match (parent left index) with
         | None -> parent right index
         | Some x -> Some x
  
  let rec children (tree: t) (index: int) : int list =
    match tree with
    | Leaf -> []
    | Node (left, value, right) -> 
       if (value = index) then
         match (left, right) with
         | Leaf, Leaf -> []
         | Node (_, lval, _), Leaf -> [lval]
         | Leaf, Node (_, rval, _) -> [rval]
         | Node (_, lval, _), Node (_, rval, _) -> [lval ; rval]
       else
         (children left index) @ (children right index)
end

module type Test = sig
  include Sig.INPUT
end

module Test = struct
  type i = ((int -> string) * BinaryTree.t)
  type v = int
  type node = (string * v)
  type t = (node list * i)

  let create (input: i) : t =
    let rec aux (tree: BinaryTree.t) (curr: node list) : node list =
      match tree with
      | Leaf -> curr
      | Node (left, value, right) ->
         aux (right) (aux (left) (((fst input) value, value) :: curr))
    in (aux (snd input) [], input)

  let parent (input: t) (index: node) : node option =
    match (BinaryTree.parent (snd (snd input)) (snd index)) with
    | None -> None
    | Some v -> Some ((fst index), v)

  let children (input: t) (index: node) : node list =
    let children = BinaryTree.children (snd (snd input)) (snd index) in
    List.map (fun c -> ((fst (snd input)) c, c)) children

  let elements (input: t) : node list = (fst input)

  let label (element: node) : string = fst element
  let value (element: node) : v = snd element
  let root  (tree: t) : node =
    match (snd (snd tree)) with
    | Leaf -> ("", -1)
    | Node (_, value, _) -> ((fst (snd tree)) value, value)

  let compare (x: node) (y: node) : Cost.t =
    let s1 = label x in
    let s2 = label y in
    let total = ref 0 in
    let f =
      (fun dest i c ->
        try total := !total + (Int.abs ((Char.code c) - (Char.code dest.[i])))
        with _ -> total := !total + (Char.code c) - (Char.code 'a') + 1) in
    (if (String.length s1) >= (String.length s2) then String.iteri (f s2) s1
     else String.iteri (f s1) s2);
    Cost.of_int (!total)

end

module Node = struct
  module Input = Test
  
  type t = Original of Input.node | Minus | Plus | Dummy of int
  let mk v = Original v
  let minus () = Minus
  let plus () = Plus
  let compare = Stdlib.compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module G = Graph.Imperative.Graph.ConcreteLabeled(Node)(Sig.Edge)

let create_bipartite (t1: Test.t) (t2: Test.t) : G.t =
  let transform = List.map (fun x -> Node.mk x) in
  let nodes1 = Node.plus() :: (transform (Test.elements t1)) in
  let nodes2 = Node.minus() :: (transform (Test.elements t2)) in
  let g = G.create ~size:((List.length nodes1) + (List.length nodes2)) () in
  List.iter (G.add_vertex g) nodes1;
  List.iter (G.add_vertex g) nodes2;
  List.iter (fun x -> List.iter (G.add_edge g x) nodes2) nodes1;
  g

module P = Pruning.Make(Test)(Node)(G)
(* Setup end. *)

let trivial_example () =
  let open BinaryTree in
  let tree1 = Node (Node (Leaf, 2, Leaf), 1, Node (Leaf, 3, Leaf)) in
  let tree2 = Node (Node (Leaf, 7, Leaf), 6, Node (Leaf, 8, Leaf)) in
  let labels1 = function
    | 1 -> "a"
    | 2 -> "d"
    | 3 -> "f"
    | _ -> ""
  in
  let labels2 = function
    | 6 -> "a"
    | 7 -> "c"
    | 8 -> "g"
    | _ -> ""
  in
  let test1 = Test.create (labels1, tree1) in
  let test2 = Test.create (labels2, tree2) in
  let graph = create_bipartite test1 test2 in
  test1, test2, graph


let cost_pp ppf c = Fmt.pf ppf "Cost(%d)" (Cost.to_int c)
let cost_equal (ca: Cost.t) (cb: Cost.t) : bool = ((Cost.compare ca cb) = 0)
let cost_t = Alcotest.testable cost_pp cost_equal

let node = function
  | 1 -> "a", 1
  | 2 -> "d", 2
  | 3 -> "f", 3
  | 6 -> "a", 6
  | 7 -> "c", 7
  | 8 -> "g", 8
  | _ -> failwith "internal error"

(* ---------------------------------------------------------------------------------------------- *)
(* FORCED MOVES *)

(* The cost of the forced move between the two roots should be null on a full bipartite.  *)
let test_trivial_root_forced_move_left () =
  let test1, test2, graph = trivial_example() in
  List.iter
    (fun v ->
      let fcost = P.forced_move_left test2 graph v (node 6) in
      Alcotest.(check cost_t) "forced move cost on non-leaf in full bipartite should be null"
        (Cost.null) (fcost)) (Test.children test1 (node 1))

(* The cost of the forced move between a root and a leaf should be non-null on a full bipartite.  *)
let test_trivial_non_root_forced_move_left () =
  let test1, test2, graph = trivial_example() in
  List.iter
    (fun v ->
      let fcost = P.forced_move_left test2 graph v (node 7) in
      Alcotest.(check cost_t) "forced move cost on leaf in full bipartite should be Cost.cm"
        (Cost.cm) (fcost)) (Test.children test1 (node 1))

(* Symmetric test. *)
let test_trivial_root_forced_move_right () =
  let test1, test2, graph = trivial_example() in
  List.iter
    (fun v ->
      let fcost = P.forced_move_right test1 graph (node 1) v in
      Alcotest.(check cost_t) "forced move cost on non-leaf in full bipartite should be null"
        (Cost.null) (fcost)) (Test.children test2 (node 6))

let test_trivial_non_root_forced_move_right () =
  let test1, test2, graph = trivial_example() in
  List.iter
    (fun v ->
      let fcost = P.forced_move_right test1 graph (node 2) v in
      Alcotest.(check cost_t) "forced move cost on leaf in full bipartite should be Cost.cm"
        (Cost.cm) (fcost)) (Test.children test2 (node 6))

(* What happens if we remove one edge ? Two edges ? All the edges ? *)
let test_remove_edges_forced_move_left () =
  let test1, test2, graph = trivial_example() in
  let test expect message = 
    List.iter
      (fun v -> 
        let fcost = P.forced_move_left test2 graph v (node 6) in
        Alcotest.(check cost_t) message (expect) (fcost))
      (Test.children test1 (node 1))
  in
  List.iter
    (fun (a, b) ->
      G.remove_edge graph (Node.mk a) (Node.mk b);
      test (Cost.null)
        "forced move cost on roots should be null when not all edges are removed between their children")
    [(node 2, node 7) ; (node 3, node 7)];
  G.remove_edge graph (Node.mk (node 2)) (Node.mk (node 8));
  (* Should be cm, 0 *)
  let fcost = P.forced_move_left test2 graph (node 2) (node 6) in
  Alcotest.(check cost_t) "forced move cost on roots should be not null when all edges are removed
                           between their children" (Cost.cm) (fcost);
  let fcost = P.forced_move_left test2 graph (node 3) (node 6) in
  Alcotest.(check cost_t) "forced move cost on roots should be null when not all edges are removed
                           between their children" (Cost.null) (fcost);
  G.remove_edge graph (Node.mk (node 3)) (Node.mk (node 8));
  test
    (Cost.cm)
    "forced move cost on roots should be not null when all edges are removed between their children"

(* Same but for forced_move_right *)
let test_remove_edges_forced_move_right () =
  let test1, test2, graph = trivial_example() in
  let test expect message = 
    List.iter
      (fun v -> 
        let fcost = P.forced_move_right test1 graph (node 1) v in
        Alcotest.(check cost_t) message (expect) (fcost))
      (Test.children test2 (node 6))
  in
  List.iter
    (fun (a, b) ->
      G.remove_edge graph (Node.mk a) (Node.mk b);
      test (Cost.null)
        "forced move cost on roots should be null when not all edges are removed between their children")
    [(node 2, node 7) ; (node 2, node 8)];
  G.remove_edge graph (Node.mk (node 3)) (Node.mk (node 7));
  (* Should be cm, 0 *)
  let fcost = P.forced_move_right test1 graph (node 1) (node 7) in
  Alcotest.(check cost_t) "forced move cost on roots should be not null when all edges are removed
                           between their children" (Cost.cm) (fcost);
  let fcost = P.forced_move_right test1 graph (node 1) (node 8) in
  Alcotest.(check cost_t) "forced move cost on roots should be null when an edge exists
                           between their children" (Cost.null) (fcost);
  G.remove_edge graph (Node.mk (node 3)) (Node.mk (node 8));
  test
    (Cost.cm)
    "forced move cost on roots should be not null when all edges are removed between their children"

(* ---------------------------------------------------------------------------------------------- *)
(* UPDATE COST *)

(* Test if it is properly stored, i.e., if any pair (u, v) with u in T1 or in T2 and v in T2 or in *)
(* T1 has its cost stored in the table.*)
let test_record_of_update_cost () =
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  let is_expected_cost a b v = 
    Alcotest.(check cost_t)
      "recorded cost should be computed cost"
      (Pruning.CostTable.get costs (Node.mk a) (Node.mk b)) v;
    Alcotest.(check cost_t)
      "recorded cost should be computed cost"
      (Pruning.CostTable.get costs (Node.mk b) (Node.mk a)) v in
  List.iter
    (fun m -> List.iter (fun n -> is_expected_cost m n (Test.compare m n)) (Test.elements test2))
    (Test.elements test1)

(* Test if, given a non-edge, it fails. *)
let test_fail_on_non_edge () =
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  Alcotest.check_raises
    "cost table should fail if non-edge is given"
    (Failure("internal error"))
    (fun () -> let _ = Pruning.CostTable.get costs (Node.mk (node 1)) (Node.mk (node 1)) in ())

(* ---------------------------------------------------------------------------------------------- *)
(* LOWER BOUND COMPUTATION *)

let test_lower_bound_impossible_pm_combinations() =
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  let failure_test msg n m =
    Alcotest.check_raises
      msg
      (Failure("internal error"))
      (fun () -> let _ = P.lower_bound costs graph test1 test2 n m in ()) in
  failure_test "plus should not be given as the second argument" (Node.mk (node 1)) (Node.plus());
  failure_test "minus should not be given as the first argument" (Node.minus()) (Node.mk (node 6));
  failure_test "plus/minus should be given as the first/second argument" (Node.minus()) (Node.plus())

let test_plus_lower_bound() =
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  let test_plus v =
    Alcotest.(check cost_t)
      "a link to the 'plus' node should have the weight of the insertion"
      (Cost.lb_ci())
      (P.lower_bound costs graph test1 test2 (Node.plus()) (Node.mk v)) in
  List.iter (test_plus) (Test.elements test2)

let test_minus_lower_bound() =
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  let test_minus v =
    Alcotest.(check cost_t)
      "a link to the 'plus' node should have the weight of the insertion"
      (Cost.lb_cd())
      (P.lower_bound costs graph test1 test2 (Node.mk v) (Node.minus())) in
  List.iter (test_minus) (Test.elements test1)

let test_plus_minus_lower_bound() =
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  Alcotest.(check cost_t)
    "a link between 'plus' and 'minus' node should have a null weight"
    (Cost.null)
    (P.lower_bound costs graph test1 test2 (Node.plus()) (Node.minus()))

let update_cost f c =
  Cost.of_int (f (Cost.to_int c))

(* The lower-bound between the roots should be the update cost when no edge is removed. *)
let test_roots_full_bipartite_lower_bound() =
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  Alcotest.(check cost_t)
    "the lower-bound between non-leaves in a full bipartite graph should be their update cost."
    (update_cost (fun i -> 2*i) (Pruning.CostTable.get costs (Node.mk (node 1)) (Node.mk (node 6))))
    (P.lower_bound costs graph test1 test2 (Node.mk (node 1)) (Node.mk (node 6)))

(* The lower-bound between two leaves should always be the update cost. *)
let test_leaves_lower_bound() =
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  let test_costs () =
    List.iter
      (fun m -> List.iter
       (fun n ->
         Alcotest.(check cost_t)
           "the lower-bound between leaves in a full bipartite graph should always be their update cost."
           (update_cost (fun i -> 2*i) (Pruning.CostTable.get costs (Node.mk m) (Node.mk n)))
           (P.lower_bound costs graph test1 test2 (Node.mk m) (Node.mk n)))
       [node 7 ; node 8]
      )
      [node 2 ; node 3] in
  test_costs();
  List.iter (fun (a, b) -> G.remove_edge graph (Node.mk (node a)) (Node.mk (node b)); test_costs())
    [(1, 6) ; (1, 7) ; (1, 8) ; (2, 6) ; (2, 7) ; (2, 8) ; (3, 6) ; (3, 7) ; (3, 8)]

(* Lower-bound between the parents are updated when there is a child that is not linked to the *)
(* any of the other's children. *)
let test_removal_of_children_edges() =
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  G.remove_edge graph (Node.mk (node 2)) (Node.mk (node 7));
  G.remove_edge graph (Node.mk (node 3)) (Node.mk (node 7));
  let fcost = P.lower_bound costs graph test1 test2 (Node.mk (node 1)) (Node.mk (node 6)) in
  Alcotest.(check cost_t)
    "The lower-bound cost between nodes that have not all its children linked should not be the
     update cost"
    (update_cost (fun c -> c + (Cost.to_int Cost.cm)) (Pruning.CostTable.get costs (Node.mk (node 1)) (Node.mk (node 6))))
    (fcost)

(* Lower-bound between the parents s.t. none of their child are linked. *)
let test_no_edges_means_maximum_lb_cost() =
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  let rem_edge a b = G.remove_edge graph (Node.mk (node a)) (Node.mk (node b)) in
  List.iter (fun (a, b) -> rem_edge a b) [2, 7; 3, 7; 2, 8; 3, 8];
  let fcost = P.lower_bound costs graph test1 test2 (Node.mk (node 1)) (Node.mk (node 6)) in
  Alcotest.(check cost_t)
    "The lower-bound cost between nodes that have not any of its children linked should be the
     maximum lower-bound"
    (update_cost
       (fun c ->
         c + (Stdlib.min (List.length (Test.children test1 (node 1))) (List.length (Test.children test2 (node 6))))*(Cost.to_int Cost.cm))
       (Pruning.CostTable.get costs (Node.mk (node 1)) (Node.mk (node 6))))
    (fcost)

(* Lower-bound between something with children and something without children. *)
let test_children_no_children_lb_cost() =
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  let fcost = P.lower_bound costs graph test1 test2 (Node.mk (node 1)) (Node.mk (node 7)) in
  Alcotest.(check cost_t)
    "The lower-bound cost between something with children and something with no children should take
     a big penality."
    (update_cost
       (fun c -> 2*c + (List.length (Test.children test1 (node 1)))*(Cost.to_int Cost.cm))
       (Pruning.CostTable.get costs (Node.mk (node 1)) (Node.mk (node 7))))
    (fcost);
  let fcost = P.lower_bound costs graph test1 test2 (Node.mk (node 2)) (Node.mk (node 6)) in
  Alcotest.(check cost_t)
    "The lower-bound cost between something with children and something with no children should take
     a big penality."
    (update_cost
       (fun c -> 2*c + (List.length (Test.children test2 (node 6)))*(Cost.to_int Cost.cm))
       (Pruning.CostTable.get costs (Node.mk (node 2)) (Node.mk (node 6))))
    (fcost)

(* ---------------------------------------------------------------------------------------------- *)
(* UPPER BOUND COMPUTATION *)

let test_plus_minus_cancel_each_other_ub() =
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  let ucost = P.compute_upper_bound (Node.plus()) (Node.minus()) costs test1 test2 graph in
  Alcotest.(check cost_t)
    "The upper-bound cost between plus and minus node should be null."
    (Cost.null)
    (ucost)

let test_ub_plus_minus_combinations_that_should_fail() =
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  let failure_test msg n m =
    Alcotest.check_raises
      msg
      (Failure("internal error"))
      (fun () -> let _ = P.compute_upper_bound n m costs test1 test2 graph in ()) in
  failure_test "plus should not be given as the second argument" (Node.mk (node 1)) (Node.plus());
  failure_test "minus should not be given as the first argument" (Node.minus()) (Node.mk (node 6));
  failure_test "plus/minus should be given as the first/second argument" (Node.minus()) (Node.plus())  

let test_plus_upper_bound() =
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  let test_plus v =
    Alcotest.(check cost_t)
      "a link to the 'plus' node should have the weight of the insertion"
      (Cost.ub_ci())
      (P.compute_upper_bound (Node.plus()) (Node.mk v) costs test1 test2 graph) in
  List.iter (test_plus) (Test.elements test2)

let test_minus_upper_bound() =
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  let test_minus v =
    Alcotest.(check cost_t)
      "a link to the 'minus' node should have the weight of the deletion"
      (Cost.ub_cd())
      (P.compute_upper_bound (Node.mk v) (Node.minus()) costs test1 test2 graph) in
  List.iter (test_minus) (Test.elements test1)

let test_nonleaves_no_removal_upper_bound() =
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  let ucost = P.compute_upper_bound (Node.mk (node 1)) (Node.mk (node 6)) costs test1 test2 graph in
  Alcotest.(check cost_t)
    "The upper-bound cost between nodes that have all their children linked should be the
     maximum upper-bound"
    (update_cost (fun c -> 2*c + 4*(3*(Cost.to_int Cost.cc) + (Cost.to_int Cost.cm)))
       (Pruning.CostTable.get costs (Node.mk (node 1)) (Node.mk (node 6))))
    (ucost)

let test_leaves_min_ub_cost() =
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  let test_mincost (a, b) = 
    let ucost = P.compute_upper_bound (Node.mk (node a)) (Node.mk (node b)) costs test1 test2 graph in
    Alcotest.(check cost_t)
      "The upper-bound cost between leaves should be the minimum upper-bound (i.e., the update cost)"
      (update_cost (fun c -> 2*c) (Pruning.CostTable.get costs (Node.mk (node a)) (Node.mk (node b))))
      (ucost)
  in List.iter test_mincost [2, 7; 2, 8; 3, 7; 3, 8]

let test_removal_irrelevant_edges_upper_bound() = 
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  let ub = P.compute_upper_bound (Node.mk (node 1)) (Node.mk (node 6)) costs test1 test2 graph in
  G.remove_edge graph (Node.mk (node 1)) (Node.plus());
  Alcotest.(check cost_t)
    "The upper-bound cost should not change if an irrelevant edge is removed"
    (ub)
    (P.compute_upper_bound (Node.mk (node 1)) (Node.mk (node 6)) costs test1 test2 graph)

let test_removal_relevant_edges_upper_bound() = 
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  let ub = P.compute_upper_bound (Node.mk (node 1)) (Node.mk (node 6)) costs test1 test2 graph in
  G.remove_edge graph (Node.mk (node 1)) (Node.mk (node 7));
  Alcotest.(check int)
    "The upper-bound cost should change if a relevant edge is removed"
    (1)
    (Cost.compare ub (P.compute_upper_bound (Node.mk (node 1)) (Node.mk (node 6)) costs test1 test2 graph))

let rec validate msg goal = function
  | [] -> ()
  | [_] -> ()
  | h::x::t ->
     let comp = (Cost.compare h x) in
     if (List.exists (fun v -> comp = v) goal) then
       validate msg goal t
     else
       Alcotest.(check int)
         msg
         (List.hd goal)
         (Cost.compare h x)

let test_removal_edges_upper_bound() = 
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  let ub = P.compute_upper_bound (Node.mk (node 1)) (Node.mk (node 6)) costs test1 test2 graph in
  let successive_scores =
    let compute (a, b) =
      G.remove_edge graph (Node.mk (node a)) (Node.mk (node b));
      P.compute_upper_bound (Node.mk (node 1)) (Node.mk (node 6)) costs test1 test2 graph
    in List.map (compute) [(2, 7) ; (3, 8) ; (2, 8) ; (3, 8)]
  in (validate "The upper-bound should decrease when removing edges" [1] (ub::successive_scores))

(* ---------------------------------------------------------------------------------------------- *)
(* HEAPS *)

let heap_to_list h =
  let rec aux (acc: Cost.t list) =
    try 
      let acc' = (Pruning.UpperBoundHeap.top h) :: acc in
      Pruning.UpperBoundHeap.remove_top h;
      aux acc'
    with _ -> acc
  in aux []

let test_heap_init () =
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  let ub_table = P.init_upper_bounds test1 test2 graph costs in
  (* For each element of test1 (resp. test2), check if the heap has size |t2| + 1 (resp. |t1| + 1)
  and that each heap can be converted to a list that is validated. *)
  let check_heap test oth =
    List.iter
      (fun m ->
        let l = heap_to_list (Hashtbl.find ub_table (Node.mk m)) in
        Alcotest.(check int)
          "When no edge is removed, the number of elements in an upper-bound heap should be the max."
          ((List.length (Test.elements oth)) + 1)
          (List.length l);
        validate "The elements of the upper bound heap should be in (non-strict) increasing order" [1;0] l)
      (Test.elements test)
  in
  check_heap test1 test2;
  check_heap test2 test1

let test_heap_init_edge_removed() =
  let test1, test2, graph = trivial_example() in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  G.remove_edge graph (Node.mk (node 1)) (Node.mk (node 7));
  let ub_table = P.init_upper_bounds test1 test2 graph costs in
  let l1 = heap_to_list (Hashtbl.find ub_table (Node.mk (node 1))) in
  Alcotest.(check int)
    "When edges are removed, the number of elements in an upper-bound heap should be adjusted accordingly."
    (List.length (Test.elements test2))
    (List.length l1);
  let l7 = heap_to_list (Hashtbl.find ub_table (Node.mk (node 7))) in
  Alcotest.(check int)
    "When edges are removed, the number of elements in an upper-bound heap should be adjusted accordingly."
    (List.length (Test.elements test1))
    (List.length l7)

(* ---------------------------------------------------------------------------------------------- *)
(* PRUNING IN ITSELF *)

let test_all_nodes_have_at_least_one_edge() =
  let test1, test2, graph = trivial_example() in
  let graph = P.prune test1 test2 graph in
  G.iter_vertex
    (fun x ->
      Alcotest.(check bool)
        "Every vertex of the pruned graph should have at least one neighbour."
        (true)
        ((G.out_degree graph x) > 0))
    graph

let test_example_expect_edges() =
  let test1, test2, graph = trivial_example() in
  let graph = P.prune test1 test2 graph in
  let check_edge msg x y =
    Alcotest.(check bool)
      ("Expected an edge between " ^ msg)
      (true)
      (G.mem_edge graph x y)
  in
  check_edge "the roots." (Node.mk (node 1)) (Node.mk (node 6));
  check_edge "the leaves." (Node.mk (node 2)) (Node.mk (node 7));
  check_edge "the leaves." (Node.mk (node 3)) (Node.mk (node 8))

let test_same_root_no_link_plus_minus() = 
  let test1, test2, graph = trivial_example() in
  let graph = P.prune test1 test2 graph in
  let check_edge msg x y =
    Alcotest.(check bool)
      ("Expected no edge between " ^ msg)
      (false)
      (G.mem_edge graph x y)
  in
  check_edge "root and minus." (Node.mk (node 1)) (Node.minus());
  check_edge "root and plus." (Node.plus()) (Node.mk (node 6))

let test_edge_labeling() =
  let test1, test2, graph = trivial_example() in
  let graph = P.prune test1 test2 graph in
  let costs = P.compute_update_costs test1 test2 (G.nb_edges graph) in
  G.iter_edges
    (fun x y ->
      let e = G.find_edge graph x y in
      Alcotest.(check cost_t)
        "The edges of the pruned graph should be labeled with lower-bounds."
        (P.lower_bound costs graph test1 test2 x y)
        (G.E.label e))
    graph

let () =
  Alcotest.run "Pruning" [
      "forced-moves", [
        test_case "trivial_root_move_left" `Quick test_trivial_root_forced_move_left;
        test_case "trivial_non_root_move_left" `Quick test_trivial_non_root_forced_move_left;
        test_case "trivial_root_move_right" `Quick test_trivial_root_forced_move_right;
        test_case "trivial_non_root_move_right" `Quick test_trivial_non_root_forced_move_right;
        test_case "remove_edges_move_left" `Quick test_remove_edges_forced_move_left;
        test_case "remove_edges_move_right" `Quick test_remove_edges_forced_move_right;
      ];
      "update-costs", [
          test_case "record_update_costs" `Quick test_record_of_update_cost;
          test_case "fail_on_non_edge" `Quick test_fail_on_non_edge;
        ];
      "lower-bound", [
          test_case "plus_minus_combinations_that_should_fail" `Quick test_lower_bound_impossible_pm_combinations;
          test_case "edge_to_plus_should_have_ci_lb" `Quick test_plus_lower_bound;
          test_case "edge_to_minus_should_have_cd_lb" `Quick test_minus_lower_bound;
          test_case "edge_plus_minus_should_have_null_cost" `Quick test_plus_minus_lower_bound;
          test_case "edge_between_nonleaves_should_be_update_cost" `Quick test_roots_full_bipartite_lower_bound;
          test_case "leaves_lower_bound" `Quick test_leaves_lower_bound;
          test_case "removal_of_children_edges_adds_cost" `Quick test_removal_of_children_edges;
          test_case "removal_of_all_children_set_max_cost" `Quick test_no_edges_means_maximum_lb_cost;
          test_case "cost_between_internal_node_and_leaf" `Quick test_children_no_children_lb_cost;
        ];
      "upper-bound", [
          test_case "plus_minus_cancel_each_other_ub" `Quick test_plus_minus_cancel_each_other_ub;
          test_case "plus_minus_combinations_that_fail" `Quick test_ub_plus_minus_combinations_that_should_fail;
          test_case "plus_upper_bound" `Quick test_plus_upper_bound;
          test_case "minus_upper_bound" `Quick test_minus_upper_bound;
          test_case "nonleaves_no_removal" `Quick test_nonleaves_no_removal_upper_bound;
          test_case "leaves_min_cost" `Quick test_leaves_min_ub_cost;
          test_case "removing_irrelevant_edges" `Quick test_removal_irrelevant_edges_upper_bound;
          test_case "removing_relevant_edges" `Quick test_removal_relevant_edges_upper_bound;
          test_case "removing_edges_lowers_ub" `Quick test_removal_edges_upper_bound;
        ];
      "heaps", [
          test_case "right_heap_initialization" `Quick test_heap_init;
          test_case "right_heap_init_when_edge_removed" `Quick test_heap_init_edge_removed;
        ];
      "pruning", [
          test_case "all_nodes_have_one_neighbours_or_more" `Quick test_all_nodes_have_at_least_one_edge;
          test_case "edges_expectation_in_example" `Quick test_example_expect_edges;
          test_case "same_root_no_link_plus_minus" `Quick test_same_root_no_link_plus_minus;
          test_case "edge_are_labeled" `Quick test_edge_labeling;
        ];
    ]

