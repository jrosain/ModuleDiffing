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
  type t = (v list * i)

  let create (input: i) : t =
    let rec aux (tree: BinaryTree.t) (curr: v list) : v list =
      match tree with
      | Leaf -> curr
      | Node (left, value, right) ->
         aux (right) (aux (left) (value :: curr))
    in (aux (snd input) [], input)

  let parent (input: t) (index: v) : v option =
    BinaryTree.parent (snd (snd input)) index

  let children (input: t) (index: v) : v list =
    BinaryTree.children (snd (snd input)) index

  let elements (input: t) : v list = (fst input)

  let print_v (x: v) : unit =
    Printf.printf "%d" x

  (*let to_string (input: t) (x: v) : string =
    let label = (fst (snd input)) x in
    label ^ " (" ^ (string_of_int x) ^ ")"*)

  let compare (input: t) (x: v) (y: v) : Cost.t =
    let s1 = (fst (snd input)) x in
    let s2 = (fst (snd input)) y in
    let total = ref 0 in
    let f =
      (fun dest i c ->
        try total := !total + (Int.abs ((Char.code c) - (Char.code dest.[i])))
        with _ -> total := !total + (Char.code c) - (Char.code 'a') + 1) in
    (if (String.length s1) >= (String.length s2) then String.iteri (f s2) s1
     else String.iteri (f s1) s2);
    Cost.int_to_cost (!total)
end

module Node = struct
  module Input = Test
  
  type t = Original of Input.v | Minus | Plus
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


let cost_pp ppf c = Fmt.pf ppf "Cost(%d)" (Cost.cost_to_int c)
let cost_equal (ca: Cost.t) (cb: Cost.t) : bool = ((Cost.compare ca cb) = 0)
let cost_t = Alcotest.testable cost_pp cost_equal

(* The cost of the forced move between the two roots should be null on a full bipartite.  *)
let test_trivial_root_forced_move_left () =
  let test1, test2, graph = trivial_example() in
  List.iter
    (fun v ->
      let fcost = P.forced_move_left test2 graph v 6 in
      Alcotest.(check cost_t) "forced move cost on non-leaf in full bipartite should be null"
        (Cost.null) (fcost)) (Test.children test1 1)

(* The cost of the forced move between a root and a leaf should be non-null on a full bipartite.  *)
let test_trivial_non_root_forced_move_left () =
  let test1, test2, graph = trivial_example() in
  List.iter
    (fun v ->
      let fcost = P.forced_move_left test2 graph v 7 in
      Alcotest.(check cost_t) "forced move cost on leaf in full bipartite should be Cost.cm"
        (Cost.cm) (fcost)) (Test.children test1 1)

(* Symmetric test. *)
let test_trivial_root_forced_move_right () =
  let test1, test2, graph = trivial_example() in
  List.iter
    (fun v ->
      let fcost = P.forced_move_right test1 graph 1 v in
      Alcotest.(check cost_t) "forced move cost on non-leaf in full bipartite should be null"
        (Cost.null) (fcost)) (Test.children test2 6)

let test_trivial_non_root_forced_move_right () =
  let test1, test2, graph = trivial_example() in
  List.iter
    (fun v ->
      let fcost = P.forced_move_right test1 graph 2 v in
      Alcotest.(check cost_t) "forced move cost on leaf in full bipartite should be Cost.cm"
        (Cost.cm) (fcost)) (Test.children test2 6)

(* What happens if we remove one edge ? Two edges ? All the edges ? *)
let test_remove_edges_forced_move_left () =
  let test1, test2, graph = trivial_example() in
  let test expect message = 
    List.iter
      (fun v -> 
        let fcost = P.forced_move_left test2 graph v 6 in
        Alcotest.(check cost_t) message (expect) (fcost))
      (Test.children test1 1)
  in
  List.iter
    (fun (a, b) ->
      G.remove_edge graph (Node.mk a) (Node.mk b);
      test (Cost.null)
        "forced move cost on roots should be null when not all edges are removed between their children")
    [(2, 7) ; (3, 7)];
  G.remove_edge graph (Node.mk 2) (Node.mk 8);
  (* Should be cm, 0 *)
  let fcost = P.forced_move_left test2 graph 2 6 in
  Alcotest.(check cost_t) "forced move cost on roots should be not null when all edges are removed
                           between their children" (Cost.cm) (fcost);
  let fcost = P.forced_move_left test2 graph 3 6 in
  Alcotest.(check cost_t) "forced move cost on roots should be not null when all edges are removed
                           between their children" (Cost.null) (fcost);
  G.remove_edge graph (Node.mk 3) (Node.mk 8);
  test
    (Cost.cm)
    "forced move cost on roots should be not null when all edges are removed between their children"

let () =
  Alcotest.run "Pruning" [
      "forced-moves", [
        test_case "trivial_root_forced_move_left" `Quick test_trivial_root_forced_move_left;
        test_case "trivial_non_root_forced_move_left" `Quick test_trivial_non_root_forced_move_left;
        test_case "trivial_root_forced_move_right" `Quick test_trivial_root_forced_move_right;
        test_case "trivial_non_root_forced_move_right" `Quick test_trivial_non_root_forced_move_right;
        test_case "test_remove_edges_forced_move_left" `Quick test_remove_edges_forced_move_left;
      ];
    ]

