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

let test_forced_move_left () =
  let open BinaryTree in
  let tree1 = Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf)) in
  let tree2 = Node (Node (Leaf, 6, Leaf), 7, Node (Leaf, 8, Leaf)) in
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
  let fleft1 = P.forced_move_left test2 graph 1 6 in
  Alcotest.(check int) "forced move cost on full bipartite should be null" 0 (Cost.cost_to_int fleft1)

let () =
  Alcotest.run "Pruning" [
      "forced-moves", [
        test_case "trivial_move_left" `Quick test_forced_move_left;
        (*test_case "" `Quick test_forced_move_right;*)
      ];
    ]
