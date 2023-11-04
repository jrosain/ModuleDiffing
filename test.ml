(** This file is used to test the algorithm on a nice input. *)

module Tree = struct
  type t = Leaf | Node of int * (t list)

  let parent (tree: t) (v: int) : int =
    let rec aux (tree: t) : int =
      match tree with
      | Leaf -> -1
      | Node (p, ls) ->
         if (List.exists (fun e -> match e with Leaf -> false | Node(x, _) -> x = v) ls) then
           p
         else
           List.find (fun i -> i <> -1) (List.map aux ls)
    in aux tree

  let children (tree: t) (v: int) : int list =
    let rec aux (tree: t) : int list = 
      match tree with
      | Leaf -> failwith "Not found"
      | Node (p, ls) ->
         if p = v then
           List.filter (fun i -> i <> -1) (List.map (fun e -> match e with Leaf -> -1 | Node (v, _) -> v) ls)
         else
           List.flatten (List.map aux ls)
    in aux tree
end

module type Test = sig
  include Sig.INPUT
end

module Test = struct
  type i = Tree.t
  type v = int
  type t = (v list * i)

  let create (input: i) : t =
    let rec aux (tree: i) : v list =
      match input with
      | Leaf -> []
      | Node (v, children) -> List.fold_left (fun acc child -> (acc) @ (aux child)) [v] children
    in (aux input, input)
         
  let parent (tree: t) (index: v) : v = Tree.parent (snd tree) (List.nth (fst tree) index)
  
  let children (tree: t) (index: v) : v list = Tree.children (snd tree) (List.nth (fst tree) index)
    
  let elements (tree: t) : v list = (fst tree)
  let compare (tree: t) (x: v) (y: v) : Cost.t = Cost.int_to_cost (if x > y then (x - y) else (y - x))
end

module Diff = Diffing.Make(Test)

let launch_test () =
  let tree1 = Tree.Node(1, [Tree.Node(2, [Tree.Node(3, [])]) ; Tree.Node(4, [Tree.Node(5, []) ; Tree.Node(6, []) ; Tree.Node(7, [Tree.Node(8, [])])]) ; Tree.Node(9, [Tree.Node(10, [])])]) in
  let tree2 = Tree.Node(51, [Tree.Node(52, [Tree.Node(53, [])]) ; Tree.Node(55, [Tree.Node(56, []) ; Tree.Node(57, [Tree.Node(58, [Tree.Node(59, [])])]) ; Tree.Node(61, [Tree.Node(62, [])])]) ; Tree.Node(60, [Tree.Node(63, [Tree.Node(64, [])])])]) in
  let t1 = Test.create tree1 in
  let t2 = Test.create tree2 in
  let patch = Diff.exec t1 t2 in
  ()
