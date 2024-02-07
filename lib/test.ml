(** This file is used to test the algorithm on a nice input. *)

module Tree = struct
  type t = Null | Node of (string * int) * (t list)

  let parent (tree: t) (v: int) : (string * int) option =
    match tree with
    | Null -> failwith "No tree"
    | Node ((_, k), _) ->
       if k = (v) then None
       else
         let rec aux (tree: t) : (string * int) option =
           match tree with
           | Null -> None
           | Node ((s, p), ls) ->
              if (List.length ls) = (0) then None
              else if (List.exists (fun e -> match e with Null -> false | Node ((_, x), _)
                                                                          -> x = v) ls) then Some (s, p)
              else
                try
                  List.find (fun x -> match x with None -> false | Some _ -> true) (List.map aux ls)
                with
                | Not_found -> None
         in aux tree

  let children (tree: t) (v: int) : (string * int) list =
    let rec aux (tree: t) : (string * int) list option =
      match tree with
      | Null -> failwith "No tree"
      | Node ((_, p), ls) -> 
         if (p = v) then Some (List.map (function | Null -> ("", 0) | (Node (x, _)) -> x) ls)
         else if (List.length ls) = 0 then None
         else
           try
             List.find (fun x -> match x with None -> false | Some _ -> true) (List.map aux ls)
           with
           | Not_found -> None
    in
    match (aux tree) with
    | None -> []
    | Some ls -> ls
end

module type Test = sig
  include Sig.INPUT
end

module Test = struct
  type i = Tree.t
  type v = (string * int)
  type t = (v list * i)

  let create (input: i) : t =
    let rec aux (tree: i) : v list =
      match tree with
      | Tree.Null -> []
      | Tree.Node (v, children) -> v :: (List.flatten (List.map aux children))
    in (aux input, input)
         
  let parent (tree: t) (index: v) : v option =
    Tree.parent (snd tree) (snd index)
  
  let children (tree: t) (index: v) : v list =
    Tree.children (snd tree) (snd index)
    
  let elements (tree: t) : v list = (fst tree)
  let compare (_: t) (_: t) (x: v) (y: v) : Cost.t =
    let s1, s2 = (fst x), (fst y) in
    let total = ref 0 in
    let f =
      (fun dest i c ->
        try total := !total + (Int.abs ((Char.code c) - (Char.code dest.[i])))
        with _ -> total := !total + (Char.code c) - (Char.code 'a') + 1) in
    (if (String.length s1) >= (String.length s2) then String.iteri (f s2) s1
     else String.iteri (f s1) s2);
    Cost.int_to_cost (!total)
    
  let print_v (x: v) = print_int (snd x)
end

module Diff = Diffing.Make(Test)

let launch_test () =
  let tree1 = Tree.Node(("a", 1), [Tree.Node(("b", 2), [Tree.Node(("d", 3), [])]) ;
                                   Tree.Node(("e", 4), [Tree.Node(("a", 5), []) ;
                                                        Tree.Node(("f", 6), []) ]);
                                   Tree.Node(("cc", 7),[Tree.Node(("ac", 8),[])]) ;
                                   Tree.Node(("cd", 9), [Tree.Node(("ad", 10), [])])]) in
  let tree2 = Tree.Node(("a", 51), [Tree.Node(("f", 52), [Tree.Node(("b", 53),
                                                                    [Tree.Node(("d", 54), [])])]) ;
                                    Tree.Node(("e", 55), [Tree.Node(("a", 56), []) ;
                                                          Tree.Node(("f", 57),
                                                                    [Tree.Node(("b", 58), [Tree.Node(("d", 59), [])])])]) ;
                                              Tree.Node(("g", 60), [Tree.Node(("cc", 61), [Tree.Node(("ac", 62),
                                                                                                      [])])
  ; Tree.Node(("cd", 63), [Tree.Node(("ad", 64), [])])])]) in
  let t1 = Test.create tree1 in
  let t2 = Test.create tree2 in
  let _ = Diff.exec t1 t2 in
  ()
