module Make(I: Sig.INPUT)(G: Sig.G) = struct
  let prune (t1: I.t) (t2: I.t) (graph: G.t) : G.t =
    graph
end

(* By maintaining a priority queue (based on edge costs) of edges incident on *)
(* each node of the induced graph, the test to determine whether an edge may be *)
(* pruned can be performed in constant time. If the edge is pruned, removing it *)
(* from the induced graph requires constant time, while removing it from the *)
(* priority queues at each of its nodes requires O(logn) time. When an edge [m; *)
(* n] is pruned, we also record the changes to the costs cm?(m; p(n)), cm?(n; *)
(* p(m)), cmf (m; p(n)), and cmf (n; p(m)), which can be done in constant *)
(* time. Thus, pruning an edge requires O(logn) time. Since at most O(n^2) are *)
(* pruned, the total worst case cost of the pruning phase is O(n^2logn) *) 

(* Goal: precompute all the costs and keep them in a data-structure.
   Update them on the fly in O(1).

 *)

