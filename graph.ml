(* Public types *)
type 'a vertices = 'a list
type g = (int * float) list list
type 'a graph    =  ('a vertices * g)

(* Private functions *)
let get_index (v: 'a) (vertices: 'a vertices) : int =
  let rec aux (explore: 'a vertices) (acc: int) : int =
    match explore with
    | [] -> failwith "Vertex not found in the graph."
    | h::t ->
       if h = v then acc
       else aux (t) (acc + 1)
  in
  aux (vertices) 0

let add_edge_from (src: int) (dst: int) (w: float) (graph: g) : g =
  let rec aux (explore: g) (src: int) =
    let head = (List.hd explore) in
    match src with
    | 0 -> ((dst, w) :: head) :: (List.tl explore)
    | _ -> head :: (aux (List.tl explore) (src - 1))
  in aux (graph) (src)

let rec remove_link (links: (int*float) list) (v: int) : (int*float) list =
  match links with
  | [] -> []
  | h :: t ->
     let dst, _ = h in
     if dst = v then (remove_link t v)
     else h :: (remove_link t v)

let rec remove_links (explore: g) (v: int) : g =
  match explore with
  | [] -> []
  | h::t -> (remove_link h v) :: (remove_links t v)

let remove_vertex_edges (v: int) (graph: 'a graph) : 'a graph =
  let rec remove_vertex (g: 'a graph) (v: int) : 'a graph =
    match v with
    | 0 -> (List.tl (fst g), List.tl (snd g))
    | _ ->
       let nv, ng = remove_vertex (List.tl (fst g), List.tl (snd g)) (v - 1) in
       (List.hd (fst g) :: nv, List.hd (snd g) :: ng)
  in
  let nv, ng = remove_vertex graph v in
  (nv, remove_links ng v)

let remove_link (x: int) (y: int) (edges: g) : g =
  let rec aux (explore: g) (x: int) : g =
    match x with
    | 0 -> (remove_link (List.hd explore) y) :: (List.tl explore)
    | _ -> (List.hd explore) :: (aux (List.tl explore) (x-1))
  in aux edges x

let rec get_actual_vertices (links: (int * float) list) (vertices: 'a vertices) : 'a vertices =
  let rec aux (vertices: 'a vertices) (goal: int) : 'a =
    match goal with
    | 0 -> (List.hd vertices)
    | _ -> aux (List.tl vertices) (goal - 1)
  in
  match links with
  | [] -> []
  | (dst,_)::t -> (aux (vertices) (dst)) :: (get_actual_vertices (t) (vertices))

(* Implementation of the public interface *)

let empty () = ([], [])

let add_vertex (v: 'a) (g: 'a graph) : 'a graph =
  (v :: (fst g), [] :: snd g)

let add_edge (x: 'a) (y: 'a) (w: float) (g: 'a graph) : 'a graph =
  let x_index = get_index x (fst g) in
  let y_index = get_index y (fst g) in
  (fst g, add_edge_from (x_index) (y_index) (w) (snd g))

let remove_vertex (v: 'a) (g: 'a graph) : 'a graph =
  let v_index = get_index v (fst g) in
  remove_vertex_edges v_index g

let remove_edge (x: 'a) (y: 'a) (g: 'a graph) : 'a graph =
  let x_index = get_index x (fst g) in
  let y_index = get_index y (fst g) in
  (fst g, remove_link x_index y_index (snd g))

let vertices (g: 'a graph) = (fst g)

let neighbours (v: 'a) (graph: 'a graph) : 'a vertices =
  let v_index = get_index v (fst graph) in
  let rec aux (vi: int) (edges: g) : 'a vertices =
    match vi with
    | 0 -> get_actual_vertices (List.hd edges) (fst graph)
    | _ -> aux (vi - 1) (List.tl edges)
  in aux (v_index) (snd graph)
