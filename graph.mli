(** This file implements different versions of a graph structure.

    It provides some elementary operations such as:
      * Insertion / Deletion of a vertex.
      * Insertion / Deletion of an edge.
      * Getter of the list of vertices.
      * Getter of the neighbours of a vertex.

    The goal of this file is to offer an interface of a graph which doesn't change when the 
    background data structures used to implement it are changed. *)

type 'a vertices
type 'a graph

val empty: unit -> 'a graph
val add_vertex: 'a -> 'a graph -> 'a graph
val add_edge: 'a -> 'a -> float -> 'a graph -> 'a graph
val remove_vertex: 'a -> 'a graph -> 'a graph
val remove_edge: 'a -> 'a -> 'a graph -> 'a graph
val vertices: 'a graph -> 'a vertices
val neighbours: 'a -> 'a graph -> 'a vertices
