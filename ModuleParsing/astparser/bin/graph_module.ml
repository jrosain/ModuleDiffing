module Graph = 
sig 
  module Node = 
    sig 
      type color
      type t
      val get_color : t -> color
      val create : color -> t
    end 
  module Edge = 
    sig 
      type weight
      type t
      val get_weight : 
      val 
    end 
  type t
  val count_color : t -> ((Node.color * int) list)
  val add_edge : t -> Edge.t -> t
  val add_vertex : t -> Node.t -> t   
end