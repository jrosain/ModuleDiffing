open Pprintast;;
(*
type tleaf = 
   | Val of value_description
   | Type of type_declaration
   | Exception of type_exception
   | Module_alias of module_substitution
   | ModuleType_nonsig of module_type_declaration
;;

type stree =
   | Leaf of tleaf
   | Node of module_declaration * stree list
;;
*)

let s = Parse.interface (Lexing.from_string "module type Hello_type =
   sig
     val hello : unit -> unit
   end") in 

Format.printf "%a" Pprintast.interface s;;

(*
let rec tree_builder (s : signature) : stree = 
   let (shead::_) = s in (* Only one signature in the list *)
      
*)