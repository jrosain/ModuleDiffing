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

type value_description_annotation =
  {
    pval_name: string loc;
    pval_type: core_type;
    pval_loc: Location.t;
    (* NOTE: no prim, attributes *)
  }

and type_declaration_annotation = (* TODO: These come in lists w/ rec flag, careful when parsing *)
  {
    ptype_name: string loc;
    ptype_params: (core_type * (variance * injectivity)) list;
    ptype_kind: type_kind;
    ptype_private: private_flag;
    ptype_loc: Location.t;
    (* NOTE: no attributes, manifest, cstrs *)
  }

(* TODO: type_exception *)

and module_declaration_annotation =
  {
     pmd_name: string option loc;
     pmd_loc: Location.t;
     (* NOTE: no attributes, type *)
     (* Module types are handled in child nodes *)
  }

and module_type_annotation =
    {
     pmty_desc: module_type_desc;
     pmty_loc: Location.t;
     (* NOTE: no attributes *)
    }


let s = Parse.interface (Lexing.from_string "module type Hello_type =
  sig
    val hello : unit -> unit
  end") in 

Format.printf "%a" Pprintast.signature s;;

(*
let rec tree_builder (s : signature) : stree = 
  let (shead::_) = s in (* Only one signature in the list *)
     
*)
