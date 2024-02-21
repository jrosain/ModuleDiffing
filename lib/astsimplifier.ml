open Parsetree;;
open Asttypes;;

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

and type_exception_annotation =
  {
    ptyexn_name: string loc;
    ptyexn_kind : extension_constructor_kind;
    ptyexn_loc: Location.t;
    (* NOTE: no attributes *)
  }

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

and annotation = 
  |Aval of value_description_annotation
  |Atype of type_declaration_annotation
  |Aexn of type_exception_annotation
  |Amod_decl of module_declaration_annotation
  |Amod_type of module_type_annotation

and annotation_tree = 
  |Leaf of annotation
  |Node of annotation * (annotation_tree list)


let get_value_info (v : value_description) : annotation_tree (*Leaf*) = 
  Leaf(Aval({pval_name = v.pval_name; pval_type = v.pval_type; pval_loc = v.pval_loc}));;

let rec get_type_info (tl : type_declaration list) : annotation_tree list (*Multiple leaves*) = 
  match tl with
    |[] -> []
    |t::ts -> (Leaf(Atype({ptype_name = t.ptype_name; ptype_params = t.ptype_params; ptype_kind = t.ptype_kind;
                           ptype_private = t.ptype_private; ptype_loc = t.ptype_loc})))::(get_type_info ts);;

let get_exception_info (e : type_exception) : annotation_tree (*Leaf*) = 
  let ec = e.ptyexn_constructor in (*We go through one level*)
  Leaf(Aexn({ptyexn_name = ec.pext_name; ptyexn_kind = ec.pext_kind; ptyexn_loc = ec.pext_loc}));;
                          
                          

(*
let tree_builder (s : signature) : annotation_tree = 
  let (shead::_) = s in (* Only one signature in the list *)
*)