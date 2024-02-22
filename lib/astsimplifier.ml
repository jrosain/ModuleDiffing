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
     pmty_desc: module_type_descriptor;
     (* NOTE: no attributes, type *)
     (* The type is in the descriptor *)
     (* The loc is that of the declaration and not of the type *)
  }

and module_type_descriptor = (* TODO: these should probably all have their own (short) annotation type *)
  | Ident
  | Signature
  | Functor
  | With
  | TypeOf
  | Extension
  | Alias
  | Empty (* This is not a real module_type, but a module type declaration may be empty *)

and annotation =
  |Aval of value_description_annotation
  |Atype of type_declaration_annotation
  |Aexn of type_exception_annotation
  |Amod_decl of module_declaration_annotation

and annotation_tree = 
  |Leaf of annotation
  |Node of annotation * (annotation_tree list)



let rec build_simplified_ast (s : signature) : annotation_tree list =
  match s with
  |[] -> []
  |si::sis -> match si.psig_desc with
    | Psig_value(v) -> (get_value_info v)::(build_simplified_ast sis)
    | Psig_type(_ , tl) -> (get_type_info tl) @ (build_simplified_ast sis)
    | Psig_exception(e) -> (get_exception_info e)::(build_simplified_ast sis)
    | Psig_module(md) -> (get_module_declaration_info md)::(build_simplified_ast sis)
    | Psig_recmodule(mdl) -> (get_module_declarations_info mdl) @ (build_simplified_ast sis)
    (* TODO: | Psig_modtype(mt) -> (get_module_type_info)::(build_simplified_ast sis) *)
    | _ -> failwith "unsupported signature item"
    (* NOTE: no typesubst, typext, modsubst, modtypesubst, open, include, class, class_type, attribute, extension*)

and get_value_info (v : value_description) : annotation_tree (*Leaf*) =
  Leaf(Aval({pval_name = v.pval_name; pval_type = v.pval_type; pval_loc = v.pval_loc}))

and get_type_info (tl : type_declaration list) : annotation_tree list (*Multiple leaves*) =
  match tl with
    |[] -> []
    |t::ts -> (Leaf(Atype({ptype_name = t.ptype_name;
                           ptype_params = t.ptype_params;
                           ptype_kind = t.ptype_kind;
                           ptype_private = t.ptype_private;
                           ptype_loc = t.ptype_loc})))::(get_type_info ts)

and get_exception_info (e : type_exception) : annotation_tree (*Leaf*) =
  let ec = e.ptyexn_constructor in (*We go through one level*)
  Leaf(Aexn({ptyexn_name = ec.pext_name; ptyexn_kind = ec.pext_kind; ptyexn_loc = ec.pext_loc}))
                          
and get_module_declaration_info (md : module_declaration) : annotation_tree (*Leaf unless Signature*) =
  (* Check wether the module type is a signature *)
  let descriptor, s_opt = match md.pmd_type.pmty_desc with
    | Pmty_ident(_) -> Ident, None
    | Pmty_signature(s) -> Signature, Some(s)
    | Pmty_functor(_) -> Functor, None
    | Pmty_with(_,_) -> With, None
    | Pmty_typeof(_) -> TypeOf, None
    | Pmty_extension(_) -> Extension, None
    | Pmty_alias(_) -> Alias, None
  in
  (* In any case the annotation is the same *)
  let mod_decl_annot = Amod_decl({pmd_name = md.pmd_name;
                                 pmd_loc = md.pmd_loc;
                                 pmty_desc = descriptor;
                                })
  in
  (* If it is a signature, recursively build a subtree *)
  match s_opt with
  | None -> Leaf(mod_decl_annot)
  | Some(s) -> Node(mod_decl_annot, build_simplified_ast(s))

(** Naively go through module declarations. TODO: recursion ? *)
and get_module_declarations_info (mdl : module_declaration list) : annotation_tree list = List.map get_module_declaration_info mdl

;;
