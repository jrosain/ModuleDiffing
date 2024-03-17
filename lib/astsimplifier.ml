open Parsetree;;
open Asttypes;;

module AnnotationTree = struct

  type toplevel_annotation =
    {
      tplvl_name: string;
    }
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
    |Atplvl of toplevel_annotation
    |Aval of value_description_annotation
    |Atype of type_declaration_annotation
    |Aexn of type_exception_annotation
    |Amod_decl of module_declaration_annotation

  and annotation_node = int * annotation

  and annotation_tree =
    |Leaf of annotation_node
    |Node of annotation_node * (annotation_tree list)

  type v = int
  type node = annotation_node
  type t = annotation_tree

  let parent (tree : t) (n : node) : node option =
    let rec parent_lst (tree : t list) (n : node) = match tree with
      | (Node((id', a), l)) :: _ when (List.mem l n)  -> Some(id',a)
      | (Node(_, l)) :: q -> begin
          match parent_lst l n with
          | None -> parent_lst q n
          | Some p -> Some p
        end
      | _ -> None
    in
    parent_lst [tree] n

  let children (tree : t) (n : node) : node list =
    let rec children_lst (tree : t list) (n : node) : node list option =
      match tree with
      | [] -> None
      | (Node(n',l)) :: _  when n' = n -> Some(List.map (fun x -> match x with | Node(n,_) -> n | Leaf(n) -> n) l)
      | (Node(_,l)) :: q -> begin
          match children_lst l n with
          | None -> children_lst q n
          | Some(n') -> Some(n')
        end
      | (Leaf(n')) :: _  when n' = n -> Some([])
      | _ :: q -> children_lst q n
    in
    match children_lst [tree] n with
    | None -> failwith "AnnotationTree.children : node not found"
    | Some(l) -> l

  let elements (tree : t) : node list =
    let rec elements_lst (acc : node list) (tree : t list) = match tree with
      | [] -> acc
      | Leaf(n) :: q -> elements_lst (n::acc) q
      | Node(n,l) :: q -> elements_lst (elements_lst (n::acc) l) q
    in
    elements_lst [] [tree]

  let compare (n1 : node) (n2 : node) : Cost.t = Cost.of_int 0 (*TODO: implement correctly*)

  (* NOTE: Some module declerations may have an empty name and therefor empty label *)
  let label (n : node) : string =
    match (snd n) with
    | Atplvl(a) -> a.tplvl_name
    | Aval(a) -> a.pval_name.txt
    | Atype(a) -> a.ptype_name.txt
    | Aexn(a) -> a.ptyexn_name.txt
    | Amod_decl(a) -> match a.pmd_name.txt with
      | Some (s) -> s
      | None -> ""

  let value (n : node) : v = fst n
  let root (tree : t) : node = match tree with Node(n, _) -> n | Leaf(n) -> n
end

module type SimplifiedAST = sig
  include Sig.INPUT
end

module SimplifiedAST = struct

  type i = signature
  type v = AnnotationTree.v
  type node = AnnotationTree.node
  type t = AnnotationTree.t

  let id_counter = ref 1 (* 0 is reserved for the toplevel (see "create" function) *)

  let new_id = id_counter := !id_counter + 1; !id_counter

  let rec create_forest (s : i) : t list =
    match s with
    |[] -> []
    |si::sis -> match si.psig_desc with
      | Psig_value(v) -> (get_value_info v)::(create_forest sis)
      | Psig_type(_ , tl) -> (get_type_info tl) @ (create_forest sis)
      | Psig_exception(e) -> (get_exception_info e)::(create_forest sis)
      | Psig_module(md) -> (get_module_declaration_info md)::(create_forest sis)
      | Psig_recmodule(mdl) -> (get_module_declarations_info mdl) @ (create_forest sis)
      (* TODO: | Psig_modtype(mt) -> (get_module_type_info)::(create_forest sis) *)
      | _ -> failwith "unsupported signature item"
  (* NOTE: no typesubst, typext, modsubst, modtypesubst, open, include, class, class_type, attribute, extension*)

  and get_value_info (v : value_description) : t (*Leaf*) =
    Leaf(new_id, Aval({pval_name = v.pval_name; pval_type = v.pval_type; pval_loc = v.pval_loc}))

  and get_type_info (tl : type_declaration list) : t list (*Multiple leaves*) =
    match tl with
    |[] -> []
    |t::ts -> (Leaf(nex_id, Atype({ptype_name = t.ptype_name;
                                   ptype_params = t.ptype_params;
                                   ptype_kind = t.ptype_kind;
                                   ptype_private = t.ptype_private;
                                   ptype_loc = t.ptype_loc})))::(get_type_info ts)

  and get_exception_info (e : type_exception) : t (*Leaf*) =
    let ec = e.ptyexn_constructor in (*We go through one level*)
    Leaf(new_id, Aexn({ptyexn_name = ec.pext_name; ptyexn_kind = ec.pext_kind; ptyexn_loc = ec.pext_loc}))

  and get_module_declaration_info (md : module_declaration) : t (*Leaf unless Signature*) =
    (* Check wether the module type is a signature *)
    let descriptor, s_opt = match md.pmd_type.pmty_desc with
      | Pmty_ident(_) -> AnnotationTree.Ident, None
      | Pmty_signature(s) -> AnnotationTree.Signature, Some(s)
      | Pmty_functor(_) -> AnnotationTree.Functor, None
      | Pmty_with(_,_) -> AnnotationTree.With, None
      | Pmty_typeof(_) -> AnnotationTree.TypeOf, None
      | Pmty_extension(_) -> AnnotationTree.Extension, None
      | Pmty_alias(_) -> AnnotationTree.Alias, None
    in
    (* In any case the annotation is the same *)
    let mod_decl_annot = AnnotationTree.Amod_decl({pmd_name = md.pmd_name;
                                    pmd_loc = md.pmd_loc;
                                    pmty_desc = descriptor;
                                   })
    in
    (* If it is a signature, recursively build a subtree *)
    match s_opt with
    | None -> Leaf(new_id, mod_decl_annot)
    | Some(s) -> Node((new_id, mod_decl_annot), create_forest(s))

  (** Naively go through module declarations. TODO: recursion ? *)
  and get_module_declarations_info (mdl : module_declaration list) : t list = List.map get_module_declaration_info mdl

  let create (s : i) : t = AnnotationTree.Node((0 ,AnnotationTree.Atplvl({tplvl_name = "toplevel"})), create_forest s)
  let parent (tree : t) (n : node) : node option = AnnotationTree.parent tree n
  let children (tree : t) (n : node) : node list = AnnotationTree.children tree n
  let elements (tree : t) : node list = AnnotationTree.elements tree
  let compare (n1 : node) (n2 : node) : Cost.t = AnnotationTree.compare n1 n2
  let label (n : node) : string = AnnotationTree.label n
  let value (n : node) : v = AnnotationTree.value n
  let root (tree : t) : node = AnnotationTree.root tree
end

;;
