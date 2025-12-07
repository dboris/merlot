(** Doc_extract - Extract documentation from OCaml typed tree

    Extracts doc comments (ocaml.doc attributes) and generates
    EEP-48 compatible documentation chunks for the BEAM.
*)

(** Documentation entry for a function or type *)
type doc_entry = {
  name: string;
  arity: int;
  kind: [`Function | `Type | `Callback];
  signature: string list;
  doc: string option;
  line: int;
}

(** Module documentation *)
type module_doc = {
  module_name: string;
  module_doc: string option;
  entries: doc_entry list;
}

(** Extract doc string from ocaml.doc attribute *)
let extract_doc_from_attributes (attrs : Parsetree.attributes) : string option =
  let open Parsetree in
  List.find_map (fun attr ->
    if attr.attr_name.txt = "ocaml.doc" then
      match attr.attr_payload with
      | PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant {pconst_desc = Pconst_string (s, _, _); _}; _}, _); _}] ->
          Some (String.trim s)
      | _ -> None
    else None
  ) attrs

(** Extract floating doc comments (ocaml.text) from structure items *)
let extract_floating_doc (item : Typedtree.structure_item) : string option =
  match item.str_desc with
  | Tstr_attribute attr ->
      if attr.attr_name.txt = "ocaml.text" then
        match attr.attr_payload with
        | Parsetree.PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant {pconst_desc = Pconst_string (s, _, _); _}; _}, _); _}] ->
            Some (String.trim s)
        | _ -> None
      else None
  | _ -> None

(** Get signature string for a type *)
let rec type_to_signature (ty : Types.type_expr) : string =
  let open Types in
  match get_desc ty with
  | Tvar _ -> "'a"
  | Tarrow (_, arg, ret, _) ->
      let arg_s = type_to_signature arg in
      let ret_s = type_to_signature ret in
      Printf.sprintf "%s -> %s" arg_s ret_s
  | Ttuple tys ->
      (* Tuples now have optional labels: (label option * type_expr) list *)
      let parts = List.map (fun (_, ty) -> type_to_signature ty) tys in
      Printf.sprintf "(%s)" (String.concat " * " parts)
  | Tconstr (path, [], _) ->
      Path.name path
  | Tconstr (path, args, _) ->
      let args_s = List.map type_to_signature args in
      Printf.sprintf "%s %s" (String.concat ", " args_s) (Path.name path)
  | Tlink ty -> type_to_signature ty
  | _ -> "_"

(** Compute arity from function type *)
let rec arity_of_type ty =
  let open Types in
  match get_desc ty with
  | Tarrow (_, _, ret, _) -> 1 + arity_of_type ret
  | _ -> 0

(** Extract documentation from a value binding *)
let extract_value_doc (vb : Typedtree.value_binding) : doc_entry option =
  match vb.vb_pat.pat_desc with
  | Tpat_var (id, _, _) ->
      let name = Ident.name id in
      let doc = extract_doc_from_attributes vb.vb_attributes in
      let ty = vb.vb_expr.exp_type in
      let arity = max 1 (arity_of_type ty) in
      let signature = [type_to_signature ty] in
      let line = vb.vb_loc.loc_start.pos_lnum in
      Some {
        name;
        arity;
        kind = `Function;
        signature;
        doc;
        line;
      }
  | _ -> None

(** Extract documentation from a type declaration *)
let extract_type_doc (td : Typedtree.type_declaration) : doc_entry option =
  let name = Ident.name td.typ_id in
  let doc = extract_doc_from_attributes td.typ_attributes in
  let line = td.typ_loc.loc_start.pos_lnum in
  Some {
    name;
    arity = 0;
    kind = `Type;
    signature = [name];
    doc;
    line;
  }

(** Extract all documentation from a structure *)
let extract_module_doc (module_name : string) (str : Typedtree.structure) : module_doc =
  let module_doc = ref None in
  let entries = ref [] in

  List.iter (fun item ->
    (* Check for floating doc that might be module doc *)
    (match extract_floating_doc item with
     | Some doc when !module_doc = None -> module_doc := Some doc
     | _ -> ());

    (* Extract from value bindings *)
    (match item.str_desc with
     | Tstr_value (_, bindings) ->
         List.iter (fun vb ->
           match extract_value_doc vb with
           | Some entry -> entries := entry :: !entries
           | None -> ()
         ) bindings
     | Tstr_type (_, decls) ->
         List.iter (fun td ->
           match extract_type_doc td with
           | Some entry -> entries := entry :: !entries
           | None -> ()
         ) decls
     | _ -> ())
  ) str.str_items;

  {
    module_name;
    module_doc = !module_doc;
    entries = List.rev !entries;
  }

(** Format a single doc entry as Erlang term string for EEP-48 *)
let format_doc_entry (entry : doc_entry) : string =
  let kind = match entry.kind with
    | `Function -> "function"
    | `Type -> "type"
    | `Callback -> "callback"
  in
  let doc_value = match entry.doc with
    | Some d -> Printf.sprintf "#{<<\"en\">> => <<\"%s\">>}" (String.escaped d)
    | None -> "none"
  in
  let signature = match entry.signature with
    | [] -> "[]"
    | sigs -> Printf.sprintf "[%s]"
        (String.concat ", " (List.map (fun s -> Printf.sprintf "<<\"%s\">>" (String.escaped s)) sigs))
  in
  Printf.sprintf "{{%s, '%s', %d}, {%d, 0}, %s, %s, #{}}"
    kind entry.name entry.arity entry.line signature doc_value

(** Generate EEP-48 docs_v1 chunk as Erlang term string *)
let generate_docs_chunk (doc : module_doc) : string =
  let module_doc_value = match doc.module_doc with
    | Some d -> Printf.sprintf "#{<<\"en\">> => <<\"%s\">>}" (String.escaped d)
    | None -> "none"
  in
  let entries = List.map format_doc_entry doc.entries in
  Printf.sprintf "{docs_v1, {0, 0}, 'merlot', <<\"text/markdown\">>, %s, #{}, [%s]}"
    module_doc_value
    (String.concat ",\n  " entries)

(** Write docs chunk file (can be loaded alongside .beam) *)
let write_docs_chunk (filename : string) (doc : module_doc) : unit =
  let content = generate_docs_chunk doc in
  let oc = open_out filename in
  output_string oc content;
  close_out oc

(** Generate human-readable markdown documentation *)
let generate_markdown (doc : module_doc) : string =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf (Printf.sprintf "# Module `%s`\n\n" doc.module_name);

  (match doc.module_doc with
   | Some d -> Buffer.add_string buf (Printf.sprintf "%s\n\n" d)
   | None -> ());

  let functions = List.filter (fun e -> e.kind = `Function) doc.entries in
  let types = List.filter (fun e -> e.kind = `Type) doc.entries in

  if types <> [] then begin
    Buffer.add_string buf "## Types\n\n";
    List.iter (fun entry ->
      Buffer.add_string buf (Printf.sprintf "### `%s`\n\n" entry.name);
      (match entry.doc with
       | Some d -> Buffer.add_string buf (Printf.sprintf "%s\n\n" d)
       | None -> ())
    ) types
  end;

  if functions <> [] then begin
    Buffer.add_string buf "## Functions\n\n";
    List.iter (fun entry ->
      let sig_str = String.concat "" entry.signature in
      Buffer.add_string buf (Printf.sprintf "### `%s/%d`\n\n" entry.name entry.arity);
      Buffer.add_string buf (Printf.sprintf "```ocaml\nval %s : %s\n```\n\n" entry.name sig_str);
      (match entry.doc with
       | Some d -> Buffer.add_string buf (Printf.sprintf "%s\n\n" d)
       | None -> ())
    ) functions
  end;

  Buffer.contents buf

(** Write markdown documentation file *)
let write_markdown (filename : string) (doc : module_doc) : unit =
  let content = generate_markdown doc in
  let oc = open_out filename in
  output_string oc content;
  close_out oc
