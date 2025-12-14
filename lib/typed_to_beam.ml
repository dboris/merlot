(** Typed_to_beam - Convert OCaml TypedTree to BEAM IR

    This is the core transformation that maps OCaml's typed AST
    to our stable BEAM intermediate representation.
*)

open Beam_ir
open Data_types

(** Elixir struct mode - when true, module uses Elixir-compatible conventions:
    - Module prefix is "Elixir." instead of "Merlot."
    - Records compile to maps with __struct__ key instead of tagged tuples
    - __struct__/0 and __struct__/1 functions are auto-generated *)
let elixir_struct_mode = ref false

(** Track record type info for __struct__ generation when in elixir_struct mode *)
type record_info = {
  field_names: string list;
}
let elixir_struct_record : record_info option ref = ref None

(** Store the module name for use in elixir_struct mode *)
let current_module_name : string ref = ref ""

(** Elixir type imports - types marked with [@@elixir] are compiled as maps.
    Maps type name to optional explicit Elixir module name.
    Example: "user" -> None means infer Elixir.User
             "response" -> Some "Elixir.Plug.Conn.Response" for explicit name *)
let elixir_types : (string, string option) Hashtbl.t = Hashtbl.create 16

let reset_elixir_types () = Hashtbl.clear elixir_types

let register_elixir_type type_name explicit_module =
  Hashtbl.add elixir_types type_name explicit_module

let is_elixir_type type_name =
  Hashtbl.mem elixir_types type_name

let reset_elixir_struct () =
  elixir_struct_mode := false;
  elixir_struct_record := None;
  current_module_name := "";
  reset_elixir_types ()

(** Check if a structure item is the [@@@elixir_struct] attribute *)
let is_elixir_struct_attribute (item : Typedtree.structure_item) : bool =
  match item.str_desc with
  | Tstr_attribute attr ->
      attr.attr_name.txt = "elixir_struct"
  | _ -> false

(** Scan structure for [@@@elixir_struct] attribute *)
let has_elixir_struct_attribute (str : Typedtree.structure) : bool =
  List.exists is_elixir_struct_attribute str.str_items

(** Add the appropriate prefix to a module name.
    Uses "Elixir." in elixir_struct mode, otherwise "Merlot.".
    Example: "person" -> "Elixir.Person" or "Merlot.Person" *)
let prefixed_module_name name =
  let prefix = if !elixir_struct_mode then "Elixir." else "Merlot." in
  prefix ^ String.capitalize_ascii name

(** Fresh variable generation *)
let var_counter = ref 0

let fresh_var prefix =
  incr var_counter;
  Printf.sprintf "%s_%d" prefix !var_counter

let reset_vars () = var_counter := 0

(** Variable shadowing: track bound variables and rename duplicates in patterns *)
module StringSet = Set.Make(String)

let pattern_vars_seen = ref StringSet.empty
let pattern_renames : (string, string) Hashtbl.t = Hashtbl.create 16

let reset_pattern_vars () =
  pattern_vars_seen := StringSet.empty;
  Hashtbl.clear pattern_renames

(** Get unique variable name, renaming if shadowed *)
let get_unique_pattern_var name =
  let erlang_name =
    if name = "" then "_Anon"
    else if name.[0] = '_' then name
    else String.capitalize_ascii name
  in
  if StringSet.mem erlang_name !pattern_vars_seen then begin
    (* Variable is shadowed - create a fresh name *)
    let fresh = fresh_var erlang_name in
    Hashtbl.add pattern_renames name fresh;
    pattern_vars_seen := StringSet.add fresh !pattern_vars_seen;
    fresh
  end else begin
    pattern_vars_seen := StringSet.add erlang_name !pattern_vars_seen;
    erlang_name
  end

(** External declarations map: unique ident name -> (module, function) *)
let externals : (string, string * string) Hashtbl.t = Hashtbl.create 16

let reset_externals () = Hashtbl.clear externals

let register_external id prim =
  match prim with
  | [mod_name; func_name] ->
      Hashtbl.add externals (Ident.unique_name id) (mod_name, func_name)
  | [func_name] ->
      (* Single string means erlang module *)
      Hashtbl.add externals (Ident.unique_name id) ("erlang", func_name)
  | _ -> ()

let lookup_external id =
  Hashtbl.find_opt externals (Ident.unique_name id)

(** Top-level functions map: unique_name -> (display_name, arity)
    We use unique_name as key to distinguish identically-named bindings
    (e.g., local variable 'name' vs top-level function 'name') *)
let top_level_functions : (string, string * int) Hashtbl.t = Hashtbl.create 16

let reset_top_level_functions () = Hashtbl.clear top_level_functions

let register_top_level_function id arity =
  Hashtbl.add top_level_functions (Ident.unique_name id) (Ident.name id, arity)

let lookup_top_level_function id =
  Hashtbl.find_opt top_level_functions (Ident.unique_name id)

(** Compute function arity from its type *)
let rec arity_of_type ty =
  let ty = Types.get_desc ty in
  match ty with
  | Types.Tarrow (_, _, ret, _) -> 1 + arity_of_type ret
  | _ -> 0

(** Sanitize a name to be valid in Core Erlang.
    Core Erlang variable names can only contain alphanumeric characters and underscore.
    OCaml allows apostrophes in names (e.g., x', r') which must be converted. *)
let sanitize_erlang_name (name : string) : string =
  (* Replace ' with _prime, other invalid chars with _ *)
  let buf = Buffer.create (String.length name) in
  String.iter (fun c ->
    if c = '\'' then Buffer.add_string buf "_prime"
    else if c >= 'a' && c <= 'z' then Buffer.add_char buf c
    else if c >= 'A' && c <= 'Z' then Buffer.add_char buf c
    else if c >= '0' && c <= '9' then Buffer.add_char buf c
    else if c = '_' then Buffer.add_char buf c
    else Buffer.add_char buf '_'
  ) name;
  Buffer.contents buf

(** Convert OCaml identifier to Erlang-safe name *)
let convert_ident (id : Ident.t) : string =
  let name = Ident.name id in
  (* Erlang variables must start with uppercase or _ *)
  let sanitized = sanitize_erlang_name name in
  if sanitized = "" then "_Anon"
  else if sanitized.[0] = '_' then sanitized
  else String.capitalize_ascii sanitized

(** Convert a Path.t to a module/function reference *)
let rec path_to_string (path : Path.t) : string =
  match path with
  | Pident id -> convert_ident id
  | Pdot (p, s) -> path_to_string p ^ "." ^ s
  | Papply _ -> failwith "Papply not supported"
  | Pextra_ty _ -> failwith "Pextra_ty not supported"

(* ============================================================================
   Pretty-print function generation
   ============================================================================ *)

(** Generate a BEAM expression that formats a type as an OCaml-style string.
    Returns an expression that evaluates to a string representation. *)
let rec generate_pp_expr_for_type (ty : Types.type_expr) (value_expr : expr) : expr =
  match Types.get_desc ty with
  | Types.Tconstr (path, _, _) ->
      let type_name = Path.last path in
      (match type_name with
       | "int" ->
           (* Use Integer.to_string for integers *)
           Expr_call {
             module_ = Expr_lit (Lit_atom "erlang");
             func = Expr_lit (Lit_atom "integer_to_binary");
             args = [value_expr];
           }
       | "float" ->
           (* Use io_lib:format with ~g for clean float formatting *)
           Expr_call {
             module_ = Expr_lit (Lit_atom "erlang");
             func = Expr_lit (Lit_atom "iolist_to_binary");
             args = [Expr_call {
               module_ = Expr_lit (Lit_atom "io_lib");
               func = Expr_lit (Lit_atom "format");
               args = [Expr_lit (Lit_string "~g"); Expr_list [value_expr]];
             }];
           }
       | "string" ->
           (* Wrap string in quotes *)
           Expr_call {
             module_ = Expr_lit (Lit_atom "erlang");
             func = Expr_lit (Lit_atom "iolist_to_binary");
             args = [Expr_list [
               Expr_lit (Lit_string "\"");
               value_expr;
               Expr_lit (Lit_string "\"");
             ]];
           }
       | "bool" ->
           (* Convert boolean atom to string *)
           Expr_call {
             module_ = Expr_lit (Lit_atom "erlang");
             func = Expr_lit (Lit_atom "atom_to_binary");
             args = [value_expr];
           }
       | "unit" ->
           Expr_lit (Lit_string "()")
       | "list" ->
           (* For lists, use Inspect fallback for now *)
           generate_inspect_fallback value_expr
       | "option" ->
           (* For options, use Inspect fallback *)
           generate_inspect_fallback value_expr
       | _ ->
           (* For user-defined types, call local pp_<typename> function *)
           Expr_local_call {
             func_name = "pp_" ^ String.lowercase_ascii type_name;
             arity = 1;
             args = [value_expr];
           })
  | Types.Ttuple tys ->
      (* For tuples, format each element *)
      generate_tuple_pp tys value_expr
  | Types.Tlink ty ->
      generate_pp_expr_for_type ty value_expr
  | Types.Tpoly (ty, _) ->
      (* Unwrap polymorphic type scheme *)
      generate_pp_expr_for_type ty value_expr
  | Types.Tvar _ ->
      (* For polymorphic types, use Inspect fallback *)
      generate_inspect_fallback value_expr
  | _ ->
      (* Fallback: use Erlang's ~p format *)
      generate_inspect_fallback value_expr

(** Generate Inspect.to_string fallback for unknown types *)
and generate_inspect_fallback value_expr =
  Expr_call {
    module_ = Expr_lit (Lit_atom "erlang");
    func = Expr_lit (Lit_atom "iolist_to_binary");
    args = [Expr_call {
      module_ = Expr_lit (Lit_atom "io_lib");
      func = Expr_lit (Lit_atom "format");
      args = [Expr_lit (Lit_string "~p"); Expr_list [value_expr]];
    }];
  }

(** Generate tuple pretty-printing: (a, b, c) -> "(a_str, b_str, c_str)" *)
and generate_tuple_pp tys value_expr =
  let n = List.length tys in
  if n = 0 then Expr_lit (Lit_string "()")
  else
    (* Extract elements and format each *)
    let parts = List.mapi (fun i (_, ty) ->
      let elem_var = Printf.sprintf "Elem_%d" i in
      let elem_expr = Expr_call {
        module_ = Expr_lit (Lit_atom "erlang");
        func = Expr_lit (Lit_atom "element");
        args = [Expr_lit (Lit_int (i + 1)); value_expr];
      } in
      (elem_var, generate_pp_expr_for_type ty elem_expr)
    ) tys in
    (* Build string: "(" ++ elem1 ++ ", " ++ elem2 ++ ... ++ ")" *)
    let concat_all parts =
      let rec build = function
        | [] -> Expr_lit (Lit_string ")")
        | [(_, pp_expr)] ->
            Expr_call {
              module_ = Expr_lit (Lit_atom "erlang");
              func = Expr_lit (Lit_atom "iolist_to_binary");
              args = [Expr_list [pp_expr; Expr_lit (Lit_string ")")]];
            }
        | (_, pp_expr) :: rest ->
            Expr_call {
              module_ = Expr_lit (Lit_atom "erlang");
              func = Expr_lit (Lit_atom "iolist_to_binary");
              args = [Expr_list [pp_expr; Expr_lit (Lit_string ", "); build rest]];
            }
      in
      Expr_call {
        module_ = Expr_lit (Lit_atom "erlang");
        func = Expr_lit (Lit_atom "iolist_to_binary");
        args = [Expr_list [Expr_lit (Lit_string "("); build parts]];
      }
    in
    concat_all parts

(** Generate a pp function for a record type *)
let generate_pp_for_record type_name (labels : Typedtree.label_declaration list) : fun_def =
  let param = "V" in
  (* Generate: "{field1 = val1; field2 = val2; ...}" *)
  let field_parts = List.mapi (fun i (ld : Typedtree.label_declaration) ->
    let field_name = Ident.name ld.ld_id in
    (* Access field: element(i+2, V) for tagged tuples, or maps:get for maps *)
    let field_access = Expr_call {
      module_ = Expr_lit (Lit_atom "erlang");
      func = Expr_lit (Lit_atom "element");
      args = [Expr_lit (Lit_int (i + 2)); Expr_var param];  (* +2: skip tag and 1-indexing *)
    } in
    let field_pp = generate_pp_expr_for_type ld.ld_type.ctyp_type field_access in
    (field_name, field_pp)
  ) labels in
  (* Build: "{" ++ "name = " ++ pp_name ++ "; " ++ ... ++ "}" *)
  let build_record_string parts =
    let rec build first = function
      | [] -> Expr_lit (Lit_string "}")
      | [(name, pp_expr)] ->
          let sep = if first then "" else "; " in
          Expr_call {
            module_ = Expr_lit (Lit_atom "erlang");
            func = Expr_lit (Lit_atom "iolist_to_binary");
            args = [Expr_list [
              Expr_lit (Lit_string (sep ^ name ^ " = "));
              pp_expr;
              Expr_lit (Lit_string "}");
            ]];
          }
      | (name, pp_expr) :: rest ->
          let sep = if first then "" else "; " in
          Expr_call {
            module_ = Expr_lit (Lit_atom "erlang");
            func = Expr_lit (Lit_atom "iolist_to_binary");
            args = [Expr_list [
              Expr_lit (Lit_string (sep ^ name ^ " = "));
              pp_expr;
              build false rest;
            ]];
          }
    in
    Expr_call {
      module_ = Expr_lit (Lit_atom "erlang");
      func = Expr_lit (Lit_atom "iolist_to_binary");
      args = [Expr_list [Expr_lit (Lit_string "{"); build true parts]];
    }
  in
  {
    name = "pp_" ^ String.lowercase_ascii type_name;
    arity = 1;
    params = [param];
    body = build_record_string field_parts;
    loc = no_loc;
  }

(** Generate a pp function for a variant type *)
let generate_pp_for_variant type_name (constrs : Typedtree.constructor_declaration list) : fun_def =
  let param = "V" in
  (* Generate case expression matching each constructor *)
  let clauses = List.map (fun (cd : Typedtree.constructor_declaration) ->
    let cstr_name = Ident.name cd.cd_id in
    let cstr_atom = String.lowercase_ascii cstr_name in
    match cd.cd_args with
    | Cstr_tuple [] ->
        (* Nullary constructor: atom *)
        let pat = Pat_lit (Lit_atom cstr_atom) in
        let body = Expr_lit (Lit_string cstr_name) in
        (pat, None, body)
    | Cstr_tuple arg_types ->
        (* Constructor with args: {atom, arg1, arg2, ...} *)
        let n_args = List.length arg_types in
        let arg_vars = List.mapi (fun i _ -> Printf.sprintf "Arg_%d" i) arg_types in
        let pat = Pat_tuple (Pat_lit (Lit_atom cstr_atom) :: List.map (fun v -> Pat_var v) arg_vars) in
        (* Format: "Cstr_name (arg1, arg2, ...)" or "Cstr_name arg" for single arg *)
        let arg_pps = List.map2 (fun var ct ->
          generate_pp_expr_for_type ct.Typedtree.ctyp_type (Expr_var var)
        ) arg_vars arg_types in
        let body = if n_args = 1 then
          Expr_call {
            module_ = Expr_lit (Lit_atom "erlang");
            func = Expr_lit (Lit_atom "iolist_to_binary");
            args = [Expr_list [
              Expr_lit (Lit_string (cstr_name ^ " "));
              List.hd arg_pps;
            ]];
          }
        else
          (* Multiple args: format as tuple *)
          let args_str = List.fold_left (fun acc pp ->
            match acc with
            | None -> Some pp
            | Some prev ->
                Some (Expr_call {
                  module_ = Expr_lit (Lit_atom "erlang");
                  func = Expr_lit (Lit_atom "iolist_to_binary");
                  args = [Expr_list [prev; Expr_lit (Lit_string ", "); pp]];
                })
          ) None arg_pps in
          Expr_call {
            module_ = Expr_lit (Lit_atom "erlang");
            func = Expr_lit (Lit_atom "iolist_to_binary");
            args = [Expr_list [
              Expr_lit (Lit_string (cstr_name ^ " ("));
              (match args_str with Some e -> e | None -> Expr_lit (Lit_string ""));
              Expr_lit (Lit_string ")");
            ]];
          }
        in
        (pat, None, body)
    | Cstr_record _ ->
        (* Inline record constructor - use fallback *)
        let pat = Pat_var "_" in
        let body = generate_inspect_fallback (Expr_var param) in
        (pat, None, body)
  ) constrs in
  {
    name = "pp_" ^ String.lowercase_ascii type_name;
    arity = 1;
    params = [param];
    body = Expr_case { scrutinee = Expr_var param; clauses };
    loc = no_loc;
  }

(** Generate pp functions for a type declaration *)
let generate_pp_for_type (td : Typedtree.type_declaration) : fun_def option =
  let type_name = Ident.name td.typ_id in
  (* Skip types with type parameters for now - they need special handling *)
  if td.typ_params <> [] then None
  else match td.typ_kind with
  | Ttype_record labels ->
      Some (generate_pp_for_record type_name labels)
  | Ttype_variant constrs ->
      Some (generate_pp_for_variant type_name constrs)
  | Ttype_abstract ->
      (* Abstract types - can't generate pp *)
      None
  | Ttype_open ->
      (* Open types - can't generate pp *)
      None

(** Count the number of format arguments expected by a format string *)
let count_format_args fmt =
  let count = ref 0 in
  let i = ref 0 in
  while !i < String.length fmt do
    if fmt.[!i] = '%' && !i + 1 < String.length fmt then begin
      let next = fmt.[!i + 1] in
      match next with
      | '%' -> i := !i + 2  (* %% is an escaped %, not an arg *)
      | '.' ->
          (* Handle precision like %.2f - skip digits, then count *)
          let j = ref (!i + 2) in
          while !j < String.length fmt && fmt.[!j] >= '0' && fmt.[!j] <= '9' do
            incr j
          done;
          if !j < String.length fmt then begin
            incr count;
            i := !j + 1
          end else
            incr i
      | _ -> incr count; i := !i + 2
    end else
      incr i
  done;
  !count

(** Convert OCaml format specifiers to Erlang format codes *)
let convert_format_string fmt =
  (* Convert %d -> ~B, %s -> ~s, %f -> ~f, %b -> ~w, etc. *)
  let buf = Buffer.create (String.length fmt) in
  let i = ref 0 in
  while !i < String.length fmt do
    if fmt.[!i] = '%' && !i + 1 < String.length fmt then begin
      let next = fmt.[!i + 1] in
      match next with
      | '%' -> Buffer.add_char buf '%'; i := !i + 2
      | 'd' | 'i' -> Buffer.add_string buf "~B"; i := !i + 2  (* ~B for integers *)
      | 's' -> Buffer.add_string buf "~s"; i := !i + 2
      | 'f' -> Buffer.add_string buf "~f"; i := !i + 2
      | 'b' -> Buffer.add_string buf "~w"; i := !i + 2  (* bool as atom *)
      | 'c' -> Buffer.add_string buf "~c"; i := !i + 2
      | 'a' | 'p' -> Buffer.add_string buf "~p"; i := !i + 2  (* any term *)
      | '.' ->
          (* Handle precision like %.2f *)
          let j = ref (!i + 2) in
          while !j < String.length fmt && fmt.[!j] >= '0' && fmt.[!j] <= '9' do
            incr j
          done;
          if !j < String.length fmt then begin
            let prec = String.sub fmt (!i + 2) (!j - !i - 2) in
            let spec = fmt.[!j] in
            (match spec with
             | 'f' -> Buffer.add_string buf ("~." ^ prec ^ "f")
             | _ -> Buffer.add_string buf "~p");
            i := !j + 1
          end else begin
            Buffer.add_char buf '%';
            incr i
          end
      | _ -> Buffer.add_string buf "~p"; i := !i + 2  (* fallback to ~p *)
    end else begin
      Buffer.add_char buf fmt.[!i];
      incr i
    end
  done;
  Buffer.contents buf

(** Extract raw format string from a BEAM IR format expression.
    OCaml format strings compile to a tuple: (format_ast, raw_string) *)
let rec extract_format_string expr =
  match expr with
  | Expr_tuple [_fmt_ast; raw_str] ->
      (* Format is (format_structure, raw_string_as_charlist) *)
      extract_string_from_charlist raw_str
  | _ -> None

and extract_string_from_charlist expr =
  (* Convert Erlang charlist representation back to string *)
  let rec collect acc = function
    | Expr_lit Lit_nil -> Some (String.concat "" (List.rev acc))
    | Expr_cons (Expr_lit (Lit_int c), rest) ->
        collect (String.make 1 (Char.chr c) :: acc) rest
    | _ -> None
  in
  collect [] expr

(** Convert Printf.sprintf call to io_lib:format *)
let convert_printf_sprintf format_expr value_args =
  match extract_format_string format_expr with
  | Some fmt_str ->
      let erlang_fmt = convert_format_string fmt_str in
      let expected_args = count_format_args fmt_str in
      let provided_args = List.length value_args in
      if provided_args >= expected_args then
        (* Full application: all args provided *)
        let format_call = Expr_call {
          module_ = Expr_lit (Lit_atom "io_lib");
          func = Expr_lit (Lit_atom "format");
          args = [
            Expr_lit (Lit_string erlang_fmt);
            Expr_list value_args;
          ];
        } in
        Expr_call {
          module_ = Expr_lit (Lit_atom "lists");
          func = Expr_lit (Lit_atom "flatten");
          args = [format_call];
        }
      else
        (* Partial application: return a lambda that collects remaining args *)
        let missing = expected_args - provided_args in
        let param_names = List.init missing (fun i -> fresh_var (Printf.sprintf "Arg%d" (i + 1))) in
        let param_exprs = List.map (fun v -> Expr_var v) param_names in
        let all_args = value_args @ param_exprs in
        let format_call = Expr_call {
          module_ = Expr_lit (Lit_atom "io_lib");
          func = Expr_lit (Lit_atom "format");
          args = [
            Expr_lit (Lit_string erlang_fmt);
            Expr_list all_args;
          ];
        } in
        let body = Expr_call {
          module_ = Expr_lit (Lit_atom "lists");
          func = Expr_lit (Lit_atom "flatten");
          args = [format_call];
        } in
        (* Wrap in nested lambdas for currying *)
        List.fold_right (fun param acc ->
          Expr_fun { params = [param]; body = acc }
        ) param_names body
  | None ->
      (* Fallback: can't extract format string, return error *)
      failwith "Printf: could not extract format string"

(** Convert Printf.printf call to io:format *)
let convert_printf_printf format_expr value_args =
  match extract_format_string format_expr with
  | Some fmt_str ->
      let erlang_fmt = convert_format_string fmt_str in
      Expr_call {
        module_ = Expr_lit (Lit_atom "io");
        func = Expr_lit (Lit_atom "format");
        args = [
          Expr_lit (Lit_string erlang_fmt);
          Expr_list value_args;
        ];
      }
  | None ->
      failwith "Printf: could not extract format string"

(** Convert OCaml constant to BEAM literal *)
let convert_constant (const : Asttypes.constant) : literal =
  match const with
  | Const_int n -> Lit_int n
  | Const_char c -> Lit_char c
  | Const_string (s, _, _) -> Lit_string s
  | Const_float s -> Lit_float (float_of_string s)
  | Const_int32 n -> Lit_int (Int32.to_int n)
  | Const_int64 n -> Lit_int (Int64.to_int n)
  | Const_nativeint n -> Lit_int (Nativeint.to_int n)

(** Convert a typed pattern to BEAM pattern with variable shadowing detection *)
let rec convert_pattern_with_seen seen (pat : Typedtree.pattern) : pattern * StringSet.t =
  match pat.pat_desc with
  | Tpat_any -> (Pat_any, seen)
  | Tpat_var (id, _, _) ->
      let name = Ident.name id in
      let sanitized = sanitize_erlang_name name in
      let erlang_name =
        if sanitized = "" then "_Anon"
        else if sanitized.[0] = '_' then sanitized
        else String.capitalize_ascii sanitized
      in
      if StringSet.mem erlang_name seen then
        (* Variable is shadowed - create a fresh name *)
        let fresh = fresh_var erlang_name in
        (Pat_var fresh, StringSet.add fresh seen)
      else
        (Pat_var erlang_name, StringSet.add erlang_name seen)
  | Tpat_constant const -> (Pat_lit (convert_constant const), seen)
  | Tpat_tuple pats ->
      (* OCaml 5.4: tuples are (label option * pattern) list *)
      let pats', seen' = List.fold_left (fun (acc_pats, acc_seen) (_, p) ->
        let p', new_seen = convert_pattern_with_seen acc_seen p in
        (p' :: acc_pats, new_seen)
      ) ([], seen) pats in
      (Pat_tuple (List.rev pats'), seen')
  | Tpat_construct (_, cd, pats, _) ->
      convert_construct_pattern_with_seen seen cd pats
  | Tpat_alias (pat, id, _, _, _) ->
      let pat', seen' = convert_pattern_with_seen seen pat in
      let name = Ident.name id in
      let erlang_name =
        if name = "" then "_Anon"
        else if name.[0] = '_' then name
        else String.capitalize_ascii name
      in
      if StringSet.mem erlang_name seen' then
        let fresh = fresh_var erlang_name in
        (Pat_alias (pat', fresh), StringSet.add fresh seen')
      else
        (Pat_alias (pat', erlang_name), StringSet.add erlang_name seen')
  | Tpat_variant (label, arg_opt, _) ->
      convert_variant_pattern_with_seen seen label arg_opt
  | Tpat_record (fields, _) ->
      let fields', seen' = List.fold_left (fun (acc_fields, acc_seen) (_, lbl, pat) ->
        let pat', new_seen = convert_pattern_with_seen acc_seen pat in
        ((lbl.lbl_name, pat') :: acc_fields, new_seen)
      ) ([], seen) fields in
      (Pat_record (List.rev fields'), seen')
  | Tpat_array _ -> failwith "Array patterns not yet supported"
  | Tpat_or (p1, _p2, _) ->
      (* For now, just use first alternative *)
      convert_pattern_with_seen seen p1
  | Tpat_lazy _ -> failwith "Lazy patterns not supported"

(** Wrapper that starts with empty seen set *)
and convert_pattern (pat : Typedtree.pattern) : pattern =
  fst (convert_pattern_with_seen StringSet.empty pat)

(** Convert constructor pattern with seen tracking *)
and convert_construct_pattern_with_seen seen cd pats =
  let name = cd.cstr_name in
  match name, pats with
  | "()", [] -> (Pat_lit (Lit_atom "ok"), seen)
  | "true", [] -> (Pat_lit (Lit_atom "true"), seen)
  | "false", [] -> (Pat_lit (Lit_atom "false"), seen)
  | "[]", [] -> (Pat_lit Lit_nil, seen)
  | "::", [hd; tl] ->
      let hd', seen' = convert_pattern_with_seen seen hd in
      let tl', seen'' = convert_pattern_with_seen seen' tl in
      (Pat_cons (hd', tl'), seen'')
  | "None", [] -> (Pat_lit (Lit_atom "none"), seen)
  | "Some", [p] ->
      let p', seen' = convert_pattern_with_seen seen p in
      (Pat_tuple [Pat_lit (Lit_atom "some"); p'], seen')
  | "Ok", [p] ->
      let p', seen' = convert_pattern_with_seen seen p in
      (Pat_tuple [Pat_lit (Lit_atom "ok"); p'], seen')
  | "Error", [p] ->
      let p', seen' = convert_pattern_with_seen seen p in
      (Pat_tuple [Pat_lit (Lit_atom "error"); p'], seen')
  | _, [] -> (Pat_lit (Lit_atom (String.lowercase_ascii name)), seen)
  | _, _ ->
      let pats', seen' = List.fold_left (fun (acc_pats, acc_seen) p ->
        let p', new_seen = convert_pattern_with_seen acc_seen p in
        (p' :: acc_pats, new_seen)
      ) ([], seen) pats in
      (Pat_tuple (Pat_lit (Lit_atom (String.lowercase_ascii name)) :: List.rev pats'), seen')

(** Convert constructor pattern (wrapper) *)
and convert_construct_pattern cd pats =
  fst (convert_construct_pattern_with_seen StringSet.empty cd pats)

(** Convert polymorphic variant pattern with seen tracking *)
and convert_variant_pattern_with_seen seen label arg_opt =
  let atom = Pat_lit (Lit_atom label) in
  match arg_opt with
  | None -> (atom, seen)
  | Some p ->
      let p', seen' = convert_pattern_with_seen seen p in
      (Pat_tuple [atom; p'], seen')

(** Convert polymorphic variant pattern (wrapper) *)
and convert_variant_pattern label arg_opt =
  fst (convert_variant_pattern_with_seen StringSet.empty label arg_opt)

(** Convert a computation pattern (OCaml 5.4+)
    Computation patterns wrap value patterns or exception patterns *)
and convert_computation_pattern (pat : Typedtree.computation Typedtree.general_pattern) : pattern =
  match pat.pat_desc with
  | Tpat_value v ->
      (* Tpat_value wraps a value pattern - coerce and convert *)
      convert_pattern (v :> Typedtree.pattern)
  | Tpat_exception _ ->
      (* Exception patterns become a catch-all for now *)
      Pat_any
  | Tpat_or (p1, _p2, _) ->
      (* For or-patterns, just use first alternative *)
      convert_computation_pattern p1

(** Check if a path refers to Printf module *)
let is_printf_path path =
  match path with
  | Path.Pdot (Path.Pident mod_id, _) -> Ident.name mod_id = "Printf"
  | Path.Pdot (Path.Pdot (_, "Printf"), _) -> true
  | _ -> false

(** Check if a path refers to Lazy module *)
let is_lazy_path path =
  match path with
  | Path.Pdot (Path.Pident mod_id, _) -> Ident.name mod_id = "Lazy"
  | Path.Pdot (Path.Pdot (_, "Lazy"), _) -> true
  | _ -> false

(** Check if a path refers to Array module *)
let is_array_path path =
  match path with
  | Path.Pdot (Path.Pident mod_id, _) -> Ident.name mod_id = "Array"
  | Path.Pdot (Path.Pdot (_, "Array"), _) -> true
  | _ -> false

(** Check if a path refers to Inspect module *)
let is_inspect_path path =
  match path with
  | Path.Pdot (Path.Pident mod_id, _) -> Ident.name mod_id = "Inspect"
  | Path.Pdot (Path.Pdot (_, "Inspect"), _) -> true
  | _ -> false

(** Check if a path refers to any Macro module (for macro syntax).
    Matches both top-level Macro and nested Macro modules. *)
let rec is_macro_path path =
  match path with
  | Path.Pdot (Path.Pident mod_id, _) -> Ident.name mod_id = "Macro"
  | Path.Pdot (Path.Pdot (_, "Macro"), _) -> true
  | Path.Pdot (parent, "quote") -> is_macro_parent parent
  | Path.Pdot (parent, "to_string") -> is_macro_parent parent
  | Path.Pdot (parent, "unquote") -> is_macro_parent parent
  | Path.Pdot (parent, "defmacro") -> is_macro_parent parent
  | _ -> false

(** Check if a path ends with Macro *)
and is_macro_parent path =
  match path with
  | Path.Pident id -> Ident.name id = "Macro"
  | Path.Pdot (_, "Macro") -> true
  | _ -> false

(** Get function name from path *)
let get_path_func_name path =
  match path with
  | Path.Pdot (_, name) -> Some name
  | _ -> None

(** Check if a value binding has the [@expose] attribute.
    This is used to detect unhygienic variable bindings in macros.
    The attribute can be on the pattern (let x [@expose] = ...) or
    on the value binding itself. *)
let has_expose_attribute (vb : Typedtree.value_binding) : bool =
  (* Check pattern attributes - this is where `let x [@expose] = ...` puts it *)
  let pat_has_expose = List.exists (fun (attr : Parsetree.attribute) ->
    attr.attr_name.txt = "expose"
  ) vb.vb_pat.pat_attributes in
  (* Also check value binding attributes (for completeness) *)
  let vb_has_expose = List.exists (fun (attr : Parsetree.attribute) ->
    attr.attr_name.txt = "expose"
  ) vb.vb_attributes in
  pat_has_expose || vb_has_expose

(** Extract raw format string from a typed constant expression.
    OCaml's Printf format strings are represented with CamlinternalFormatBasics.Format
    constructor containing (format_ast, raw_string). We want the raw_string.
    The format AST contains string_literal nodes that are partial strings,
    but the raw_string is the complete format string. *)
let rec extract_typed_format_string (expr : Typedtree.expression) =
  match expr.exp_desc with
  | Texp_constant (Const_string (s, _, _)) -> Some s
  | Texp_construct (_, cd, args) ->
      (* Format constructor: look for the constructor name and structure *)
      let constructor_name = cd.cstr_name in
      (match constructor_name, args with
       | "Format", [_fmt_ast; raw_str] ->
           (* CamlinternalFormatBasics.Format (fmt_ast, raw_string) *)
           (* The raw_string is the second argument *)
           extract_typed_format_string raw_str
       | _, _ ->
           (* Try each argument *)
           List.find_map extract_typed_format_string args)
  | Texp_tuple exprs ->
      (* Could be (format_ast, raw_string) tuple - try last element first (raw string) *)
      (* Tuple elements are (label option, expression) pairs *)
      let exprs_only = List.map snd exprs in
      (match List.rev exprs_only with
       | last :: _ -> extract_typed_format_string last
       | [] -> None)
  | Texp_apply (_, args) ->
      (* Format string might be nested in function application *)
      List.find_map (fun (_, arg) ->
        match arg with
        | Typedtree.Arg e -> extract_typed_format_string e
        | Typedtree.Omitted _ -> None
      ) args
  | _ -> None

(** Convert a typed expression to BEAM IR *)
let rec convert_expr (expr : Typedtree.expression) : Beam_ir.expr =
  match expr.exp_desc with
  | Texp_ident (Path.Pident id, _, _) ->
      (* Check if this is a top-level function being used as a value *)
      (match lookup_top_level_function id with
       | Some (func_name, arity) ->
           (* Function reference *)
           Expr_fun_ref { func_name; arity }
       | None ->
           (* Variable reference *)
           Expr_var (convert_ident id))
  | Texp_ident (Path.Pdot (Path.Pident mod_id, func_name), _, _) ->
      (* Module.func - convert to call module:func() for zero-arity access *)
      let mod_name = String.lowercase_ascii (Ident.name mod_id) in
      Expr_call {
        module_ = Expr_lit (Lit_atom mod_name);
        func = Expr_lit (Lit_atom func_name);
        args = [];
      }
  | Texp_ident (Path.Pdot (Path.Pdot (_, sub_mod), func_name), _, _) ->
      (* Parent.Sub.func - use submodule as the erlang module *)
      (* e.g. Stdlib.Seq.empty -> seq:empty() *)
      let mod_name = String.lowercase_ascii sub_mod in
      Expr_call {
        module_ = Expr_lit (Lit_atom mod_name);
        func = Expr_lit (Lit_atom func_name);
        args = [];
      }
  | Texp_ident (path, _, _) ->
      (* Fallback for other path forms *)
      Expr_var (path_to_string path)

  | Texp_constant const ->
      Expr_lit (convert_constant const)

  | Texp_let (rec_flag, bindings, body) ->
      convert_let rec_flag bindings body

  | Texp_function (params, body) ->
      convert_function params body

  | Texp_apply (func, args) ->
      convert_apply func args

  | Texp_match (scrutinee, comp_cases, _effect_cases, _) ->
      (* OCaml 5.4: computation cases contain value+exception patterns,
         effect_cases contain effect handlers. We use comp_cases for regular matching. *)
      convert_match scrutinee comp_cases

  | Texp_tuple exprs ->
      (* OCaml 5.4: tuples are (label option * expr) list *)
      Expr_tuple (List.map (fun (_, e) -> convert_expr e) exprs)

  | Texp_construct (_, cd, args) ->
      convert_construct cd args

  | Texp_variant (label, arg_opt) ->
      convert_variant label arg_opt

  | Texp_record { fields; extended_expression; _ } ->
      convert_record fields extended_expression

  | Texp_field (expr, _, lbl) ->
      (* Check if this is accessing a field of a map-backed type:
         1. 'type t' in elixir_struct mode
         2. Any type marked with [@@elixir] *)
      let record_type_name = match Types.get_desc lbl.lbl_res with
        | Types.Tconstr (path, _, _) ->
            String.lowercase_ascii (Filename.basename (Path.name path))
        | _ -> ""
      in
      let is_map_type =
        (!elixir_struct_mode && record_type_name = "t") ||
        is_elixir_type record_type_name
      in
      if is_map_type then
        (* Map-backed type - use maps:get *)
        Expr_call {
          module_ = Expr_lit (Lit_atom "maps");
          func = Expr_lit (Lit_atom "get");
          args = [Expr_lit (Lit_atom lbl.lbl_name); convert_expr expr];
        }
      else
        (* Normal records are tuples - use element/2 *)
        let idx = lbl.lbl_pos + 1 in  (* Erlang tuples are 1-indexed *)
        Expr_call {
          module_ = Expr_lit (Lit_atom "erlang");
          func = Expr_lit (Lit_atom "element");
          args = [Expr_lit (Lit_int (idx + 1)); convert_expr expr];  (* +1 for tag *)
        }

  | Texp_ifthenelse (cond, then_, else_opt) ->
      let else_expr = match else_opt with
        | Some e -> convert_expr e
        | None -> Expr_lit (Lit_atom "ok")
      in
      Expr_if {
        cond = convert_expr cond;
        then_ = convert_expr then_;
        else_ = else_expr;
      }

  | Texp_sequence (e1, e2) ->
      Expr_seq (convert_expr e1, convert_expr e2)

  | Texp_try (body, exn_cases, _val_cases) ->
      (* In OCaml 5.4, Texp_try has: body, exception_handlers, value_handlers
         We use exn_cases for exception handling *)
      convert_try body exn_cases

  | Texp_array (_, elements) ->
      (* Arrays become Erlang tuples - use erlang:list_to_tuple for construction *)
      let elem_exprs = List.map convert_expr elements in
      Expr_call {
        module_ = Expr_lit (Lit_atom "erlang");
        func = Expr_lit (Lit_atom "list_to_tuple");
        args = [Expr_list elem_exprs];
      }
  | Texp_while _ -> failwith "While loops not supported (use recursion)"

  | Texp_for (loop_var, _pat, start_expr, end_expr, direction, body_expr) ->
      (* Convert for loop to recursive function:
         for i = start to end do body done
         becomes:
         letrec loop(i) = if i <= end then (body; loop(i+1)) else ()
      *)
      let var_name = convert_ident loop_var in
      let loop_name = fresh_var "for_loop" in
      let start_val = convert_expr start_expr in
      let end_val = convert_expr end_expr in
      let body_val = convert_expr body_expr in
      let end_var = fresh_var "End" in
      let step_op, cmp_op = match direction with
        | Asttypes.Upto -> (Op_add, Op_le)
        | Asttypes.Downto -> (Op_sub, Op_ge)
      in
      (* Build: letrec loop(I) = case I =< End of true -> body, loop(I+1); false -> ok end *)
      let loop_body =
        Expr_if {
          cond = Expr_binary (cmp_op, Expr_var var_name, Expr_var end_var);
          then_ = Expr_seq (body_val,
                    Expr_local_call {
                      func_name = loop_name;
                      arity = 1;
                      args = [Expr_binary (step_op, Expr_var var_name, Expr_lit (Lit_int 1))]
                    });
          else_ = Expr_lit (Lit_atom "ok");
        }
      in
      (* Wrap in let for end value, then letrec for loop, then call loop(start) *)
      Expr_let {
        var = end_var;
        value = end_val;
        body = Expr_letrec {
          bindings = [(loop_name, [var_name], loop_body)];
          body = Expr_local_call { func_name = loop_name; arity = 1; args = [start_val] };
        }
      }

  | Texp_send _ -> failwith "Objects not supported"
  | Texp_new _ -> failwith "Objects not supported"
  | Texp_instvar _ -> failwith "Objects not supported"
  | Texp_setinstvar _ -> failwith "Objects not supported"
  | Texp_override _ -> failwith "Objects not supported"
  | Texp_letmodule _ -> failwith "Local modules not yet supported"
  | Texp_letexception _ -> failwith "Local exceptions not yet supported"

  | Texp_assert (cond, _) ->
      (* Convert assert to: if cond then () else error(assertion_failed) *)
      Expr_if {
        cond = convert_expr cond;
        then_ = Expr_lit (Lit_atom "ok");
        else_ = Expr_call {
          module_ = Expr_lit (Lit_atom "erlang");
          func = Expr_lit (Lit_atom "error");
          args = [Expr_lit (Lit_atom "assertion_failed")];
        }
      }
  | Texp_lazy e ->
      (* lazy e becomes fun () -> e (a thunk) *)
      Expr_fun {
        params = ["_Lazy_unit"];
        body = convert_expr e;
      }

  | Texp_object _ -> failwith "Objects not supported"
  | Texp_pack _ -> failwith "First-class modules not supported"

  | Texp_letop { let_; ands; param; body; _ } ->
      (* let* x = e1 in e2  desugars to  (let* ) e1 (fun x -> e2)
         We convert this to: op(e1, fun param -> body) *)
      let convert_path_to_expr (path : Path.t) : Beam_ir.expr =
        match path with
        | Path.Pident id ->
            (match lookup_top_level_function id with
             | Some (func_name, arity) -> Expr_fun_ref { func_name; arity }
             | None -> Expr_var (convert_ident id))
        | _ -> Expr_var (path_to_string path)
      in
      let convert_binding_op (bop : Typedtree.binding_op) cont =
        let op_expr = convert_path_to_expr bop.bop_op_path in
        let rhs = convert_expr bop.bop_exp in
        (* Apply: op(rhs, cont) - both args at once (Core Erlang doesn't support currying) *)
        Expr_apply (op_expr, [rhs; cont])
      in
      (* Convert the body case to a function *)
      let param_name = convert_ident param in
      let body_expr = convert_expr body.c_rhs in
      let body_fun = Expr_fun { params = [param_name]; body = body_expr } in
      (* Start with the main let_ binding *)
      let result = convert_binding_op let_ body_fun in
      (* If there are 'and' bindings, we need special handling.
         For now, support the simple case without 'and' *)
      if ands <> [] then
        failwith "let* ... and* ... not yet supported (use nested let*)"
      else
        result
  | Texp_unreachable -> failwith "Unreachable"
  | Texp_extension_constructor _ -> failwith "Extension constructors not supported"
  | Texp_open (_, e) -> convert_expr e
  | Texp_atomic_loc _ -> failwith "Atomic locations not supported"
  | Texp_setfield _ -> failwith "Mutable record fields not supported"

(** Check if a pattern is a simple variable binding *)
and is_simple_var_pattern (pat : Typedtree.pattern) =
  match pat.pat_desc with
  | Typedtree.Tpat_var _ -> true
  | Typedtree.Tpat_any -> true
  | _ -> false

(** Convert let bindings *)
and convert_let rec_flag bindings body =
  match rec_flag with
  | Asttypes.Nonrecursive ->
      List.fold_right (fun vb acc ->
        let pat = vb.Typedtree.vb_pat in
        (* Check if binding has [@expose] attribute for unhygienic binding *)
        if has_expose_attribute vb && is_simple_var_pattern pat then
          (* [@expose]: use Pat_inject for unhygienic variable binding *)
          let var = pat_to_var pat in
          Expr_case {
            scrutinee = convert_expr vb.vb_expr;
            clauses = [(Pat_inject var, None, acc)];
          }
        else if is_simple_var_pattern pat then
          (* Simple variable binding: use let directly *)
          let var = pat_to_var pat in
          Expr_let {
            var;
            value = convert_expr vb.vb_expr;
            body = acc;
          }
        else
          (* Complex pattern (tuple, etc.): use case expression *)
          Expr_case {
            scrutinee = convert_expr vb.vb_expr;
            clauses = [(convert_pattern pat, None, acc)];
          }
      ) bindings (convert_expr body)
  | Asttypes.Recursive ->
      (* Extract Ident from pattern for letrec functions *)
      let pat_to_ident (pat : Typedtree.pattern) =
        match pat.pat_desc with
        | Typedtree.Tpat_var (id, _, _) -> Some id
        | _ -> None
      in
      (* First pass: collect all recursive function names and arities *)
      let rec_info = List.map (fun vb ->
        let id_opt = pat_to_ident vb.Typedtree.vb_pat in
        let name = String.lowercase_ascii (pat_to_var vb.Typedtree.vb_pat) in
        let arity = arity_of_type vb.Typedtree.vb_expr.exp_type in
        (vb, id_opt, name, arity)
      ) bindings in
      (* Register letrec functions so they can be referenced in function bodies AND in body *)
      List.iter (fun (_, id_opt, _, arity) ->
        match id_opt with
        | Some id -> register_top_level_function id arity
        | None -> ()
      ) rec_info;
      (* Extract function bodies with the letrec names in scope *)
      let rec_bindings = List.map (fun (vb, _, name, _) ->
        let params, func_body = extract_function vb.Typedtree.vb_expr in
        (name, params, func_body)
      ) rec_info in
      (* Convert body with letrec names still in scope *)
      let converted_body = convert_expr body in
      (* Remove the temporary registrations after converting everything *)
      List.iter (fun (_, id_opt, _, _) ->
        match id_opt with
        | Some id -> Hashtbl.remove top_level_functions (Ident.unique_name id)
        | None -> ()
      ) rec_info;
      Expr_letrec {
        bindings = rec_bindings;
        body = converted_body;
      }

and pat_to_var (pat : Typedtree.pattern) =
  match pat.pat_desc with
  | Typedtree.Tpat_var (id, _, _) -> convert_ident id
  | _ -> fresh_var "V"

(** Check if a type is unit *)
and is_unit_type ty =
  match Types.get_desc ty with
  | Types.Tconstr (path, [], _) -> Path.name path = "unit"
  | Types.Ttuple [] -> true  (* empty tuple is also unit *)
  | _ -> false

(** Get the type of a function parameter *)
and get_param_type (fp : Typedtree.function_param) : Types.type_expr option =
  match fp.fp_kind with
  | Tparam_pat pat -> Some pat.pat_type
  | Tparam_optional_default (pat, _) -> Some pat.pat_type

(** Get the pattern from a function parameter *)
and get_param_pattern (fp : Typedtree.function_param) : Typedtree.pattern =
  match fp.fp_kind with
  | Tparam_pat pat -> pat
  | Tparam_optional_default (pat, _) -> pat

(** Check if a function parameter has a simple pattern (var or any) *)
and is_simple_param (fp : Typedtree.function_param) : bool =
  is_simple_var_pattern (get_param_pattern fp)

(** Extract parameter info: (param_var, target_pattern option, default_expr option)
    - param_var: the variable name for the function parameter
    - target_pattern: for complex patterns or optional args, the pattern to bind
    - default_expr: for optional args, the default value expression *)
and extract_param_info (fp : Typedtree.function_param)
    : (string * Typedtree.pattern option * Typedtree.expression option) option =
  (* Check if parameter type is unit - if so, skip this parameter *)
  match get_param_type fp with
  | Some ty when is_unit_type ty -> None
  | _ ->
      match fp.fp_kind with
      | Tparam_optional_default (pat, default_expr) ->
          (* Optional param: use fresh var for the option wrapper, store default *)
          let var = fresh_var "Opt" in
          Some (var, Some pat, Some default_expr)
      | Tparam_pat _ ->
          if is_simple_param fp then
            (* Simple param: just use the name *)
            Some (convert_ident fp.fp_param, None, None)
          else
            (* Complex param: generate fresh var, remember pattern for destructuring *)
            let var = fresh_var "Param" in
            Some (var, Some (get_param_pattern fp), None)

(** Extract parameter name from function_param, returns None for unit params *)
and param_name_opt (fp : Typedtree.function_param) : string option =
  (* Check if parameter type is unit - if so, skip this parameter *)
  match get_param_type fp with
  | Some ty when is_unit_type ty -> None
  | _ -> Some (convert_ident fp.fp_param)

(** Extract parameter name from function_param (legacy, for non-unit params) *)
and param_name (fp : Typedtree.function_param) : string =
  convert_ident fp.fp_param

(** Wrap body with case expressions for parameter patterns and optional arg defaults.
    For optional args: generates case to unwrap Some/None and apply default.
    For complex patterns: generates case to destructure. *)
and wrap_body_with_param_destructuring
    (param_infos : (string * Typedtree.pattern option * Typedtree.expression option) list)
    (body : expr) : expr =
  List.fold_right (fun (var_name, pat_opt, default_opt) acc ->
    match default_opt, pat_opt with
    | Some default_expr, Some pat ->
        (* Optional argument with pattern: unwrap option and bind to pattern *)
        let target_var = match pat.Typedtree.pat_desc with
          | Typedtree.Tpat_var (id, _, _) -> convert_ident id
          | _ -> fresh_var "OptVal"
        in
        let default_val = convert_expr default_expr in
        (* case OptVar of {some, V} -> <bind V to pattern>; none -> <use default> *)
        Expr_case {
          scrutinee = Expr_var var_name;
          clauses = [
            (Pat_tuple [Pat_lit (Lit_atom "some"); Pat_var target_var], None,
              (* If pattern is just a var, we're done. Otherwise need to destructure *)
              (match pat.Typedtree.pat_desc with
               | Typedtree.Tpat_var _ -> acc
               | _ -> Expr_case {
                   scrutinee = Expr_var target_var;
                   clauses = [(convert_pattern pat, None, acc)];
                 }));
            (Pat_lit (Lit_atom "none"), None,
              (* Bind the default value to the target variable *)
              Expr_let { var = target_var; value = default_val; body = acc });
          ];
        }
    | None, Some pat ->
        (* Complex pattern without default: just destructure *)
        Expr_case {
          scrutinee = Expr_var var_name;
          clauses = [(convert_pattern pat, None, acc)];
        }
    | Some default_expr, None ->
        (* Optional argument with simple var pattern - shouldn't happen but handle it *)
        let default_val = convert_expr default_expr in
        Expr_case {
          scrutinee = Expr_var var_name;
          clauses = [
            (Pat_tuple [Pat_lit (Lit_atom "some"); Pat_var var_name], None, acc);
            (Pat_lit (Lit_atom "none"), None,
              Expr_let { var = var_name; value = default_val; body = acc });
          ];
        }
    | None, None ->
        acc  (* Simple param, no destructuring needed *)
  ) param_infos body

(** Extract parameters from a function expression *)
and extract_function expr =
  match expr.Typedtree.exp_desc with
  | Texp_function (params, Tfunction_body body_expr) ->
      let param_infos = List.filter_map extract_param_info params in
      let param_names = List.map (fun (x, _, _) -> x) param_infos in
      let more_params, body = extract_function body_expr in
      let wrapped_body = wrap_body_with_param_destructuring param_infos body in
      (param_names @ more_params, wrapped_body)
  | Texp_function (params, Tfunction_cases { cases; param; _ }) ->
      (* Function with pattern matching cases *)
      let param_infos = List.filter_map extract_param_info params in
      let param_names = List.map (fun (x, _, _) -> x) param_infos in
      let match_var = convert_ident param in
      let clauses = List.map (fun c ->
        let pat = convert_pattern c.Typedtree.c_lhs in
        let guard = Option.map convert_expr c.c_guard in
        let body = convert_expr c.c_rhs in
        (pat, guard, body)
      ) cases in
      let case_body = Expr_case { scrutinee = Expr_var match_var; clauses } in
      let wrapped_body = wrap_body_with_param_destructuring param_infos case_body in
      (param_names @ [match_var], wrapped_body)
  | _ -> ([], convert_expr expr)

(** Convert function expression *)
and convert_function params body =
  match body with
  | Typedtree.Tfunction_body body_expr ->
      let param_infos = List.filter_map extract_param_info params in
      let param_names = List.map (fun (x, _, _) -> x) param_infos in
      let more_params, final_body = extract_function body_expr in
      let wrapped_body = wrap_body_with_param_destructuring param_infos final_body in
      Expr_fun {
        params = param_names @ more_params;
        body = wrapped_body;
      }
  | Typedtree.Tfunction_cases { cases; param; _ } ->
      let param_infos = List.filter_map extract_param_info params in
      let param_names = List.map (fun (x, _, _) -> x) param_infos in
      let match_var = convert_ident param in
      let clauses = List.map (fun c ->
        let pat = convert_pattern c.Typedtree.c_lhs in
        let guard = Option.map convert_expr c.c_guard in
        let body = convert_expr c.c_rhs in
        (pat, guard, body)
      ) cases in
      let case_body = Expr_case { scrutinee = Expr_var match_var; clauses } in
      let wrapped_body = wrap_body_with_param_destructuring param_infos case_body in
      Expr_fun {
        params = param_names @ [match_var];
        body = wrapped_body;
      }

(** Extract pattern matching clauses from a function expression *)
and extract_receive_clauses (expr : Typedtree.expression) : (pattern * expr option * expr) list =
  match expr.exp_desc with
  | Texp_function (_, Tfunction_cases { cases; _ }) ->
      (* Function with pattern matching - extract the cases *)
      List.map (fun c ->
        let pat = convert_pattern c.Typedtree.c_lhs in
        let guard = Option.map convert_expr c.c_guard in
        let body = convert_expr c.c_rhs in
        (pat, guard, body)
      ) cases
  | Texp_function (params, Tfunction_body body_expr) ->
      (* Simple function - create a single clause binding the param *)
      let pname = match List.filter_map param_name_opt params with
        | [p] -> p
        | _ -> fresh_var "Msg"
      in
      [(Pat_var pname, None, convert_expr body_expr)]
  | _ ->
      (* Fallback: create a catch-all clause *)
      let var = fresh_var "Msg" in
      [(Pat_var var, None, Expr_apply (convert_expr expr, [Expr_var var]))]

(** Convert spawn call - wraps unit-taking functions in 0-arity lambdas *)
and convert_spawn args =
  match args with
  | [(_, Typedtree.Arg func_expr)] ->
      (* The function passed to spawn takes unit, which means it has arity 1 in OCaml.
         In Erlang, spawn needs a 0-arity fun. We wrap the call:
         spawn(pong) -> spawn(fun() -> pong('ok') end) *)
      let func_converted = convert_expr func_expr in
      let wrapped = match func_converted with
        | Expr_fun_ref { func_name; arity } when arity = 1 ->
            (* Wrap: fun () -> apply 'name'/1 ('ok') *)
            Expr_fun {
              params = [];
              body = Expr_local_call { func_name; arity; args = [Expr_lit (Lit_atom "ok")] }
            }
        | Expr_fun { params = [_]; body } ->
            (* Already a lambda with one (unit) param - make it 0-arity *)
            Expr_fun { params = []; body }
        | other ->
            (* Already a 0-arity fun or something else - use as-is *)
            other
      in
      Expr_call {
        module_ = Expr_lit (Lit_atom "erlang");
        func = Expr_lit (Lit_atom "spawn");
        args = [wrapped];
      }
  | _ ->
      failwith "spawn expects a single function argument"

(** Convert receive expression: receive (function | Pat1 -> Body1 | ...) *)
and convert_receive args =
  match args with
  | [(_, Typedtree.Arg handler)] ->
      let clauses = extract_receive_clauses handler in
      Expr_receive { clauses; timeout = None }
  | _ ->
      failwith "receive expects a single function argument"

(** Convert receive with timeout: receive_timeout timeout_ms (function | ...) after_body *)
and convert_receive_timeout args =
  match args with
  | [(_, Typedtree.Arg timeout_expr); (_, Typedtree.Arg handler); (_, Typedtree.Arg after_body)] ->
      let clauses = extract_receive_clauses handler in
      let timeout_val = convert_expr timeout_expr in
      let after_expr = convert_expr after_body in
      Expr_receive { clauses; timeout = Some (timeout_val, after_expr) }
  | [(_, Typedtree.Arg timeout_expr); (_, Typedtree.Arg handler)] ->
      (* Two-argument form: timeout and handler, after body is unit *)
      let clauses = extract_receive_clauses handler in
      let timeout_val = convert_expr timeout_expr in
      Expr_receive { clauses; timeout = Some (timeout_val, Expr_lit (Lit_atom "timeout")) }
  | _ ->
      failwith "receive_timeout expects (timeout, handler) or (timeout, handler, after_body) arguments"

(** Extract external module/function from a primitive description.
    For "external foo : t = "mod" "func"", prim_name = "mod", prim_native_name = "func".
    Returns None for OCaml internal primitives (those starting with '%'). *)
and get_primitive_info (vd : Types.value_description) : (string * string) option =
  match vd.val_kind with
  | Types.Val_prim prim ->
      let mod_name = prim.Primitive.prim_name in
      let func_name = prim.Primitive.prim_native_name in
      (* Skip OCaml internal primitives (they start with %) *)
      if String.length mod_name > 0 && mod_name.[0] = '%' then
        None
      else if func_name = "" then
        (* Single-string external: use "erlang" as module *)
        Some ("erlang", mod_name)
      else
        Some (mod_name, func_name)
  | _ -> None

(** Convert function application *)
and convert_apply func args =
  (* Check for special module calls BEFORE converting arguments *)
  match func.exp_desc with
  | Texp_ident (path, _, _) when is_array_path path ->
      (* Handle Array module - arrays are tuples in Erlang *)
      let func_name = match get_path_func_name path with Some n -> n | None -> "" in
      let typed_args = List.filter_map (fun (_, arg) ->
        match arg with
        | Typedtree.Arg e -> Some e
        | Typedtree.Omitted _ -> None
      ) args in
      (match func_name, typed_args with
       | ("get" | "unsafe_get"), [arr; idx] ->
           (* Array.get arr idx => erlang:element(idx + 1, arr) *)
           let arr_expr = convert_expr arr in
           let idx_expr = convert_expr idx in
           Expr_call {
             module_ = Expr_lit (Lit_atom "erlang");
             func = Expr_lit (Lit_atom "element");
             args = [Expr_binary (Op_add, idx_expr, Expr_lit (Lit_int 1)); arr_expr];
           }
       | ("set" | "unsafe_set"), [arr; idx; value] ->
           (* Array.set arr idx value => erlang:setelement(idx + 1, arr, value) *)
           let arr_expr = convert_expr arr in
           let idx_expr = convert_expr idx in
           let value_expr = convert_expr value in
           Expr_call {
             module_ = Expr_lit (Lit_atom "erlang");
             func = Expr_lit (Lit_atom "setelement");
             args = [Expr_binary (Op_add, idx_expr, Expr_lit (Lit_int 1)); arr_expr; value_expr];
           }
       | "length", [arr] ->
           (* Array.length arr => erlang:tuple_size(arr) *)
           let arr_expr = convert_expr arr in
           Expr_call {
             module_ = Expr_lit (Lit_atom "erlang");
             func = Expr_lit (Lit_atom "tuple_size");
             args = [arr_expr];
           }
       | "make", [len; init] ->
           (* Array.make len init => erlang:make_tuple(len, init) *)
           let len_expr = convert_expr len in
           let init_expr = convert_expr init in
           Expr_call {
             module_ = Expr_lit (Lit_atom "erlang");
             func = Expr_lit (Lit_atom "make_tuple");
             args = [len_expr; init_expr];
           }
       | _ ->
           (* Unknown Array function - use default handling *)
           let func_expr = convert_expr func in
           let arg_exprs = List.map convert_expr typed_args in
           Expr_apply (func_expr, arg_exprs))
  | Texp_ident (path, _, _) when is_lazy_path path ->
      (* Handle Lazy.force - just call the thunk with 'ok' (unit) *)
      let func_name = match get_path_func_name path with Some n -> n | None -> "" in
      let typed_args = List.filter_map (fun (_, arg) ->
        match arg with
        | Typedtree.Arg e -> Some e
        | Typedtree.Omitted _ -> None
      ) args in
      (match func_name, typed_args with
       | "force", [lazy_val] ->
           (* Lazy.force thunk => thunk(ok) *)
           let thunk_expr = convert_expr lazy_val in
           Expr_apply (thunk_expr, [Expr_lit (Lit_atom "ok")])
       | _ ->
           (* Unknown Lazy function - use default handling *)
           let func_expr = convert_expr func in
           let arg_exprs = List.map convert_expr typed_args in
           Expr_apply (func_expr, arg_exprs))
  | Texp_ident (path, _, _) when is_macro_path path ->
      (* Handle Macro module for compile-time metaprogramming:
         - Macro.quote expr      -> Expr_quote (convert expr)
         - Macro.unquote name    -> Expr_unquote "name"
         - Macro.to_string name  -> Expr_stringify "name"
      *)
      let func_name = match get_path_func_name path with Some n -> n | None -> "" in
      let typed_args = List.filter_map (fun (_, arg) ->
        match arg with
        | Typedtree.Arg e -> Some e
        | Typedtree.Omitted _ -> None
      ) args in
      (match func_name, typed_args with
       | "quote", [inner_expr] ->
           (* Macro.quote expr -> wrap the converted expr in Expr_quote *)
           Expr_quote (convert_expr inner_expr)
       | "unquote", [name_expr] ->
           (* Macro.unquote expr -> Expr_unquote "expr" (splices macro parameter AST) *)
           (match name_expr.exp_desc with
            | Texp_constant (Const_string (name, _, _)) -> Expr_unquote name
            | Texp_ident (Path.Pident id, _, _) -> Expr_unquote (Ident.name id)
            | _ -> failwith "Macro.unquote requires a macro parameter or string literal")
       | "to_string", [name_expr] ->
           (* Macro.to_string expr -> Expr_stringify "expr" *)
           (match name_expr.exp_desc with
            | Texp_constant (Const_string (name, _, _)) -> Expr_stringify name
            | Texp_ident (Path.Pident id, _, _) -> Expr_stringify (Ident.name id)
            | _ -> failwith "Macro.to_string requires a macro parameter or string literal")
       | _ ->
           (* Unknown Macro function - use default handling *)
           let func_expr = convert_expr func in
           let arg_exprs = List.map convert_expr typed_args in
           Expr_apply (func_expr, arg_exprs))
  | Texp_ident (path, _, _) when is_printf_path path ->
      let func_name = match get_path_func_name path with Some n -> n | None -> "" in
      let typed_args = List.filter_map (fun (_, arg) ->
        match arg with
        | Typedtree.Arg e -> Some e
        | Typedtree.Omitted _ -> None
      ) args in
      (match func_name, typed_args with
       | ("sprintf" | "printf"), format_arg :: value_typed_args ->
           (* Extract format string from typed AST before conversion *)
           (match extract_typed_format_string format_arg with
            | Some fmt_str ->
                let erlang_fmt = convert_format_string fmt_str in
                let value_args = List.map convert_expr value_typed_args in
                let expected_args = count_format_args fmt_str in
                let provided_args = List.length value_args in
                if func_name = "sprintf" then
                  if provided_args >= expected_args then
                    (* sprintf: full application - lists:flatten(io_lib:format(fmt, [args])) *)
                    let format_call = Expr_call {
                      module_ = Expr_lit (Lit_atom "io_lib");
                      func = Expr_lit (Lit_atom "format");
                      args = [
                        Expr_lit (Lit_string erlang_fmt);
                        Expr_list value_args;
                      ];
                    } in
                    Expr_call {
                      module_ = Expr_lit (Lit_atom "lists");
                      func = Expr_lit (Lit_atom "flatten");
                      args = [format_call];
                    }
                  else
                    (* sprintf: partial application - return a lambda *)
                    let missing = expected_args - provided_args in
                    let param_names = List.init missing (fun i -> fresh_var (Printf.sprintf "Arg%d" (i + 1))) in
                    let param_exprs = List.map (fun v -> Expr_var v) param_names in
                    let all_args = value_args @ param_exprs in
                    let format_call = Expr_call {
                      module_ = Expr_lit (Lit_atom "io_lib");
                      func = Expr_lit (Lit_atom "format");
                      args = [
                        Expr_lit (Lit_string erlang_fmt);
                        Expr_list all_args;
                      ];
                    } in
                    let body = Expr_call {
                      module_ = Expr_lit (Lit_atom "lists");
                      func = Expr_lit (Lit_atom "flatten");
                      args = [format_call];
                    } in
                    List.fold_right (fun param acc ->
                      Expr_fun { params = [param]; body = acc }
                    ) param_names body
                else
                  if provided_args >= expected_args then
                    (* printf: full application - io:format(fmt, [args]) *)
                    Expr_call {
                      module_ = Expr_lit (Lit_atom "io");
                      func = Expr_lit (Lit_atom "format");
                      args = [
                        Expr_lit (Lit_string erlang_fmt);
                        Expr_list value_args;
                      ];
                    }
                  else
                    (* printf: partial application - return a lambda *)
                    let missing = expected_args - provided_args in
                    let param_names = List.init missing (fun i -> fresh_var (Printf.sprintf "Arg%d" (i + 1))) in
                    let param_exprs = List.map (fun v -> Expr_var v) param_names in
                    let all_args = value_args @ param_exprs in
                    let body = Expr_call {
                      module_ = Expr_lit (Lit_atom "io");
                      func = Expr_lit (Lit_atom "format");
                      args = [
                        Expr_lit (Lit_string erlang_fmt);
                        Expr_list all_args;
                      ];
                    } in
                    List.fold_right (fun param acc ->
                      Expr_fun { params = [param]; body = acc }
                    ) param_names body
            | None ->
                (* Couldn't extract format string, fall through to default *)
                let func_expr = convert_expr func in
                let arg_exprs = List.map convert_expr typed_args in
                Expr_apply (func_expr, arg_exprs))
       | _ ->
           (* Unknown Printf function - use default handling *)
           let func_expr = convert_expr func in
           let arg_exprs = List.filter_map (fun (_, arg) ->
             match arg with
             | Typedtree.Arg e -> Some (convert_expr e)
             | Typedtree.Omitted _ -> None
           ) args in
           Expr_apply (func_expr, arg_exprs))
  | Texp_ident (path, _, _) when is_inspect_path path ->
      (* Handle Inspect module - special handling for pp, others go to Merlot.Inspect *)
      let func_name = match get_path_func_name path with Some n -> n | None -> "" in
      let typed_args = List.filter_map (fun (_, arg) ->
        match arg with
        | Typedtree.Arg e -> Some e
        | Typedtree.Omitted _ -> None
      ) args in
      (match func_name, typed_args with
       | "pp", [value_expr] ->
           (* Inspect.pp value -> generate_pp_expr_for_type based on value's type *)
           let value_type = value_expr.exp_type in
           let converted_value = convert_expr value_expr in
           generate_pp_expr_for_type value_type converted_value
       | _ ->
           (* Other Inspect functions -> call Merlot.Inspect module *)
           let arg_exprs = List.map convert_expr typed_args in
           Expr_call {
             module_ = Expr_lit (Lit_atom "Merlot.Inspect");
             func = Expr_lit (Lit_atom func_name);
             args = arg_exprs;
           })
  | _ ->
      (* Default handling for non-Printf calls *)
      let func_expr = convert_expr func in
      let arg_exprs = List.filter_map (fun (_, arg) ->
        match arg with
        | Typedtree.Arg e -> Some (convert_expr e)
        | Typedtree.Omitted _ -> None
      ) args in

      (* Check for primitive operations - first try to get primitive info from value_description *)
      match func.exp_desc with
  | Texp_ident (_, _, vd) ->
      (* Check if this is a primitive (external) by examining value_description *)
      (match get_primitive_info vd with
       | Some ("erlang", "receive") ->
           (* Special handling for receive - extract pattern matching from the argument *)
           convert_receive args
       | Some ("erlang", "receive_timeout") ->
           (* receive with timeout: receive_timeout timeout_ms handler_fn *)
           convert_receive_timeout args
       | Some ("erlang", "spawn") ->
           (* spawn expects a 0-arity fun, but OCaml functions take unit *)
           convert_spawn args
       | Some ("merlot", "pipe_first") ->
           (* Data-first pipe: x |. f a b  =>  f x a b
              Need to look at original typed args to detect externals *)
           let typed_args_list = List.filter_map (fun (_, arg) ->
             match arg with
             | Typedtree.Arg e -> Some e
             | Typedtree.Omitted _ -> None
           ) args in
           (match typed_args_list, arg_exprs with
            | [_; f_typed], [x; f] ->
                (* Check if f is an external function *)
                let external_info = match f_typed.exp_desc with
                  | Texp_ident (Path.Pident id, _, _) -> lookup_external id
                  | _ -> None
                in
                (match external_info with
                 | Some (mod_name, func_name) ->
                     (* f is an external - generate proper Expr_call *)
                     Expr_call {
                       module_ = Expr_lit (Lit_atom mod_name);
                       func = Expr_lit (Lit_atom func_name);
                       args = [x];
                     }
                 | None ->
                     (* Not an external - handle normally *)
                     (match f with
                      | Expr_apply (func, args) -> Expr_apply (func, x :: args)
                      | Expr_local_call { func_name; arity = _; args } ->
                          Expr_local_call { func_name; arity = List.length args + 1; args = x :: args }
                      | Expr_call { module_; func; args } ->
                          Expr_call { module_; func; args = x :: args }
                      | _ -> Expr_apply (f, [x])))
            | _ -> failwith "pipe_first expects exactly 2 arguments")
       | Some (mod_name, func_name) ->
           (* Regular external call - filter out unit arguments *)
           let non_unit_args = List.filter (function
             | Expr_lit (Lit_atom "ok") -> false
             | _ -> true
           ) arg_exprs in
           (* Elixir modules use OCaml-style bindings (function-first) for pipe
              compatibility, but the actual Elixir calls expect data-first.
              Handle argument reordering for these modules. *)
           let needs_arg_swap =
             mod_name = "Elixir.Enum" ||
             mod_name = "Elixir.String" ||
             mod_name = "Elixir.Map" ||
             mod_name = "Elixir.Keyword"
           in
           let make_call args = Expr_call {
             module_ = Expr_lit (Lit_atom mod_name);
             func = Expr_lit (Lit_atom func_name);
             args;
           } in
           (* Single-arg functions that should NOT be treated as partial apps *)
           let is_single_arg_func = List.mem func_name [
             (* Elixir.Enum *)
             "count"; "sum"; "min"; "max"; "sort"; "reverse"; "shuffle";
             "uniq"; "concat"; "first"; "last"; "random"; "empty?"; "with_index";
             (* Elixir.String *)
             "length"; "upcase"; "downcase"; "capitalize"; "trim";
             "trim_leading"; "trim_trailing"; "graphemes"; "codepoints";
             "to_integer"; "to_float"; "to_atom"; "to_charlist";
             "printable?"; "valid?"; "next_grapheme";
             (* Elixir.Map *)
             "to_list"; "keys"; "values"; "new";
             (* Elixir.Keyword *)
             "keyword?";
           ] in
           (* 3-arg functions: when given 2 args, it's a partial application *)
           let is_three_arg_func = List.mem func_name [
             (* Elixir.Enum *)
             "reduce"; "slice";
             (* Elixir.String *)
             "replace"; "slice";
             (* Elixir.Map *)
             "put"; "put_new"; "get"; "update!";
             (* Elixir.Keyword *)
             "put"; "put_new"; "get";
           ] in
           if needs_arg_swap then
             (match func_name, non_unit_args with
              (* === Elixir.Enum reduce: special arg order (data, init, f) === *)
              | "reduce", [f; init; data] -> make_call [data; init; f]
              | "reduce", [f; init] ->
                  let data_var = fresh_var "Data" in
                  Expr_fun {
                    params = [data_var];
                    body = make_call [Expr_var data_var; init; f]
                  }
              | "reduce", [f] ->
                  let init_var = fresh_var "Init" in
                  let data_var = fresh_var "Data" in
                  Expr_fun {
                    params = [init_var];
                    body = Expr_fun {
                      params = [data_var];
                      body = make_call [Expr_var data_var; Expr_var init_var; f]
                    }
                  }
              (* === Generic 3-arg: (a, b, data) -> (data, a, b) === *)
              | _, [a; b; data] -> make_call [data; a; b]
              (* === Partial 3-arg: (a, b) -> fun data -> (data, a, b) === *)
              | _, [a; b] when is_three_arg_func ->
                  let data_var = fresh_var "Data" in
                  Expr_fun {
                    params = [data_var];
                    body = make_call [Expr_var data_var; a; b]
                  }
              (* === Generic 2-arg: (a, data) -> (data, a) === *)
              | _, [a; data] -> make_call [data; a]
              (* === Generic 1-arg partial: wrap in lambda === *)
              | _, [a] when not is_single_arg_func ->
                  let data_var = fresh_var "Data" in
                  Expr_fun {
                    params = [data_var];
                    body = make_call [Expr_var data_var; a]
                  }
              (* Single-arg functions or unknown - pass through *)
              | _ -> make_call non_unit_args)
           else
             make_call non_unit_args
       | None ->
           (* Not a primitive - check for other cases *)
           convert_non_primitive_apply func arg_exprs)
  | _ ->
      Expr_apply (func_expr, arg_exprs)

(** Convert non-primitive function applications *)
and convert_non_primitive_apply func arg_exprs =
  match func.exp_desc with
  | Texp_ident (Path.Pident id, _, _) ->
      convert_builtin_or_apply id arg_exprs
  | Texp_ident (Path.Pdot (Path.Pident mod_id, op), _, _)
    when Ident.name mod_id = "Stdlib" ->
      convert_stdlib_call op arg_exprs
  | Texp_ident (Path.Pdot (Path.Pident mod_id, op), _, _)
    when Ident.name mod_id = "Merlot_stdlib" ->
      convert_stdlib_call op arg_exprs
  (* Printf module handling *)
  | Texp_ident (Path.Pdot (Path.Pident mod_id, func_name), _, _)
    when Ident.name mod_id = "Printf" ->
      (match func_name, arg_exprs with
       | "sprintf", format_expr :: value_args ->
           convert_printf_sprintf format_expr value_args
       | "printf", format_expr :: value_args ->
           convert_printf_printf format_expr value_args
       | _ ->
           (* Unknown Printf function - fall through to default *)
           let mod_name = prefixed_module_name (Ident.name mod_id) in
           Expr_call {
             module_ = Expr_lit (Lit_atom mod_name);
             func = Expr_lit (Lit_atom func_name);
             args = arg_exprs;
           })
  | Texp_ident (Path.Pdot (Path.Pident mod_id, func_name), _, _) ->
      (* Cross-module call to another OCaml module - use Merlot. prefix *)
      let mod_name = prefixed_module_name (Ident.name mod_id) in
      Expr_call {
        module_ = Expr_lit (Lit_atom mod_name);
        func = Expr_lit (Lit_atom func_name);
        args = arg_exprs;
      }
  | Texp_ident (Path.Pdot (Path.Pdot (Path.Pident parent_mod, sub_mod), func_name), _, _) ->
      (* Nested module path: Parent.Sub.func *)
      let parent_name = Ident.name parent_mod in
      (* Check if this is a Merlot_stdlib submodule with OCaml implementations *)
      if parent_name = "Merlot_stdlib" && sub_mod = "List" then
        convert_merlot_list_call func_name arg_exprs
      else if parent_name = "Merlot_stdlib" && sub_mod = "Option" then
        convert_merlot_option_call func_name arg_exprs
      else if parent_name = "Merlot_stdlib" && sub_mod = "Result" then
        convert_merlot_result_call func_name arg_exprs
      else
        (* Determine if this is a Merlot stdlib module with OCaml implementation *)
        let merlot_prefixed_modules = [
          "Actor"; "Enum"; "Inspect"; "Keyword"; "Map"; "Option";
          "Process"; "Result"; "Supervisor"
        ] in
        let mod_name =
          if parent_name = "Merlot_stdlib" && List.mem sub_mod merlot_prefixed_modules then
            "Merlot." ^ sub_mod  (* OCaml implementation -> Merlot.Module *)
          else
            String.lowercase_ascii sub_mod  (* External binding -> erlang module *)
        in
        Expr_call {
          module_ = Expr_lit (Lit_atom mod_name);
          func = Expr_lit (Lit_atom func_name);
          args = arg_exprs;
        }
  | _ ->
      Expr_apply (convert_expr func, arg_exprs)

(** Convert Stdlib operations to BEAM primitives *)
and convert_stdlib_call op args =
  match op, args with
  | "+", [a; b] -> Expr_binary (Op_add, a, b)
  | "-", [a; b] -> Expr_binary (Op_sub, a, b)
  | "*", [a; b] -> Expr_binary (Op_mul, a, b)
  | "/", [a; b] -> Expr_binary (Op_idiv, a, b)  (* Integer division in OCaml *)
  | "mod", [a; b] -> Expr_binary (Op_mod, a, b)
  | "=", [a; b] -> Expr_binary (Op_eq, a, b)
  | "<>", [a; b] -> Expr_binary (Op_ne, a, b)
  | "<", [a; b] -> Expr_binary (Op_lt, a, b)
  | "<=", [a; b] -> Expr_binary (Op_le, a, b)
  | ">", [a; b] -> Expr_binary (Op_gt, a, b)
  | ">=", [a; b] -> Expr_binary (Op_ge, a, b)
  | "&&", [a; b] -> Expr_binary (Op_and, a, b)
  | "||", [a; b] -> Expr_binary (Op_or, a, b)
  | "not", [a] -> Expr_unary (Op_not, a)
  | "~-", [a] -> Expr_unary (Op_neg, a)
  | "@", [a; b] -> Expr_binary (Op_append, a, b)
  | "^", [a; b] -> Expr_binary (Op_append, a, b)  (* String concat = list append for charlists *)
  | "+.", [a; b] -> Expr_binary (Op_add, a, b)
  | "-.", [a; b] -> Expr_binary (Op_sub, a, b)
  | "*.", [a; b] -> Expr_binary (Op_mul, a, b)
  | "/.", [a; b] -> Expr_binary (Op_div, a, b)
  | "~-.", [a] -> Expr_unary (Op_neg, a)
  (* Pipe operators - inline as function application *)
  | "|>", [x; f] -> Expr_apply (f, [x])  (* x |> f  =>  f x  (data-last) *)
  (* Data-first pipe: x |. f a b  =>  f x a b  (x becomes first arg) *)
  | "|.", [x; f] ->
      (match f with
       | Expr_apply (func, args) -> Expr_apply (func, x :: args)  (* f a b => f x a b *)
       | Expr_local_call { func_name; arity; args } ->
           Expr_local_call { func_name; arity = arity + 1; args = x :: args }
       | Expr_call { module_; func; args } ->
           Expr_call { module_; func; args = x :: args }
       | _ -> Expr_apply (f, [x]))  (* Fallback: same as |> *)
  | "fst", [a] ->
      Expr_call {
        module_ = Expr_lit (Lit_atom "erlang");
        func = Expr_lit (Lit_atom "element");
        args = [Expr_lit (Lit_int 1); a];
      }
  | "snd", [a] ->
      Expr_call {
        module_ = Expr_lit (Lit_atom "erlang");
        func = Expr_lit (Lit_atom "element");
        args = [Expr_lit (Lit_int 2); a];
      }
  | "sqrt", [a] ->
      Expr_call {
        module_ = Expr_lit (Lit_atom "math");
        func = Expr_lit (Lit_atom "sqrt");
        args = [a];
      }
  | "abs_float", [a] ->
      Expr_call {
        module_ = Expr_lit (Lit_atom "erlang");
        func = Expr_lit (Lit_atom "abs");
        args = [a];
      }
  (* Exception handling *)
  | "raise", [exn] ->
      (* raise exn => erlang:raise(error, exn, [])
         Use 'error' class for OCaml exceptions *)
      Expr_call {
        module_ = Expr_lit (Lit_atom "erlang");
        func = Expr_lit (Lit_atom "raise");
        args = [Expr_lit (Lit_atom "error"); exn; Expr_list []];
      }
  | "failwith", [msg] ->
      (* failwith msg => erlang:raise(error, {failure, msg}, []) *)
      Expr_call {
        module_ = Expr_lit (Lit_atom "erlang");
        func = Expr_lit (Lit_atom "raise");
        args = [
          Expr_lit (Lit_atom "error");
          Expr_tuple [Expr_lit (Lit_atom "failure"); msg];
          Expr_list [];
        ];
      }
  | "invalid_arg", [msg] ->
      (* invalid_arg msg => erlang:raise(error, {invalid_argument, msg}, []) *)
      Expr_call {
        module_ = Expr_lit (Lit_atom "erlang");
        func = Expr_lit (Lit_atom "raise");
        args = [
          Expr_lit (Lit_atom "error");
          Expr_tuple [Expr_lit (Lit_atom "invalid_argument"); msg];
          Expr_list [];
        ];
      }
  | _ ->
      (* Unknown stdlib function - call erlang module *)
      Expr_call {
        module_ = Expr_lit (Lit_atom "erlang");
        func = Expr_lit (Lit_atom op);
        args;
      }

(** Convert Merlot_stdlib.List function calls to inline implementations *)
and convert_merlot_list_call func_name args =
  match func_name, args with
  | "nth", [lst; n] ->
      (* List.nth lst n -> lists:nth(n + 1, lst)  (0-indexed to 1-indexed) *)
      Expr_call {
        module_ = Expr_lit (Lit_atom "lists");
        func = Expr_lit (Lit_atom "nth");
        args = [Expr_binary (Op_add, n, Expr_lit (Lit_int 1)); lst];
      }
  | "length", [lst] ->
      Expr_call {
        module_ = Expr_lit (Lit_atom "erlang");
        func = Expr_lit (Lit_atom "length");
        args = [lst];
      }
  | "hd", [lst] ->
      Expr_call {
        module_ = Expr_lit (Lit_atom "erlang");
        func = Expr_lit (Lit_atom "hd");
        args = [lst];
      }
  | "tl", [lst] ->
      Expr_call {
        module_ = Expr_lit (Lit_atom "erlang");
        func = Expr_lit (Lit_atom "tl");
        args = [lst];
      }
  | "rev", [lst] ->
      Expr_call {
        module_ = Expr_lit (Lit_atom "lists");
        func = Expr_lit (Lit_atom "reverse");
        args = [lst];
      }
  | "append", [a; b] ->
      Expr_binary (Op_append, a, b)
  | "concat", [lst] | "flatten", [lst] ->
      Expr_call {
        module_ = Expr_lit (Lit_atom "lists");
        func = Expr_lit (Lit_atom "append");
        args = [lst];
      }
  | "map", [f; lst] ->
      Expr_call {
        module_ = Expr_lit (Lit_atom "lists");
        func = Expr_lit (Lit_atom "map");
        args = [f; lst];
      }
  | "filter", [pred; lst] ->
      Expr_call {
        module_ = Expr_lit (Lit_atom "lists");
        func = Expr_lit (Lit_atom "filter");
        args = [pred; lst];
      }
  | "mem", [elem; lst] ->
      Expr_call {
        module_ = Expr_lit (Lit_atom "lists");
        func = Expr_lit (Lit_atom "member");
        args = [elem; lst];
      }
  | "exists", [pred; lst] ->
      Expr_call {
        module_ = Expr_lit (Lit_atom "lists");
        func = Expr_lit (Lit_atom "any");
        args = [pred; lst];
      }
  | "for_all", [pred; lst] ->
      Expr_call {
        module_ = Expr_lit (Lit_atom "lists");
        func = Expr_lit (Lit_atom "all");
        args = [pred; lst];
      }
  | "combine", [a; b] ->
      Expr_call {
        module_ = Expr_lit (Lit_atom "lists");
        func = Expr_lit (Lit_atom "zip");
        args = [a; b];
      }
  | "split", [lst] ->
      Expr_call {
        module_ = Expr_lit (Lit_atom "lists");
        func = Expr_lit (Lit_atom "unzip");
        args = [lst];
      }
  | _ ->
      (* Unknown List function - generate call that will fail at runtime with clear error *)
      failwith (Printf.sprintf "Merlot_stdlib.List.%s is not yet implemented" func_name)

(** Convert Merlot_stdlib.Option function calls *)
and convert_merlot_option_call func_name args =
  match func_name, args with
  | "some", [v] -> Expr_tuple [Expr_lit (Lit_atom "some"); v]
  | "none", [] -> Expr_lit (Lit_atom "none")
  | "is_some", [opt] ->
      Expr_case {
        scrutinee = opt;
        clauses = [
          (Pat_tuple [Pat_lit (Lit_atom "some"); Pat_var "_"], None, Expr_lit (Lit_atom "true"));
          (Pat_var "_", None, Expr_lit (Lit_atom "false"));
        ];
      }
  | "is_none", [opt] ->
      Expr_case {
        scrutinee = opt;
        clauses = [
          (Pat_lit (Lit_atom "none"), None, Expr_lit (Lit_atom "true"));
          (Pat_var "_", None, Expr_lit (Lit_atom "false"));
        ];
      }
  | _ ->
      failwith (Printf.sprintf "Merlot_stdlib.Option.%s is not yet implemented" func_name)

(** Convert Merlot_stdlib.Result function calls *)
and convert_merlot_result_call func_name args =
  match func_name, args with
  | "ok", [v] -> Expr_tuple [Expr_lit (Lit_atom "ok"); v]
  | "error", [e] -> Expr_tuple [Expr_lit (Lit_atom "error"); e]
  | "is_ok", [res] ->
      Expr_case {
        scrutinee = res;
        clauses = [
          (Pat_tuple [Pat_lit (Lit_atom "ok"); Pat_var "_"], None, Expr_lit (Lit_atom "true"));
          (Pat_var "_", None, Expr_lit (Lit_atom "false"));
        ];
      }
  | "is_error", [res] ->
      Expr_case {
        scrutinee = res;
        clauses = [
          (Pat_tuple [Pat_lit (Lit_atom "error"); Pat_var "_"], None, Expr_lit (Lit_atom "true"));
          (Pat_var "_", None, Expr_lit (Lit_atom "false"));
        ];
      }
  | _ ->
      failwith (Printf.sprintf "Merlot_stdlib.Result.%s is not yet implemented" func_name)

(** Convert builtin or regular function application *)
and convert_builtin_or_apply id args =
  let name = Ident.name id in
  match name, args with
  | "+", [a; b] -> Expr_binary (Op_add, a, b)
  | "-", [a; b] -> Expr_binary (Op_sub, a, b)
  | "*", [a; b] -> Expr_binary (Op_mul, a, b)
  | "/", [a; b] -> Expr_binary (Op_idiv, a, b)  (* Integer division *)
  | _ ->
      (* Check if it's a known top-level function *)
      (match lookup_top_level_function id with
       | Some (func_name, arity) ->
           let num_args = List.length args in
           if num_args = arity then
             (* Full application - direct local call *)
             Expr_local_call { func_name; arity; args }
           else if num_args < arity then
             (* Partial application - wrap remaining args in lambdas *)
             let missing = arity - num_args in
             let extra_vars = List.init missing (fun i -> fresh_var (Printf.sprintf "Arg%d" (i + 1))) in
             let extra_args = List.map (fun v -> Expr_var v) extra_vars in
             let full_call = Expr_local_call {
               func_name;
               arity;
               args = args @ extra_args
             } in
             (* Wrap in nested lambdas for currying *)
             List.fold_right (fun param body ->
               Expr_fun { params = [param]; body }
             ) extra_vars full_call
           else
             (* More args than arity - shouldn't happen with well-typed code *)
             Expr_local_call { func_name; arity; args }
       | None ->
           (* Local variable (function parameter) - use apply with variable *)
           Expr_apply (Expr_var (convert_ident id), args))

(** Convert match expression with computation cases (OCaml 5.4+) *)
and convert_match scrutinee cases =
  let clauses = List.filter_map (fun (c : Typedtree.computation Typedtree.case) ->
    (* Skip exception patterns for now *)
    match c.c_lhs.pat_desc with
    | Tpat_exception _ -> None
    | _ ->
        let pat = convert_computation_pattern c.c_lhs in
        let guard = Option.map convert_expr c.c_guard in
        let body = convert_expr c.c_rhs in
        Some (pat, guard, body)
  ) cases in
  Expr_case {
    scrutinee = convert_expr scrutinee;
    clauses;
  }

(** Convert match expression with value cases *)
and convert_value_match scrutinee cases =
  let clauses = List.map (fun (c : Typedtree.value Typedtree.case) ->
    let pat = convert_pattern c.c_lhs in
    let guard = Option.map convert_expr c.c_guard in
    let body = convert_expr c.c_rhs in
    (pat, guard, body)
  ) cases in
  Expr_case {
    scrutinee = convert_expr scrutinee;
    clauses;
  }

(** Convert constructor application *)
and convert_construct cd args =
  let name = cd.cstr_name in
  match name, args with
  | "()", [] -> Expr_lit (Lit_atom "ok")
  | "true", [] -> Expr_lit (Lit_atom "true")
  | "false", [] -> Expr_lit (Lit_atom "false")
  | "[]", [] -> Expr_lit Lit_nil
  | "::", [hd; tl] -> Expr_cons (convert_expr hd, convert_expr tl)
  | "None", [] -> Expr_lit (Lit_atom "none")
  | "Some", [e] -> Expr_tuple [Expr_lit (Lit_atom "some"); convert_expr e]
  | "Ok", [e] -> Expr_tuple [Expr_lit (Lit_atom "ok"); convert_expr e]
  | "Error", [e] -> Expr_tuple [Expr_lit (Lit_atom "error"); convert_expr e]
  | _, [] -> Expr_lit (Lit_atom (String.lowercase_ascii name))
  | _, _ ->
      Expr_tuple (Expr_lit (Lit_atom (String.lowercase_ascii name)) ::
                  List.map convert_expr args)

(** Convert polymorphic variant *)
and convert_variant label arg_opt =
  let atom = Expr_lit (Lit_atom label) in
  match arg_opt with
  | None -> atom
  | Some e -> Expr_tuple [atom; convert_expr e]

(** Convert record expression *)
and convert_record fields extended_expression =
  (* OCaml 5.4: fields is (label_description * record_label_definition) array *)
  (* Get record name from first field's type *)
  let record_name = match fields.(0) with
    | (lbl, _) ->
        let type_name = match Types.get_desc lbl.lbl_res with
          | Types.Tconstr (path, _, _) -> Path.name path
          | _ -> "record"
        in
        String.lowercase_ascii (Filename.basename type_name)
  in
  (* Check if this should be a map:
     1. 'type t' in elixir_struct mode (produces Elixir struct with __struct__)
     2. Any type marked with [@@elixir] (produces plain map for imported structs) *)
  let is_elixir_struct = !elixir_struct_mode && record_name = "t" in
  let is_elixir_import = is_elixir_type record_name in
  if is_elixir_struct then
    (* Elixir struct mode: type t becomes a map with __struct__ key *)
    let full_module_name = "Elixir." ^ String.capitalize_ascii !current_module_name in
    (* Convert base expression if this is a record update *)
    let base_expr = Option.map convert_expr extended_expression in
    (* Build field pairs for the map *)
    let field_pairs = Array.to_list fields |> List.map (fun (lbl, def) ->
      let key = Expr_lit (Lit_atom lbl.lbl_name) in
      let value = match def with
        | Typedtree.Kept _ ->
            (* Field kept from base record - use maps:get to extract *)
            (match base_expr with
             | Some base ->
                 Expr_call {
                   module_ = Expr_lit (Lit_atom "maps");
                   func = Expr_lit (Lit_atom "get");
                   args = [key; base];
                 }
             | None -> failwith "Kept field without base expression")
        | Typedtree.Overridden (_, e) -> convert_expr e
      in
      (key, value)
    ) in
    (* Add __struct__ key *)
    let struct_pair = (Expr_lit (Lit_atom "__struct__"), Expr_lit (Lit_atom full_module_name)) in
    Expr_map (struct_pair :: field_pairs)
  else if is_elixir_import then
    (* Elixir import: type marked with [@@elixir] becomes a plain map (no __struct__) *)
    let base_expr = Option.map convert_expr extended_expression in
    let field_pairs = Array.to_list fields |> List.map (fun (lbl, def) ->
      let key = Expr_lit (Lit_atom lbl.lbl_name) in
      let value = match def with
        | Typedtree.Kept _ ->
            (match base_expr with
             | Some base ->
                 Expr_call {
                   module_ = Expr_lit (Lit_atom "maps");
                   func = Expr_lit (Lit_atom "get");
                   args = [key; base];
                 }
             | None -> failwith "Kept field without base expression")
        | Typedtree.Overridden (_, e) -> convert_expr e
      in
      (key, value)
    ) in
    Expr_map field_pairs
  else
    (* Normal mode: records become tuples with a tag *)
    let base_expr = Option.map convert_expr extended_expression in
    let field_exprs = Array.to_list fields |> List.map (fun (lbl, def) ->
      match def with
      | Typedtree.Kept _ ->
          (* Field kept from base record - use element/2 to extract *)
          let idx = lbl.lbl_pos + 2 in  (* +1 for 1-indexing, +1 for tag *)
          (match base_expr with
           | Some base ->
               Expr_call {
                 module_ = Expr_lit (Lit_atom "erlang");
                 func = Expr_lit (Lit_atom "element");
                 args = [Expr_lit (Lit_int idx); base];
               }
           | None -> failwith "Kept field without base expression")
      | Typedtree.Overridden (_, e) -> convert_expr e
    ) in
    Expr_tuple (Expr_lit (Lit_atom record_name) :: field_exprs)

(** Convert try expression
    In OCaml 5.4, exception handlers are value cases where the pattern
    describes the exception to catch. *)
and convert_try body (cases : Typedtree.value Typedtree.case list) =
  let class_var = fresh_var "Class" in
  let reason_var = fresh_var "Reason" in
  let stack_var = fresh_var "Stack" in

  let catch_body = match cases with
    | [] ->
        (* No exception handlers - re-raise the exception *)
        Expr_call {
          module_ = Expr_lit (Lit_atom "erlang");
          func = Expr_lit (Lit_atom "raise");
          args = [Expr_var class_var; Expr_var reason_var; Expr_var stack_var];
        }
    | _ ->
        (* Convert exception handlers to case clauses *)
        let clauses = List.map (fun (c : Typedtree.value Typedtree.case) ->
          let pat = convert_pattern c.c_lhs in
          let guard = Option.map convert_expr c.c_guard in
          let body = convert_expr c.c_rhs in
          (pat, guard, body)
        ) cases in
        Expr_case {
          scrutinee = Expr_var reason_var;
          clauses;
        }
  in

  Expr_try {
    body = convert_expr body;
    catch_var = (class_var, reason_var, stack_var);
    catch_body;
  }

(** Convert a structure item (top-level definition) *)
let convert_structure_item (item : Typedtree.structure_item) : Beam_ir.fun_def list =
  match item.str_desc with
  | Tstr_value (rec_flag, bindings) ->
      let _ = rec_flag in
      List.filter_map (fun vb ->
        match vb.Typedtree.vb_pat.pat_desc with
        | Tpat_var (id, _, _) ->
            let name = String.lowercase_ascii (Ident.name id) in
            let params, body = extract_function vb.vb_expr in
            let extracted_arity = List.length params in
            (* Get type-based arity to detect function aliases *)
            let type_arity = arity_of_type vb.Typedtree.vb_expr.exp_type in
            (* Check if body is a function reference (alias case) *)
            let is_func_ref = match body with
              | Expr_fun_ref _ -> true
              | Expr_local_call { args = []; _ } -> true  (* zero-arg call is a ref *)
              | _ -> false
            in
            let params, body, arity =
              if extracted_arity = 0 && type_arity > 0 && is_func_ref then
                (* Function alias: let f = g where g has arity N *)
                (* Generate wrapper: fun(A1, ..., AN) -> apply body (A1, ..., AN) *)
                let param_names = List.init type_arity (fun i ->
                  fresh_var (Printf.sprintf "Arg%d" (i + 1))) in
                let wrapped_body = Expr_apply (body,
                  List.map (fun p -> Expr_var p) param_names) in
                (param_names, wrapped_body, type_arity)
              else if extracted_arity = 0 then
                (* Zero-arity value - wrap in 1-arity function *)
                (["_"], body, 1)
              else
                (params, body, extracted_arity)
            in
            Some {
              name;
              arity;
              params;
              body;
              loc = no_loc;  (* TODO: extract location *)
            }
        | _ -> None
      ) bindings
  | Tstr_primitive vd ->
      (* Register external for later lookup during apply conversion *)
      register_external vd.val_id vd.val_prim;
      []
  | Tstr_type (_, type_decls) ->
      (* Type declarations generate pp_<typename> functions for pretty-printing *)
      let pp_funcs = List.filter_map (fun (td : Typedtree.type_declaration) ->
        let type_name = Ident.name td.typ_id in
        (* Check for [@@elixir] or [@@elixir "Module.Name"] attribute *)
        let elixir_attr = List.find_opt (fun (attr : Parsetree.attribute) ->
          attr.attr_name.txt = "elixir"
        ) td.typ_attributes in
        (match elixir_attr with
         | Some attr ->
             (* Extract optional explicit module name from payload *)
             let explicit_module = match attr.attr_payload with
               | Parsetree.PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant {pconst_desc = Pconst_string (s, _, _); _}; _}, _); _}] ->
                   Some s
               | _ -> None
             in
             register_elixir_type type_name explicit_module
         | None -> ());
        (* In elixir_struct mode, also process 'type t' for __struct__ generation *)
        if !elixir_struct_mode && type_name = "t" then
          (match td.typ_kind with
          | Ttype_record labels ->
              let field_names = List.map (fun (ld : Typedtree.label_declaration) ->
                Ident.name ld.ld_id
              ) labels in
              elixir_struct_record := Some { field_names }
          | _ -> ());
        (* Generate pp function for non-polymorphic types *)
        generate_pp_for_type td
      ) type_decls in
      pp_funcs
  | Tstr_exception _ -> []  (* TODO: handle exceptions *)
  | Tstr_module _ -> []  (* TODO: handle submodules *)
  | Tstr_modtype _ -> []
  | Tstr_open _ -> []
  | Tstr_include _ -> []  (* TODO *)
  | Tstr_class _ -> []  (* Objects not supported *)
  | Tstr_class_type _ -> []
  | Tstr_attribute _ -> []
  | Tstr_eval (e, _) ->
      (* Top-level expression becomes a function *)
      let body = convert_expr e in
      [{ name = "main"; arity = 1; params = ["_"]; body; loc = no_loc }]
  | Tstr_recmodule _ -> []
  | Tstr_typext _ -> []

(** Collect external declarations from structure (first pass) *)
let collect_externals (str : Typedtree.structure) =
  List.iter (fun item ->
    match item.Typedtree.str_desc with
    | Tstr_primitive vd ->
        register_external vd.val_id vd.val_prim
    | _ -> ()
  ) str.str_items

(** Collect top-level function names and arities (first pass) *)
let collect_top_level_functions (str : Typedtree.structure) =
  List.iter (fun item ->
    match item.Typedtree.str_desc with
    | Tstr_value (_, bindings) ->
        List.iter (fun vb ->
          match vb.Typedtree.vb_pat.pat_desc with
          | Tpat_var (id, _, _) ->
              let arity = arity_of_type vb.Typedtree.vb_expr.exp_type in
              if arity > 0 then
                register_top_level_function id arity
          | _ -> ()
        ) bindings
    | _ -> ()
  ) str.str_items

(** Check if a function name is a macro definition (starts with "macro_") *)
let is_macro_definition name =
  String.length name > 6 && String.sub name 0 6 = "macro_"

(** Extract macro name from "macro_foo" -> "foo" *)
let macro_name_of_function name =
  String.sub name 6 (String.length name - 6)

(** Check if an expression is a Macro.defmacro call and extract the lambda.
    Returns Some (lambda_expr) if it matches, None otherwise. *)
let extract_defmacro_lambda (expr : Typedtree.expression) : Typedtree.expression option =
  match expr.exp_desc with
  | Texp_apply (func, args) ->
      (match func.exp_desc with
       | Texp_ident (path, _, _) when is_macro_path path ->
           (match get_path_func_name path with
            | Some "defmacro" ->
                (* Extract the lambda argument *)
                (match args with
                 | [(_, Typedtree.Arg lambda_expr)] -> Some lambda_expr
                 | _ -> None)
            | _ -> None)
       | _ -> None)
  | _ -> None

(** Check if an expression is a Macro.defmacro call *)
let is_defmacro_definition (expr : Typedtree.expression) : bool =
  Option.is_some (extract_defmacro_lambda expr)

(** Extract parameter names from a function expression *)
let rec extract_param_names_from_expr (expr : Typedtree.expression) : string list =
  match expr.exp_desc with
  | Texp_function (params, Tfunction_body body_expr) ->
      let param_names = List.filter_map (fun fp ->
        match get_param_type fp with
        | Some ty when is_unit_type ty -> None
        | _ -> Some (Ident.name fp.fp_param)
      ) params in
      let more_params = extract_param_names_from_expr body_expr in
      param_names @ more_params
  | Texp_function (params, Tfunction_cases _) ->
      List.filter_map (fun fp ->
        match get_param_type fp with
        | Some ty when is_unit_type ty -> None
        | _ -> Some (Ident.name fp.fp_param)
      ) params
  | _ -> []

(** Track defmacro definitions for stub generation.
    Maps macro name to its arity (number of parameters). *)
let defmacro_definitions : (string, int) Hashtbl.t = Hashtbl.create 16

let reset_defmacro_definitions () =
  Hashtbl.clear defmacro_definitions

let register_defmacro name arity =
  Hashtbl.replace defmacro_definitions name arity

let is_defmacro_binding name =
  Hashtbl.mem defmacro_definitions name

(** Collect macro definitions and register them.
    Supports two forms:
    1. let macro_foo x y = Macro.quote (...)  -- legacy prefix convention
    2. let foo = Macro.defmacro (fun x y -> Macro.quote (...))  -- explicit defmacro
*)
let collect_macro_definitions (str : Typedtree.structure) =
  List.iter (fun item ->
    match item.Typedtree.str_desc with
    | Tstr_value (_, bindings) ->
        List.iter (fun vb ->
          match vb.Typedtree.vb_pat.pat_desc with
          | Tpat_var (id, _, _) ->
              let name = Ident.name id in
              (* Check for macro_ prefix convention *)
              if is_macro_definition name then begin
                let macro_name = macro_name_of_function name in
                let param_names = extract_param_names_from_expr vb.Typedtree.vb_expr in
                (* Convert the function body to BEAM IR - this should be the macro template *)
                let _, body = extract_function vb.Typedtree.vb_expr in
                let macro_def : Beam_ir.macro_def = {
                  macro_name;
                  macro_params = param_names;
                  macro_body = body;
                  macro_loc = Beam_ir.no_loc;
                } in
                Macro_expand.register_macro macro_def
              end
              (* Check for Macro.defmacro form *)
              else begin
                match extract_defmacro_lambda vb.Typedtree.vb_expr with
                | Some lambda_expr ->
                    let macro_name = name in
                    let param_names = extract_param_names_from_expr lambda_expr in
                    let _, body = extract_function lambda_expr in
                    let macro_def : Beam_ir.macro_def = {
                      macro_name;
                      macro_params = param_names;
                      macro_body = body;
                      macro_loc = Beam_ir.no_loc;
                    } in
                    Macro_expand.register_macro macro_def;
                    (* Register for stub generation *)
                    register_defmacro name (List.length param_names)
                | None -> ()
              end
          | _ -> ()
        ) bindings
    | _ -> ()
  ) str.str_items

(** Generate a stub function for a defmacro definition.
    The stub simply returns its last argument (identity-like behavior). *)
let generate_defmacro_stub (name : string) (arity : int) : Beam_ir.fun_def =
  let params = List.init arity (fun i -> Printf.sprintf "X%d" i) in
  let body =
    if arity = 0 then Beam_ir.Expr_lit (Beam_ir.Lit_atom "ok")
    else Beam_ir.Expr_var (Printf.sprintf "X%d" (arity - 1))
  in
  { name; arity; params; body; loc = Beam_ir.no_loc }

(** Generate __struct__/0 function for Elixir struct compatibility.
    Returns a map with __struct__ key and all fields set to nil. *)
let generate_struct_0 ~module_name (info : record_info) : Beam_ir.fun_def =
  let full_module_name = "Elixir." ^ String.capitalize_ascii module_name in
  (* Build the map: #{ '__struct__' => 'Elixir.Module', field1 => nil, ... } *)
  let struct_pair = (Expr_lit (Lit_atom "__struct__"), Expr_lit (Lit_atom full_module_name)) in
  let field_pairs = List.map (fun field ->
    (Expr_lit (Lit_atom field), Expr_lit (Lit_atom "nil"))
  ) info.field_names in
  let body = Expr_map (struct_pair :: field_pairs) in
  { name = "__struct__"; arity = 0; params = []; body; loc = Beam_ir.no_loc }

(** Generate __struct__/1 function for Elixir struct compatibility.
    Merges the default struct with provided overrides.
    Note: Elixir passes keyword lists (list of {atom, value} tuples) to __struct__/1,
    so we need to convert it to a map first with maps:from_list/1. *)
let generate_struct_1 ~module_name (_info : record_info) : Beam_ir.fun_def =
  let full_module_name = "Elixir." ^ String.capitalize_ascii module_name in
  (* Body: maps:merge(__struct__(), maps:from_list(Overrides)) *)
  let struct_call = Expr_call {
    module_ = Expr_lit (Lit_atom full_module_name);
    func = Expr_lit (Lit_atom "__struct__");
    args = [];
  } in
  let overrides_as_map = Expr_call {
    module_ = Expr_lit (Lit_atom "maps");
    func = Expr_lit (Lit_atom "from_list");
    args = [Expr_var "Overrides"];
  } in
  let body = Expr_call {
    module_ = Expr_lit (Lit_atom "maps");
    func = Expr_lit (Lit_atom "merge");
    args = [struct_call; overrides_as_map];
  } in
  { name = "__struct__"; arity = 1; params = ["Overrides"]; body; loc = Beam_ir.no_loc }

(** Convert a complete typed structure to a BEAM module *)
let convert_structure ~module_name (str : Typedtree.structure) : Beam_ir.module_def =
  reset_vars ();
  reset_externals ();
  reset_top_level_functions ();
  reset_defmacro_definitions ();
  reset_elixir_struct ();
  Macro_expand.reset ();
  (* Store module name for use in elixir_struct mode *)
  current_module_name := module_name;
  (* Check for [@@@elixir_struct] attribute *)
  elixir_struct_mode := has_elixir_struct_attribute str;
  (* First pass: collect all external declarations, top-level functions, and macro definitions *)
  collect_externals str;
  collect_top_level_functions str;
  collect_macro_definitions str;
  (* Second pass: convert structure items (excluding macro definitions) *)
  let functions : Beam_ir.fun_def list = List.concat_map convert_structure_item str.str_items in
  (* Filter out macro_ prefix definitions from the output - they don't become runtime functions *)
  let functions = List.filter (fun (f : Beam_ir.fun_def) ->
    not (is_macro_definition f.name)
  ) functions in
  (* Replace defmacro bindings with auto-generated stubs *)
  let functions = List.filter_map (fun (f : Beam_ir.fun_def) ->
    if is_defmacro_binding f.name then
      match Hashtbl.find_opt defmacro_definitions f.name with
      | Some arity -> Some (generate_defmacro_stub f.name arity)
      | None -> Some f  (* shouldn't happen *)
    else
      Some f
  ) functions in
  (* Add __struct__/0 and __struct__/1 for elixir_struct modules *)
  let functions =
    if !elixir_struct_mode then
      match !elixir_struct_record with
      | Some info ->
          let struct_0 = generate_struct_0 ~module_name info in
          let struct_1 = generate_struct_1 ~module_name info in
          struct_0 :: struct_1 :: functions
      | None -> functions
    else functions
  in
  let exports = List.map (fun (f : Beam_ir.fun_def) -> (f.name, f.arity)) functions in
  let module_def = {
    Beam_ir.name = prefixed_module_name module_name;
    exports;
    attributes = [];
    functions;
  } in
  (* Third pass: expand macros *)
  Macro_expand.expand_module module_def
