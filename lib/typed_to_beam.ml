(** Typed_to_beam - Convert OCaml TypedTree to BEAM IR

    This is the core transformation that maps OCaml's typed AST
    to our stable BEAM intermediate representation.
*)

open Beam_ir
open Data_types

(** Add the Merlot. prefix to a module name for namespacing.
    This prevents conflicts with Erlang/OTP modules like 'supervisor' and 'process'.
    Example: "process" -> "Merlot.Process" *)
let prefixed_module_name name =
  "Merlot." ^ String.capitalize_ascii name

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

(** Top-level functions map: name -> arity *)
let top_level_functions : (string, int) Hashtbl.t = Hashtbl.create 16

let reset_top_level_functions () = Hashtbl.clear top_level_functions

let register_top_level_function name arity =
  Hashtbl.add top_level_functions name arity

let lookup_top_level_function name =
  Hashtbl.find_opt top_level_functions name

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
      (* Build: lists:flatten(io_lib:format(fmt, [arg1, arg2, ...])) *)
      let format_call = Expr_call {
        module_ = Expr_lit (Lit_atom "io_lib");
        func = Expr_lit (Lit_atom "format");
        args = [
          Expr_lit (Lit_string erlang_fmt);
          Expr_list value_args;
        ];
      } in
      (* Flatten the result to get a proper string *)
      Expr_call {
        module_ = Expr_lit (Lit_atom "lists");
        func = Expr_lit (Lit_atom "flatten");
        args = [format_call];
      }
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

(** Get function name from path *)
let get_path_func_name path =
  match path with
  | Path.Pdot (_, name) -> Some name
  | _ -> None

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
      let name = Ident.name id in
      (* Check if this is a top-level function being used as a value *)
      (match lookup_top_level_function name with
       | Some arity ->
           (* Function reference *)
           Expr_fun_ref { func_name = name; arity }
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
      (* Record field access becomes element/2 call *)
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

  | Texp_try (body, _comp_cases, val_cases) ->
      convert_try body val_cases

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
            let name = Ident.name id in
            (match lookup_top_level_function name with
             | Some arity -> Expr_fun_ref { func_name = name; arity }
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
        if is_simple_var_pattern pat then
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
      (* First pass: collect all recursive function names and arities *)
      let rec_info = List.map (fun vb ->
        let name = String.lowercase_ascii (pat_to_var vb.Typedtree.vb_pat) in
        let arity = arity_of_type vb.Typedtree.vb_expr.exp_type in
        (vb, name, arity)
      ) bindings in
      (* Register letrec functions so they can be referenced in function bodies AND in body *)
      List.iter (fun (_, name, arity) ->
        register_top_level_function name arity
      ) rec_info;
      (* Extract function bodies with the letrec names in scope *)
      let rec_bindings = List.map (fun (vb, name, _) ->
        let params, func_body = extract_function vb.Typedtree.vb_expr in
        (name, params, func_body)
      ) rec_info in
      (* Convert body with letrec names still in scope *)
      let converted_body = convert_expr body in
      (* Remove the temporary registrations after converting everything *)
      List.iter (fun (_, name, _) ->
        Hashtbl.remove top_level_functions name
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
                if func_name = "sprintf" then
                  (* sprintf: lists:flatten(io_lib:format(fmt, [args])) *)
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
                  (* printf: io:format(fmt, [args]) *)
                  Expr_call {
                    module_ = Expr_lit (Lit_atom "io");
                    func = Expr_lit (Lit_atom "format");
                    args = [
                      Expr_lit (Lit_string erlang_fmt);
                      Expr_list value_args;
                    ];
                  }
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
           (* Data-first pipe: x |. f a b  =>  f x a b *)
           (match arg_exprs with
            | [x; f] ->
                (match f with
                 | Expr_apply (func, args) -> Expr_apply (func, x :: args)
                 | Expr_local_call { func_name; arity = _; args } ->
                     (* Arity is determined by actual args, not the partial application arity *)
                     Expr_local_call { func_name; arity = List.length args + 1; args = x :: args }
                 | Expr_call { module_; func; args } ->
                     Expr_call { module_; func; args = x :: args }
                 | _ -> Expr_apply (f, [x]))
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
      (* Nested module path: Parent.Sub.func -> call sub:func *)
      (* For Merlot_stdlib.Actor.func -> call actor:func *)
      let _ = parent_mod in  (* Ignore parent, use submodule as erlang module *)
      let mod_name = String.lowercase_ascii sub_mod in
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
  | "/", [a; b] -> Expr_binary (Op_div, a, b)
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
  | _ ->
      (* Unknown stdlib function - call erlang module *)
      Expr_call {
        module_ = Expr_lit (Lit_atom "erlang");
        func = Expr_lit (Lit_atom op);
        args;
      }

(** Convert builtin or regular function application *)
and convert_builtin_or_apply id args =
  let name = Ident.name id in
  match name, args with
  | "+", [a; b] -> Expr_binary (Op_add, a, b)
  | "-", [a; b] -> Expr_binary (Op_sub, a, b)
  | "*", [a; b] -> Expr_binary (Op_mul, a, b)
  | "/", [a; b] -> Expr_binary (Op_div, a, b)
  | _ ->
      (* Check if it's a known top-level function *)
      (match lookup_top_level_function name with
       | Some arity ->
           let num_args = List.length args in
           if num_args = arity then
             (* Full application - direct local call *)
             Expr_local_call { func_name = name; arity; args }
           else if num_args < arity then
             (* Partial application - wrap remaining args in lambdas *)
             let missing = arity - num_args in
             let extra_vars = List.init missing (fun i -> fresh_var (Printf.sprintf "Arg%d" (i + 1))) in
             let extra_args = List.map (fun v -> Expr_var v) extra_vars in
             let full_call = Expr_local_call {
               func_name = name;
               arity;
               args = args @ extra_args
             } in
             (* Wrap in nested lambdas for currying *)
             List.fold_right (fun param body ->
               Expr_fun { params = [param]; body }
             ) extra_vars full_call
           else
             (* More args than arity - shouldn't happen with well-typed code *)
             Expr_local_call { func_name = name; arity; args }
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
  (* Convert base expression if this is a record update *)
  let base_expr = Option.map convert_expr extended_expression in
  (* Convert each field - either from override or from base record *)
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
  (* Records become tuples with a tag *)
  Expr_tuple (Expr_lit (Lit_atom record_name) :: field_exprs)

(** Convert try expression *)
and convert_try body cases =
  let class_var = fresh_var "Class" in
  let reason_var = fresh_var "Reason" in
  let stack_var = fresh_var "Stack" in

  let catch_body = match cases with
    | [] -> Expr_primop { name = "raise"; args = [Expr_var reason_var] }
    | _ ->
        let clauses = List.map (fun c ->
          let pat = convert_pattern c.Typedtree.c_lhs in
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
  | Tstr_type _ -> []  (* Type declarations don't generate code *)
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
              let name = Ident.name id in
              let arity = arity_of_type vb.Typedtree.vb_expr.exp_type in
              if arity > 0 then
                register_top_level_function name arity
          | _ -> ()
        ) bindings
    | _ -> ()
  ) str.str_items

(** Convert a complete typed structure to a BEAM module *)
let convert_structure ~module_name (str : Typedtree.structure) : Beam_ir.module_def =
  reset_vars ();
  reset_externals ();
  reset_top_level_functions ();
  (* First pass: collect all external declarations and top-level functions *)
  collect_externals str;
  collect_top_level_functions str;
  (* Second pass: convert structure items *)
  let functions : Beam_ir.fun_def list = List.concat_map convert_structure_item str.str_items in
  let exports = List.map (fun (f : Beam_ir.fun_def) -> (f.name, f.arity)) functions in
  {
    (* Use Merlot. prefix to prevent shadowing Erlang/OTP modules *)
    name = prefixed_module_name module_name;
    exports;
    attributes = [];
    functions;
  }
