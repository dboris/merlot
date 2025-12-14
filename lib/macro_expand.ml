(** Macro_expand - Macro expansion for BEAM IR

    This module handles compile-time macro expansion. Macros are functions
    that transform BEAM IR ASTs, similar to Elixir's macro system.

    The expansion process:
    1. Collect macro definitions from the module
    2. Expand macro invocations, substituting arguments
    3. Apply hygiene renaming to prevent variable capture
    4. Return pure BEAM IR with no macro constructs
*)

open Beam_ir

(** Expansion state *)
type expansion_state = {
  mutable next_expansion_id: int;
  macros: (string, macro_def) Hashtbl.t;
}

(** Create a fresh expansion state *)
let create_state () = {
  next_expansion_id = 0;
  macros = Hashtbl.create 16;
}

(** Global expansion state - reset between modules *)
let state = create_state ()

let reset () =
  state.next_expansion_id <- 0;
  Hashtbl.clear state.macros

(** Generate a fresh expansion ID *)
let fresh_expansion_id () =
  let id = state.next_expansion_id in
  state.next_expansion_id <- id + 1;
  id

(** Register a macro definition *)
let register_macro (macro : macro_def) =
  Hashtbl.replace state.macros macro.macro_name macro

(** Look up a macro by name *)
let lookup_macro name =
  Hashtbl.find_opt state.macros name

(** Substitution environment: maps parameter names to their argument expressions *)
type subst_env = (string, expr) Hashtbl.t

(** Create a substitution environment from macro params and call arguments *)
let make_subst_env params args =
  let env = Hashtbl.create (List.length params) in
  List.iter2 (fun param arg -> Hashtbl.add env param arg) params args;
  env

(** Set of variable names that should not be renamed (injected variables) *)
module StringSet = Set.Make(String)

(** Apply hygiene context to variables in an expression.
    Variables defined within the macro body get tagged with the macro's context,
    while spliced expressions keep their original context.
    The 'injected' set contains variable names that should NOT be renamed. *)
let rec apply_hygiene_with_injected (ctx : hygiene_ctx) (injected : StringSet.t) (expr : expr) : expr =
  match expr with
  | Expr_var name ->
      if StringSet.mem name injected then
        (* Injected variable - do NOT rename *)
        Expr_var name
      else
        (* Tag variable with hygiene context *)
        let hygienic_name = Printf.sprintf "%s__%s_%d"
          name ctx.macro_name ctx.expansion_id in
        Expr_var hygienic_name
  | Expr_lit _ -> expr
  | Expr_tuple exprs -> Expr_tuple (List.map (apply_hygiene_with_injected ctx injected) exprs)
  | Expr_cons (hd, tl) -> Expr_cons (apply_hygiene_with_injected ctx injected hd, apply_hygiene_with_injected ctx injected tl)
  | Expr_list exprs -> Expr_list (List.map (apply_hygiene_with_injected ctx injected) exprs)
  | Expr_binary (op, a, b) ->
      Expr_binary (op, apply_hygiene_with_injected ctx injected a, apply_hygiene_with_injected ctx injected b)
  | Expr_unary (op, a) -> Expr_unary (op, apply_hygiene_with_injected ctx injected a)
  | Expr_apply (func, args) ->
      Expr_apply (apply_hygiene_with_injected ctx injected func, List.map (apply_hygiene_with_injected ctx injected) args)
  | Expr_fun_ref _ -> expr
  | Expr_local_call { func_name; arity; args } ->
      Expr_local_call { func_name; arity; args = List.map (apply_hygiene_with_injected ctx injected) args }
  | Expr_call { module_; func; args } ->
      Expr_call {
        module_ = apply_hygiene_with_injected ctx injected module_;
        func = apply_hygiene_with_injected ctx injected func;
        args = List.map (apply_hygiene_with_injected ctx injected) args
      }
  | Expr_fun { params; body } ->
      (* Rename parameters with hygiene context *)
      let hygienic_params = List.map (fun p ->
        Printf.sprintf "%s__%s_%d" p ctx.macro_name ctx.expansion_id
      ) params in
      Expr_fun { params = hygienic_params; body = apply_hygiene_with_injected ctx injected body }
  | Expr_let { var; value; body } ->
      let hygienic_var = Printf.sprintf "%s__%s_%d"
        var ctx.macro_name ctx.expansion_id in
      Expr_let {
        var = hygienic_var;
        value = apply_hygiene_with_injected ctx injected value;
        body = apply_hygiene_with_injected ctx injected body
      }
  | Expr_letrec { bindings; body } ->
      let hygienic_bindings = List.map (fun (name, params, body) ->
        let h_name = Printf.sprintf "%s__%s_%d" name ctx.macro_name ctx.expansion_id in
        let h_params = List.map (fun p ->
          Printf.sprintf "%s__%s_%d" p ctx.macro_name ctx.expansion_id
        ) params in
        (h_name, h_params, apply_hygiene_with_injected ctx injected body)
      ) bindings in
      Expr_letrec { bindings = hygienic_bindings; body = apply_hygiene_with_injected ctx injected body }
  | Expr_case { scrutinee; clauses } ->
      Expr_case {
        scrutinee = apply_hygiene_with_injected ctx injected scrutinee;
        clauses = List.map (fun (pat, guard, body) ->
          let (pat', new_injected) = apply_hygiene_pattern_with_injected ctx injected pat in
          (pat',
           Option.map (apply_hygiene_with_injected ctx new_injected) guard,
           apply_hygiene_with_injected ctx new_injected body)
        ) clauses
      }
  | Expr_if { cond; then_; else_ } ->
      Expr_if {
        cond = apply_hygiene_with_injected ctx injected cond;
        then_ = apply_hygiene_with_injected ctx injected then_;
        else_ = apply_hygiene_with_injected ctx injected else_
      }
  | Expr_seq (e1, e2) -> Expr_seq (apply_hygiene_with_injected ctx injected e1, apply_hygiene_with_injected ctx injected e2)
  | Expr_try { body; catch_var = (c, r, s); catch_body } ->
      let h_c = Printf.sprintf "%s__%s_%d" c ctx.macro_name ctx.expansion_id in
      let h_r = Printf.sprintf "%s__%s_%d" r ctx.macro_name ctx.expansion_id in
      let h_s = Printf.sprintf "%s__%s_%d" s ctx.macro_name ctx.expansion_id in
      Expr_try {
        body = apply_hygiene_with_injected ctx injected body;
        catch_var = (h_c, h_r, h_s);
        catch_body = apply_hygiene_with_injected ctx injected catch_body
      }
  | Expr_receive { clauses; timeout } ->
      Expr_receive {
        clauses = List.map (fun (pat, guard, body) ->
          let (pat', new_injected) = apply_hygiene_pattern_with_injected ctx injected pat in
          (pat',
           Option.map (apply_hygiene_with_injected ctx new_injected) guard,
           apply_hygiene_with_injected ctx new_injected body)
        ) clauses;
        timeout = Option.map (fun (t, b) ->
          (apply_hygiene_with_injected ctx injected t, apply_hygiene_with_injected ctx injected b)
        ) timeout
      }
  | Expr_primop { name; args } ->
      Expr_primop { name; args = List.map (apply_hygiene_with_injected ctx injected) args }
  | Expr_map pairs ->
      Expr_map (List.map (fun (k, v) ->
        (apply_hygiene_with_injected ctx injected k, apply_hygiene_with_injected ctx injected v)
      ) pairs)
  | Expr_map_update { map; updates } ->
      Expr_map_update {
        map = apply_hygiene_with_injected ctx injected map;
        updates = List.map (fun (k, v) ->
          (apply_hygiene_with_injected ctx injected k, apply_hygiene_with_injected ctx injected v)
        ) updates
      }
  (* Macro constructs - these are handled during expansion *)
  | Expr_quote _ | Expr_unquote _ | Expr_splice _ | Expr_stringify _ -> expr

(** Apply hygiene to patterns, returning updated injected set.
    Pat_inject bindings add to the injected set so their references aren't renamed. *)
and apply_hygiene_pattern_with_injected (ctx : hygiene_ctx) (injected : StringSet.t) (pat : pattern) : pattern * StringSet.t =
  match pat with
  | Pat_any -> (Pat_any, injected)
  | Pat_var name ->
      let hygienic_name = Printf.sprintf "%s__%s_%d"
        name ctx.macro_name ctx.expansion_id in
      (Pat_var hygienic_name, injected)
  | Pat_lit _ -> (pat, injected)
  | Pat_tuple pats ->
      let (pats', injected') = List.fold_left (fun (acc_pats, acc_inj) p ->
        let (p', inj') = apply_hygiene_pattern_with_injected ctx acc_inj p in
        (p' :: acc_pats, inj')
      ) ([], injected) pats in
      (Pat_tuple (List.rev pats'), injected')
  | Pat_cons (hd, tl) ->
      let (hd', injected') = apply_hygiene_pattern_with_injected ctx injected hd in
      let (tl', injected'') = apply_hygiene_pattern_with_injected ctx injected' tl in
      (Pat_cons (hd', tl'), injected'')
  | Pat_record fields ->
      let (fields', injected') = List.fold_left (fun (acc_fields, acc_inj) (n, p) ->
        let (p', inj') = apply_hygiene_pattern_with_injected ctx acc_inj p in
        ((n, p') :: acc_fields, inj')
      ) ([], injected) fields in
      (Pat_record (List.rev fields'), injected')
  | Pat_alias (pat, var) ->
      let hygienic_var = Printf.sprintf "%s__%s_%d"
        var ctx.macro_name ctx.expansion_id in
      let (pat', injected') = apply_hygiene_pattern_with_injected ctx injected pat in
      (Pat_alias (pat', hygienic_var), injected')
  | Pat_inject var ->
      (* Unhygienic binding - do NOT rename, and add to injected set *)
      (Pat_inject var, StringSet.add var injected)

(** Wrapper for backward compatibility - starts with empty injected set *)
let apply_hygiene ctx expr = apply_hygiene_with_injected ctx StringSet.empty expr

(** Wrapper for pattern hygiene *)
let apply_hygiene_pattern ctx pat = fst (apply_hygiene_pattern_with_injected ctx StringSet.empty pat)

(** Substitute unquote/splice in a quoted expression.
    - Expr_unquote "x" -> look up x in env, insert the caller's AST
    - Expr_splice "x" -> same as Expr_unquote (legacy alias) *)
let rec substitute (env : subst_env) (expr : expr) : expr =
  match expr with
  | Expr_unquote name | Expr_splice name ->
      (match Hashtbl.find_opt env name with
       | Some arg_expr -> arg_expr
       | None -> failwith (Printf.sprintf "Unquote: unbound macro parameter '%s'" name))
  | Expr_stringify name ->
      (match Hashtbl.find_opt env name with
       | Some arg_expr ->
           (* Convert AST to string representation using pp_expr *)
           let str = Format.asprintf "%a" Beam_ir.pp_expr arg_expr in
           Expr_lit (Lit_string str)
       | None -> failwith (Printf.sprintf "Stringify: unbound variable '%s'" name))
  | Expr_quote inner ->
      (* Nested quote - substitute inside *)
      Expr_quote (substitute env inner)
  | Expr_var _ | Expr_lit _ | Expr_fun_ref _ -> expr
  | Expr_tuple exprs -> Expr_tuple (List.map (substitute env) exprs)
  | Expr_cons (hd, tl) -> Expr_cons (substitute env hd, substitute env tl)
  | Expr_list exprs -> Expr_list (List.map (substitute env) exprs)
  | Expr_binary (op, a, b) ->
      Expr_binary (op, substitute env a, substitute env b)
  | Expr_unary (op, a) -> Expr_unary (op, substitute env a)
  | Expr_apply (func, args) ->
      Expr_apply (substitute env func, List.map (substitute env) args)
  | Expr_local_call { func_name; arity; args } ->
      Expr_local_call { func_name; arity; args = List.map (substitute env) args }
  | Expr_call { module_; func; args } ->
      Expr_call {
        module_ = substitute env module_;
        func = substitute env func;
        args = List.map (substitute env) args
      }
  | Expr_fun { params; body } ->
      Expr_fun { params; body = substitute env body }
  | Expr_let { var; value; body } ->
      Expr_let { var; value = substitute env value; body = substitute env body }
  | Expr_letrec { bindings; body } ->
      Expr_letrec {
        bindings = List.map (fun (n, p, b) -> (n, p, substitute env b)) bindings;
        body = substitute env body
      }
  | Expr_case { scrutinee; clauses } ->
      Expr_case {
        scrutinee = substitute env scrutinee;
        clauses = List.map (fun (p, g, b) ->
          (p, Option.map (substitute env) g, substitute env b)
        ) clauses
      }
  | Expr_if { cond; then_; else_ } ->
      Expr_if {
        cond = substitute env cond;
        then_ = substitute env then_;
        else_ = substitute env else_
      }
  | Expr_seq (e1, e2) -> Expr_seq (substitute env e1, substitute env e2)
  | Expr_try { body; catch_var; catch_body } ->
      Expr_try {
        body = substitute env body;
        catch_var;
        catch_body = substitute env catch_body
      }
  | Expr_receive { clauses; timeout } ->
      Expr_receive {
        clauses = List.map (fun (p, g, b) ->
          (p, Option.map (substitute env) g, substitute env b)
        ) clauses;
        timeout = Option.map (fun (t, b) ->
          (substitute env t, substitute env b)
        ) timeout
      }
  | Expr_primop { name; args } ->
      Expr_primop { name; args = List.map (substitute env) args }
  | Expr_map pairs ->
      Expr_map (List.map (fun (k, v) -> (substitute env k, substitute env v)) pairs)
  | Expr_map_update { map; updates } ->
      Expr_map_update {
        map = substitute env map;
        updates = List.map (fun (k, v) -> (substitute env k, substitute env v)) updates
      }

(** Simple AST to pattern conversion for stringify *)
and ast_to_pattern (expr : expr) : pattern =
  match expr with
  | Expr_var name -> Pat_var name
  | Expr_lit lit -> Pat_lit lit
  | Expr_tuple exprs -> Pat_tuple (List.map ast_to_pattern exprs)
  | Expr_list exprs ->
      List.fold_right (fun e acc -> Pat_cons (ast_to_pattern e, acc))
        exprs (Pat_lit Lit_nil)
  | _ -> Pat_any  (* Fallback for complex expressions *)

(** Expand a single macro invocation *)
let rec expand_macro_call (macro : macro_def) (args : expr list) : expr =
  if List.length args <> List.length macro.macro_params then
    failwith (Printf.sprintf
      "Macro '%s' expects %d arguments, got %d"
      macro.macro_name
      (List.length macro.macro_params)
      (List.length args));

  (* Create hygiene context for this expansion *)
  let ctx = {
    macro_name = macro.macro_name;
    expansion_id = fresh_expansion_id ();
  } in

  (* Get the quoted body *)
  let body = match macro.macro_body with
    | Expr_quote inner -> inner
    | _ -> failwith (Printf.sprintf
        "Macro '%s' body must be a quoted expression" macro.macro_name)
  in

  (* Apply hygiene to the macro body FIRST - this renames variables
     introduced by the macro, but leaves splice markers unchanged *)
  let hygienic_body = apply_hygiene ctx body in

  (* Now substitute: splice markers get replaced with call-site expressions
     which keep their original (unhygienic) variable names *)
  let env = make_subst_env macro.macro_params args in
  let substituted = substitute env hygienic_body in

  (* Recursively expand any nested macros *)
  expand_expr substituted

(** Recursively expand macros in an expression *)
and expand_expr (expr : expr) : expr =
  match expr with
  (* Check if this is a macro call - for now, look for local calls to macros *)
  | Expr_local_call { func_name; args; _ } ->
      (match lookup_macro func_name with
       | Some macro ->
           let expanded_args = List.map expand_expr args in
           expand_macro_call macro expanded_args
       | None ->
           Expr_local_call {
             func_name;
             arity = List.length args;
             args = List.map expand_expr args
           })

  (* Remove quote wrapper after expansion *)
  | Expr_quote inner -> expand_expr inner

  (* These should have been substituted *)
  | Expr_unquote name ->
      failwith (Printf.sprintf "Unquote '%s' outside of macro expansion" name)
  | Expr_splice name ->
      failwith (Printf.sprintf "Splice '%s' outside of macro expansion" name)
  | Expr_stringify name ->
      failwith (Printf.sprintf "Stringify '%s' outside of macro expansion" name)

  (* Recursively expand in all other constructs *)
  | Expr_var _ | Expr_lit _ | Expr_fun_ref _ -> expr
  | Expr_tuple exprs -> Expr_tuple (List.map expand_expr exprs)
  | Expr_cons (hd, tl) -> Expr_cons (expand_expr hd, expand_expr tl)
  | Expr_list exprs -> Expr_list (List.map expand_expr exprs)
  | Expr_binary (op, a, b) -> Expr_binary (op, expand_expr a, expand_expr b)
  | Expr_unary (op, a) -> Expr_unary (op, expand_expr a)
  | Expr_apply (func, args) ->
      Expr_apply (expand_expr func, List.map expand_expr args)
  | Expr_call { module_; func; args } ->
      Expr_call {
        module_ = expand_expr module_;
        func = expand_expr func;
        args = List.map expand_expr args
      }
  | Expr_fun { params; body } ->
      Expr_fun { params; body = expand_expr body }
  | Expr_let { var; value; body } ->
      Expr_let { var; value = expand_expr value; body = expand_expr body }
  | Expr_letrec { bindings; body } ->
      Expr_letrec {
        bindings = List.map (fun (n, p, b) -> (n, p, expand_expr b)) bindings;
        body = expand_expr body
      }
  | Expr_case { scrutinee; clauses } ->
      Expr_case {
        scrutinee = expand_expr scrutinee;
        clauses = List.map (fun (p, g, b) ->
          (p, Option.map expand_expr g, expand_expr b)
        ) clauses
      }
  | Expr_if { cond; then_; else_ } ->
      Expr_if {
        cond = expand_expr cond;
        then_ = expand_expr then_;
        else_ = expand_expr else_
      }
  | Expr_seq (e1, e2) -> Expr_seq (expand_expr e1, expand_expr e2)
  | Expr_try { body; catch_var; catch_body } ->
      Expr_try {
        body = expand_expr body;
        catch_var;
        catch_body = expand_expr catch_body
      }
  | Expr_receive { clauses; timeout } ->
      Expr_receive {
        clauses = List.map (fun (p, g, b) ->
          (p, Option.map expand_expr g, expand_expr b)
        ) clauses;
        timeout = Option.map (fun (t, b) ->
          (expand_expr t, expand_expr b)
        ) timeout
      }
  | Expr_primop { name; args } ->
      Expr_primop { name; args = List.map expand_expr args }
  | Expr_map pairs ->
      Expr_map (List.map (fun (k, v) -> (expand_expr k, expand_expr v)) pairs)
  | Expr_map_update { map; updates } ->
      Expr_map_update {
        map = expand_expr map;
        updates = List.map (fun (k, v) -> (expand_expr k, expand_expr v)) updates
      }

(** Expand all macros in a function definition *)
let expand_fun_def (fdef : fun_def) : fun_def =
  { fdef with body = expand_expr fdef.body }

(** Expand all macros in a module definition.
    Note: Macros must be registered BEFORE calling this function.
    The calling code should:
    1. Call reset() to clear previous state
    2. Register macros via register_macro
    3. Call expand_module *)
let expand_module (mdef : module_def) : module_def =
  (* Don't reset here - macros should already be registered by the caller *)
  { mdef with functions = List.map expand_fun_def mdef.functions }
