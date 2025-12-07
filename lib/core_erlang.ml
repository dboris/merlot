(** Core_erlang - Pretty printer for Core Erlang output

    Generates valid Core Erlang that can be compiled by erlc.
    See: https://www.erlang.org/doc/man/core_erlang.html
*)

open Beam_ir

let _indent_str n = String.make (n * 4) ' '

(** Generic list printer *)
let rec pp_list sep pp fmt = function
  | [] -> ()
  | [x] -> pp fmt x
  | x :: xs ->
      pp fmt x;
      Format.fprintf fmt "%s" sep;
      pp_list sep pp fmt xs

(** Print a literal *)
let pp_literal fmt lit =
  match lit with
  | Lit_int n -> Format.fprintf fmt "%d" n
  | Lit_float f -> Format.fprintf fmt "%f" f
  | Lit_atom s -> Format.fprintf fmt "'%s'" s
  | Lit_string s ->
      (* Strings become binaries in Core Erlang (Elixir-style) *)
      (* Format: #{#<byte>(8,1,'integer',['unsigned'|['big']]), ...}# *)
      if String.length s = 0 then
        Format.fprintf fmt "#{  }#"
      else begin
        Format.fprintf fmt "#{";
        String.iteri (fun i c ->
          if i > 0 then Format.fprintf fmt ",@,";
          Format.fprintf fmt "#<%d>(8,1,'integer',['unsigned'|['big']])" (Char.code c)
        ) s;
        Format.fprintf fmt "}#"
      end
  | Lit_char c -> Format.fprintf fmt "%d" (Char.code c)  (* Characters are integers *)
  | Lit_nil -> Format.fprintf fmt "[]"

(** Print a pattern *)
let rec pp_pattern fmt pat =
  match pat with
  | Pat_any -> Format.fprintf fmt "_"
  | Pat_var s -> Format.fprintf fmt "%s" s
  | Pat_lit lit -> pp_literal fmt lit
  | Pat_tuple pats ->
      Format.fprintf fmt "{%a}"
        (pp_list ", " pp_pattern) pats
  | Pat_cons (hd, tl) ->
      Format.fprintf fmt "[%a|%a]" pp_pattern hd pp_pattern tl
  | Pat_record _ ->
      (* Records become tuple patterns - should be expanded earlier *)
      Format.fprintf fmt "_"
  | Pat_alias (pat, var) ->
      Format.fprintf fmt "%s = %a" var pp_pattern pat

(** Print an expression *)
let rec pp_expr fmt expr =
  match expr with
  | Expr_var s -> Format.fprintf fmt "%s" s
  | Expr_lit lit -> pp_literal fmt lit
  | Expr_tuple exprs ->
      Format.fprintf fmt "{%a}" (pp_list ", " pp_expr) exprs
  | Expr_cons (hd, tl) ->
      Format.fprintf fmt "[%a|%a]" pp_expr hd pp_expr tl
  | Expr_list exprs ->
      Format.fprintf fmt "[%a]" (pp_list ", " pp_expr) exprs
  | Expr_binary (op, a, b) ->
      pp_binop_call fmt op a b
  | Expr_unary (op, a) ->
      pp_unop_call fmt op a
  | Expr_apply (func, args) ->
      (* Only the function expression needs parentheses when it's complex,
         arguments (including funs) don't need extra parens in Core Erlang *)
      let pp_func_expr fmt e =
        match e with
        | Expr_apply _ | Expr_let _ | Expr_case _ | Expr_letrec _ ->
            Format.fprintf fmt "(%a)" pp_expr e
        | _ -> pp_expr fmt e
      in
      Format.fprintf fmt "apply %a (%a)"
        pp_func_expr func
        (pp_list ", " pp_expr) args
  | Expr_fun_ref { func_name; arity } ->
      Format.fprintf fmt "'%s'/%d" func_name arity
  | Expr_local_call { func_name; arity; args } ->
      Format.fprintf fmt "apply '%s'/%d (%a)"
        func_name arity
        (pp_list ", " pp_expr) args
  | Expr_call { module_; func; args } ->
      Format.fprintf fmt "call %a:%a (%a)"
        pp_expr module_
        pp_expr func
        (pp_list ", " pp_expr) args
  | Expr_fun { params; body } ->
      Format.fprintf fmt "fun (%a) -> %a"
        (pp_list ", " Format.pp_print_string) params
        pp_expr body
  | Expr_let { var; value; body } ->
      Format.fprintf fmt "let <%s> = %a in %a"
        var pp_expr value pp_expr body
  | Expr_letrec { bindings; body } ->
      (* Core Erlang letrec: letrec 'name'/arity = fun (...) -> ... in body *)
      Format.fprintf fmt "letrec %a in %a"
        pp_letrec_bindings bindings
        pp_expr body
  | Expr_case { scrutinee; clauses } ->
      (* Transform clauses with illegal guards before printing *)
      let transformed_clauses = transform_clauses_for_guards scrutinee clauses in
      Format.fprintf fmt "case %a of %a end"
        pp_expr scrutinee
        pp_clauses transformed_clauses
  | Expr_if { cond; then_; else_ } ->
      (* If becomes case on boolean *)
      Format.fprintf fmt "case %a of@,  <'true'> when 'true' -> %a@,  <'false'> when 'true' -> %a@,end"
        pp_expr cond
        pp_expr then_
        pp_expr else_
  | Expr_seq (e1, e2) ->
      Format.fprintf fmt "do %a@,%a"
        pp_expr e1
        pp_expr e2
  | Expr_try { body; catch_var = (cls, rsn, stk); catch_body } ->
      Format.fprintf fmt "try@,  %a@,of <Result> -> Result@,catch <%s, %s, %s> ->@,  %a"
        pp_expr body
        cls rsn stk
        pp_expr catch_body
  | Expr_receive { clauses; timeout } ->
      (* For receive, guards with function calls are also illegal.
         However, receive doesn't have an explicit scrutinee - the message is implicit.
         We use a placeholder variable for the fallback case. *)
      let msg_var = Expr_var "_ReceivedMsg" in
      let transformed_clauses = transform_clauses_for_guards msg_var clauses in
      Format.fprintf fmt "receive %a after "
        pp_clauses transformed_clauses;
      (match timeout with
       | Some (tval, tbody) ->
           Format.fprintf fmt "%a -> %a"
             pp_expr tval pp_expr tbody
       | None ->
           (* No timeout = infinite wait *)
           Format.fprintf fmt "'infinity' -> 'ok'")
  | Expr_primop { name; args } ->
      Format.fprintf fmt "primop '%s' (%a)"
        name
        (pp_list ", " pp_expr) args

(** Print a binary operator as Core Erlang call *)
and pp_binop_call fmt op a b =
  let module_name, func_name = match op with
    | Op_add -> "erlang", "+"
    | Op_sub -> "erlang", "-"
    | Op_mul -> "erlang", "*"
    | Op_div -> "erlang", "/"
    | Op_mod -> "erlang", "rem"
    | Op_eq -> "erlang", "=:="
    | Op_ne -> "erlang", "=/="
    | Op_lt -> "erlang", "<"
    | Op_le -> "erlang", "=<"
    | Op_gt -> "erlang", ">"
    | Op_ge -> "erlang", ">="
    | Op_and -> "erlang", "and"
    | Op_or -> "erlang", "or"
    | Op_band -> "erlang", "band"
    | Op_bor -> "erlang", "bor"
    | Op_bxor -> "erlang", "bxor"
    | Op_bsl -> "erlang", "bsl"
    | Op_bsr -> "erlang", "bsr"
    | Op_append -> "erlang", "++"
  in
  Format.fprintf fmt "call '%s':'%s' (%a, %a)"
    module_name func_name
    pp_expr a pp_expr b

(** Print a unary operator as Core Erlang call *)
and pp_unop_call fmt op a =
  let module_name, func_name = match op with
    | Op_neg -> "erlang", "-"
    | Op_not -> "erlang", "not"
    | Op_bnot -> "erlang", "bnot"
  in
  Format.fprintf fmt "call '%s':'%s' (%a)"
    module_name func_name
    pp_expr a

and pp_letrec_bindings fmt bindings =
  let pp_binding fmt (name, params, body) =
    let arity = List.length params in
    (* Simple letrec binding: 'name'/arity = fun (...) -> body *)
    Format.fprintf fmt "'%s'/%d = fun (%a) -> %a"
      name arity
      (pp_list ", " Format.pp_print_string) params
      pp_expr body
  in
  pp_list "@," pp_binding fmt bindings

(** Transform clauses with illegal guards.
    When a guard contains function calls (which are illegal in Core Erlang),
    we move the guard into the body:

    Original: pattern when pred(x) -> body

    Transformed: pattern when 'true' ->
                   case pred(x) of
                     'true' -> body
                     'false' -> <continue with remaining clauses>
                   end

    This preserves OCaml's semantics where a failing guard tries the next pattern.
*)
and transform_clauses_for_guards scrutinee clauses =
  (* Build a fallback expression from remaining clauses *)
  let rec build_fallback remaining =
    match remaining with
    | [] ->
        (* No more clauses - generate match_fail *)
        Expr_primop { name = "match_fail"; args = [Expr_lit (Lit_atom "case_clause")] }
    | _ ->
        (* Recursively transform remaining clauses and wrap in case *)
        Expr_case { scrutinee; clauses = transform_all remaining }

  (* Transform all clauses, building fallbacks as needed *)
  and transform_all clauses =
    match clauses with
    | [] -> []
    | (pat, guard, body) :: rest ->
        let has_illegal_guard = match guard with
          | None -> false
          | Some g -> not (is_legal_guard g)
        in
        if has_illegal_guard then
          let guard_expr = Option.get guard in
          let fallback = build_fallback rest in
          (* Transform: pattern when 'true' ->
               case guard of 'true' -> body; 'false' -> fallback end *)
          let new_body = Expr_case {
            scrutinee = guard_expr;
            clauses = [
              (Pat_lit (Lit_atom "true"), None, body);
              (Pat_lit (Lit_atom "false"), None, fallback);
            ]
          } in
          (* This clause now has no guard, body handles the guard check *)
          (pat, None, new_body) :: transform_all rest
        else
          (* Legal guard or no guard - keep as is *)
          (pat, guard, body) :: transform_all rest
  in
  transform_all clauses

and pp_clauses fmt clauses =
  let pp_clause fmt (pat, guard, body) =
    let guard_str = match guard with
      | None -> "'true'"
      | Some g -> Format.asprintf "%a" pp_expr g
    in
    Format.fprintf fmt "@,  <%a> when %s -> %a"
      pp_pattern pat
      guard_str
      pp_expr body
  in
  List.iter (pp_clause fmt) clauses

(** Print a function definition *)
let pp_function fmt (f : fun_def) =
  Format.fprintf fmt "'%s'/%d =@,    fun (%a) ->@,      %a@,"
    f.name f.arity
    (pp_list ", " Format.pp_print_string) f.params
    pp_expr f.body

(** Print a complete module *)
let pp_module fmt (m : module_def) =
  (* Module declaration *)
  Format.fprintf fmt "module '%s' [" m.name;
  List.iteri (fun i (name, arity) ->
    if i > 0 then Format.fprintf fmt ",@,                       ";
    Format.fprintf fmt "'%s'/%d" name arity
  ) m.exports;
  Format.fprintf fmt "]@,";

  (* Attributes *)
  Format.fprintf fmt "    attributes []@,@,";

  (* Function definitions *)
  List.iter (pp_function fmt) m.functions;

  (* End module *)
  Format.fprintf fmt "end@."

(** Convert module to string *)
let module_to_string m =
  let buf = Buffer.create 4096 in
  let fmt = Format.formatter_of_buffer buf in
  Format.pp_set_margin fmt 120;
  Format.pp_open_vbox fmt 0;
  pp_module fmt m;
  Format.pp_close_box fmt ();
  Format.pp_print_flush fmt ();
  Buffer.contents buf

(** Write module to file *)
let write_module filename m =
  let content = module_to_string m in
  let oc = open_out filename in
  output_string oc content;
  close_out oc
