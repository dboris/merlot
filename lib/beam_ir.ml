(** BEAM IR - Stable intermediate representation for BEAM targets

    This IR is designed to be:
    1. Independent of OCaml compiler version
    2. Close to Core Erlang semantics
    3. Simple and stable for long-term maintenance

    The IR also supports macro constructs (quote/unquote/splice) which are
    expanded before code generation. See macro_expand.ml for the expansion logic.
*)

(** Source location information *)
type loc = {
  file: string;
  line: int;
  col: int;
}

let no_loc = { file = ""; line = 0; col = 0 }

(** Hygiene context for macro-generated variables.
    Variables with the same name but different contexts are distinct.
    This prevents macro-introduced variables from capturing user variables. *)
type hygiene_ctx = {
  macro_name: string;      (** Name of the macro that introduced this variable *)
  expansion_id: int;       (** Unique ID for this macro expansion *)
}

(** Variable reference with optional hygiene context *)
type var_ref = {
  var_name: string;
  var_ctx: hygiene_ctx option;  (** None = caller context (unhygienic) *)
}

(** Create a simple variable reference (caller context) *)
let var name = { var_name = name; var_ctx = None }

(** Create a hygienic variable reference *)
let hygienic_var name ctx = { var_name = name; var_ctx = Some ctx }

(** Literal values *)
type literal =
  | Lit_int of int
  | Lit_float of float
  | Lit_atom of string        (** Erlang atoms, e.g., 'ok', 'error' *)
  | Lit_string of string      (** String literals (become binaries in Erlang) *)
  | Lit_char of char
  | Lit_nil                   (** Empty list [] *)

(** Patterns for case/receive/function clauses *)
type pattern =
  | Pat_any                   (** Wildcard _ *)
  | Pat_var of string         (** Variable binding *)
  | Pat_lit of literal        (** Literal match *)
  | Pat_tuple of pattern list (** Tuple pattern {P1, P2, ...} *)
  | Pat_cons of pattern * pattern  (** List cons [H|T] *)
  | Pat_record of (string * pattern) list  (** Record pattern (future) *)
  | Pat_alias of pattern * string  (** Pattern with alias (P = Var) *)
  | Pat_inject of string      (** Unhygienic variable binding (Macro.expose_binding) *)

(** Binary operators *)
type binop =
  | Op_add | Op_sub | Op_mul | Op_div | Op_idiv | Op_mod
  | Op_eq | Op_ne | Op_lt | Op_le | Op_gt | Op_ge
  | Op_and | Op_or
  | Op_band | Op_bor | Op_bxor | Op_bsl | Op_bsr
  | Op_append  (** List append ++ *)

(** Unary operators *)
type unop =
  | Op_neg | Op_not | Op_bnot

(** Expressions *)
type expr =
  | Expr_var of string                      (** Variable reference *)
  | Expr_lit of literal                     (** Literal value *)
  | Expr_tuple of expr list                 (** Tuple {E1, E2, ...} *)
  | Expr_cons of expr * expr                (** List cons [H|T] *)
  | Expr_list of expr list                  (** List literal [E1, E2, ...] *)
  | Expr_binary of binop * expr * expr      (** Binary operation *)
  | Expr_unary of unop * expr               (** Unary operation *)
  | Expr_apply of expr * expr list          (** Higher-order function application *)
  | Expr_fun_ref of {                       (** Function reference 'name'/arity *)
      func_name: string;
      arity: int;
    }
  | Expr_local_call of {                    (** Local function call *)
      func_name: string;
      arity: int;
      args: expr list;
    }
  | Expr_call of {                          (** Inter-module call *)
      module_: expr;  (* Can be atom or variable *)
      func: expr;     (* Can be atom or variable *)
      args: expr list;
    }
  | Expr_fun of {                           (** Anonymous function *)
      params: string list;
      body: expr;
    }
  | Expr_let of {                           (** Let binding *)
      var: string;
      value: expr;
      body: expr;
    }
  | Expr_letrec of {                        (** Recursive let (for local funs) *)
      bindings: (string * string list * expr) list;  (* name, params, body *)
      body: expr;
    }
  | Expr_case of {                          (** Case expression *)
      scrutinee: expr;
      clauses: (pattern * expr option * expr) list;  (* pattern, guard, body *)
    }
  | Expr_if of {                            (** If expression (sugar for case) *)
      cond: expr;
      then_: expr;
      else_: expr;
    }
  | Expr_seq of expr * expr                 (** Sequence (do E1, E2) *)
  | Expr_map of (expr * expr) list          (** Map literal #{ K1 => V1, K2 => V2 } *)
  | Expr_map_update of {                    (** Map update M#{ K => V } *)
      map: expr;
      updates: (expr * expr) list;
    }
  | Expr_try of {                           (** Try-catch *)
      body: expr;
      catch_var: string * string * string;  (* Class, Reason, Stack *)
      catch_body: expr;
    }
  | Expr_receive of {                       (** Receive expression (future) *)
      clauses: (pattern * expr option * expr) list;
      timeout: (expr * expr) option;  (* timeout value, timeout body *)
    }
  | Expr_primop of {                        (** Primitive operation *)
      name: string;
      args: expr list;
    }
  (* Macro constructs - expanded before code generation *)
  | Expr_quote of expr                      (** Quoted expression [%quote ...] *)
  | Expr_unquote of string                  (** Unquote a value [%u name] *)
  | Expr_splice of string                   (** Splice an AST fragment [%s name] *)
  | Expr_stringify of string                (** Convert splice to string [%stringify name] *)

(** Macro definition *)
type macro_def = {
  macro_name: string;
  macro_params: string list;
  macro_body: expr;         (** The quoted template *)
  macro_loc: loc;
}

(** Function definition *)
type fun_def = {
  name: string;
  arity: int;
  params: string list;
  body: expr;
  loc: loc;
}

(** Module definition *)
type module_def = {
  name: string;
  exports: (string * int) list;  (** Exported functions with arities *)
  attributes: (string * literal list) list;  (** Module attributes *)
  functions: fun_def list;
}

(** Pretty printing helpers for debugging *)

let rec pp_literal fmt = function
  | Lit_int n -> Format.fprintf fmt "%d" n
  | Lit_float f -> Format.fprintf fmt "%f" f
  | Lit_atom s -> Format.fprintf fmt "'%s'" s
  | Lit_string s -> Format.fprintf fmt "\"%s\"" (String.escaped s)
  | Lit_char c -> Format.fprintf fmt "$%c" c
  | Lit_nil -> Format.fprintf fmt "[]"

let rec pp_pattern fmt = function
  | Pat_any -> Format.fprintf fmt "_"
  | Pat_var s -> Format.fprintf fmt "%s" s
  | Pat_lit lit -> pp_literal fmt lit
  | Pat_tuple pats ->
      Format.fprintf fmt "{%a}"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_pattern)
        pats
  | Pat_cons (hd, tl) ->
      Format.fprintf fmt "[%a|%a]" pp_pattern hd pp_pattern tl
  | Pat_record fields ->
      Format.fprintf fmt "#{%a}"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
          (fun fmt (name, pat) -> Format.fprintf fmt "%s = %a" name pp_pattern pat))
        fields
  | Pat_alias (pat, var) ->
      Format.fprintf fmt "%a = %s" pp_pattern pat var
  | Pat_inject var ->
      Format.fprintf fmt "(inject %s)" var

let pp_binop fmt = function
  | Op_add -> Format.fprintf fmt "+"
  | Op_sub -> Format.fprintf fmt "-"
  | Op_mul -> Format.fprintf fmt "*"
  | Op_div -> Format.fprintf fmt "/"
  | Op_idiv -> Format.fprintf fmt "div"
  | Op_mod -> Format.fprintf fmt "rem"
  | Op_eq -> Format.fprintf fmt "=:="
  | Op_ne -> Format.fprintf fmt "=/="
  | Op_lt -> Format.fprintf fmt "<"
  | Op_le -> Format.fprintf fmt "=<"
  | Op_gt -> Format.fprintf fmt ">"
  | Op_ge -> Format.fprintf fmt ">="
  | Op_and -> Format.fprintf fmt "and"
  | Op_or -> Format.fprintf fmt "or"
  | Op_band -> Format.fprintf fmt "band"
  | Op_bor -> Format.fprintf fmt "bor"
  | Op_bxor -> Format.fprintf fmt "bxor"
  | Op_bsl -> Format.fprintf fmt "bsl"
  | Op_bsr -> Format.fprintf fmt "bsr"
  | Op_append -> Format.fprintf fmt "++"

let pp_unop fmt = function
  | Op_neg -> Format.fprintf fmt "-"
  | Op_not -> Format.fprintf fmt "not"
  | Op_bnot -> Format.fprintf fmt "bnot"

(** Pretty-print an expression in a human-readable form (for stringify) *)
let rec pp_expr fmt = function
  | Expr_var name -> Format.fprintf fmt "%s" name
  | Expr_lit lit -> pp_literal fmt lit
  | Expr_tuple exprs ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_expr)
        exprs
  | Expr_list exprs ->
      Format.fprintf fmt "[%a]"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_expr)
        exprs
  | Expr_cons (hd, tl) ->
      Format.fprintf fmt "[%a | %a]" pp_expr hd pp_expr tl
  | Expr_binary (op, e1, e2) ->
      Format.fprintf fmt "(%a %a %a)" pp_expr e1 pp_binop op pp_expr e2
  | Expr_unary (op, e) ->
      Format.fprintf fmt "(%a %a)" pp_unop op pp_expr e
  | Expr_apply (func, args) ->
      Format.fprintf fmt "%a(%a)" pp_expr func
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_expr)
        args
  | Expr_local_call { func_name; args; _ } ->
      Format.fprintf fmt "%s(%a)" func_name
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_expr)
        args
  | Expr_call { module_; func; args } ->
      Format.fprintf fmt "%a:%a(%a)" pp_expr module_ pp_expr func
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_expr)
        args
  | Expr_fun { params; _ } ->
      Format.fprintf fmt "fun(%a) -> ..."
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
          Format.pp_print_string)
        params
  | Expr_fun_ref { func_name; arity } ->
      Format.fprintf fmt "fun %s/%d" func_name arity
  | Expr_let { var; value; body } ->
      Format.fprintf fmt "let %s = %a in %a" var pp_expr value pp_expr body
  | Expr_letrec _ -> Format.fprintf fmt "letrec ..."
  | Expr_case { scrutinee; _ } ->
      Format.fprintf fmt "case %a of ..." pp_expr scrutinee
  | Expr_if { cond; _ } ->
      Format.fprintf fmt "if %a then ... else ..." pp_expr cond
  | Expr_seq (e1, e2) ->
      Format.fprintf fmt "%a; %a" pp_expr e1 pp_expr e2
  | Expr_map pairs ->
      let pp_pair fmt (k, v) = Format.fprintf fmt "%a => %a" pp_expr k pp_expr v in
      Format.fprintf fmt "#{%a}"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_pair)
        pairs
  | Expr_map_update { map; updates } ->
      let pp_pair fmt (k, v) = Format.fprintf fmt "%a => %a" pp_expr k pp_expr v in
      Format.fprintf fmt "%a#{%a}" pp_expr map
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_pair)
        updates
  | Expr_try _ -> Format.fprintf fmt "try ... catch ..."
  | Expr_receive _ -> Format.fprintf fmt "receive ..."
  | Expr_primop { name; args } ->
      Format.fprintf fmt "primop %s(%a)" name
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_expr)
        args
  | Expr_quote inner -> Format.fprintf fmt "quote(%a)" pp_expr inner
  | Expr_unquote name -> Format.fprintf fmt "unquote(%s)" name
  | Expr_splice name -> Format.fprintf fmt "splice(%s)" name
  | Expr_stringify name -> Format.fprintf fmt "stringify(%s)" name

(** Check if an expression is legal as a Core Erlang guard.
    Guards can only contain:
    - Variables and literals
    - Tuple/list construction
    - Binary and unary operators (which map to erlang BIFs)
    - Calls to erlang module BIFs (comparisons, type checks, etc.)

    Guards CANNOT contain:
    - Expr_apply (higher-order function application)
    - Expr_local_call (local function calls)
    - Expr_call to non-erlang modules
    - Let bindings, letrec, case, if, try, receive, etc.
*)
let rec is_legal_guard expr =
  match expr with
  | Expr_var _ | Expr_lit _ -> true
  | Expr_tuple exprs | Expr_list exprs -> List.for_all is_legal_guard exprs
  | Expr_cons (hd, tl) -> is_legal_guard hd && is_legal_guard tl
  | Expr_binary (_, e1, e2) -> is_legal_guard e1 && is_legal_guard e2
  | Expr_unary (_, e) -> is_legal_guard e
  | Expr_call { module_ = Expr_lit (Lit_atom "erlang"); args; _ } ->
      (* Erlang BIF calls are legal in guards *)
      List.for_all is_legal_guard args
  | Expr_apply _ -> false  (* Higher-order function application is illegal *)
  | Expr_local_call _ -> false  (* Local function calls are illegal *)
  | Expr_call _ -> false  (* Non-erlang calls are illegal *)
  | Expr_fun _ | Expr_fun_ref _ -> false  (* Function values are illegal *)
  | Expr_let _ | Expr_letrec _ -> false  (* Bindings are illegal *)
  | Expr_case _ | Expr_if _ -> false  (* Control flow is illegal *)
  | Expr_seq _ -> false  (* Sequences are illegal *)
  | Expr_map pairs ->
      (* Map literals are legal in guards if all keys/values are legal *)
      List.for_all (fun (k, v) -> is_legal_guard k && is_legal_guard v) pairs
  | Expr_map_update { map; updates } ->
      (* Map updates are legal in guards *)
      is_legal_guard map &&
      List.for_all (fun (k, v) -> is_legal_guard k && is_legal_guard v) updates
  | Expr_try _ | Expr_receive _ -> false  (* Effects are illegal *)
  | Expr_primop _ -> false  (* Primops are illegal *)
  (* Macro constructs should be expanded before guard checking *)
  | Expr_quote _ | Expr_unquote _ | Expr_splice _ | Expr_stringify _ -> false
