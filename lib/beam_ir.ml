(** BEAM IR - Stable intermediate representation for BEAM targets

    This IR is designed to be:
    1. Independent of OCaml compiler version
    2. Close to Core Erlang semantics
    3. Simple and stable for long-term maintenance
*)

(** Source location information *)
type loc = {
  file: string;
  line: int;
  col: int;
}

let no_loc = { file = ""; line = 0; col = 0 }

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

(** Binary operators *)
type binop =
  | Op_add | Op_sub | Op_mul | Op_div | Op_mod
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

let pp_binop fmt = function
  | Op_add -> Format.fprintf fmt "+"
  | Op_sub -> Format.fprintf fmt "-"
  | Op_mul -> Format.fprintf fmt "*"
  | Op_div -> Format.fprintf fmt "/"
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
  | Expr_try _ | Expr_receive _ -> false  (* Effects are illegal *)
  | Expr_primop _ -> false  (* Primops are illegal *)
