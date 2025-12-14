(** Unit tests for macro expansion *)

open Merlot.Beam_ir
open Merlot.Macro_expand

(** String prefix check helper *)
let string_starts_with ~prefix s =
  let len_prefix = String.length prefix in
  let len_s = String.length s in
  len_s >= len_prefix && String.sub s 0 len_prefix = prefix

(** Helper to create a simple "unless" macro for testing *)
let make_unless_macro () =
  (* unless condition body = if not condition then body else () *)
  {
    macro_name = "unless";
    macro_params = ["condition"; "body"];
    macro_body = Expr_quote (
      Expr_if {
        cond = Expr_unary (Op_not, Expr_splice "condition");
        then_ = Expr_splice "body";
        else_ = Expr_lit (Lit_atom "ok");
      }
    );
    macro_loc = no_loc;
  }

(** Helper to create a "debug" macro for testing *)
let make_debug_macro () =
  (* debug expr = let v = expr in Io.format(...); v *)
  {
    macro_name = "debug";
    macro_params = ["expr"];
    macro_body = Expr_quote (
      Expr_let {
        var = "Value";
        value = Expr_splice "expr";
        body = Expr_seq (
          Expr_call {
            module_ = Expr_lit (Lit_atom "io");
            func = Expr_lit (Lit_atom "format");
            args = [
              Expr_lit (Lit_string "DEBUG: ~p~n");
              Expr_list [Expr_var "Value"]
            ]
          },
          Expr_var "Value"
        )
      }
    );
    macro_loc = no_loc;
  }

(** Test basic macro expansion *)
let test_unless_expansion () =
  reset ();
  register_macro (make_unless_macro ());

  (* unless (x = 0) (100 / x) *)
  let input = Expr_local_call {
    func_name = "unless";
    arity = 2;
    args = [
      Expr_binary (Op_eq, Expr_var "X", Expr_lit (Lit_int 0));
      Expr_binary (Op_div, Expr_lit (Lit_int 100), Expr_var "X");
    ]
  } in

  let result = expand_expr input in

  (* Should expand to: if not (x = 0) then (100 / x) else ok *)
  match result with
  | Expr_if { cond; then_; else_ } ->
      (* Check condition is negated *)
      (match cond with
       | Expr_unary (Op_not, Expr_binary (Op_eq, Expr_var _, Expr_lit (Lit_int 0))) ->
           ()
       | _ -> Alcotest.fail "Expected negated condition");
      (* Check then branch is the body *)
      (match then_ with
       | Expr_binary (Op_div, Expr_lit (Lit_int 100), Expr_var _) -> ()
       | _ -> Alcotest.fail "Expected division in then branch");
      (* Check else branch is ok atom *)
      (match else_ with
       | Expr_lit (Lit_atom "ok") -> ()
       | _ -> Alcotest.fail "Expected 'ok' atom in else branch")
  | _ -> Alcotest.fail "Expected Expr_if after expansion"

(** Test hygiene - macro-introduced variables don't capture user variables *)
let test_hygiene () =
  reset ();
  register_macro (make_debug_macro ());

  (* User code: let Value = 42 in debug (Value + 1) *)
  let input = Expr_let {
    var = "Value";  (* User's variable *)
    value = Expr_lit (Lit_int 42);
    body = Expr_local_call {
      func_name = "debug";
      arity = 1;
      args = [Expr_binary (Op_add, Expr_var "Value", Expr_lit (Lit_int 1))]
    }
  } in

  let result = expand_expr input in

  (* The macro's "Value" variable should be renamed to avoid capture *)
  match result with
  | Expr_let { var = outer_var; body = Expr_let { var = inner_var; _ }; _ } ->
      (* outer_var should be "Value" (user's) *)
      Alcotest.(check string) "Outer var is user's" "Value" outer_var;
      (* inner_var should be renamed (macro's) *)
      if inner_var = "Value" then
        Alcotest.fail "Macro variable should be renamed for hygiene"
      else if not (string_starts_with ~prefix:"Value__debug_" inner_var) then
        Alcotest.fail ("Expected hygienic name, got: " ^ inner_var)
  | _ -> Alcotest.fail "Expected nested let after expansion"

(** Test that non-macro calls are left unchanged *)
let test_non_macro_unchanged () =
  reset ();
  (* Don't register any macros *)

  let input = Expr_local_call {
    func_name = "some_function";
    arity = 2;
    args = [Expr_lit (Lit_int 1); Expr_lit (Lit_int 2)]
  } in

  let result = expand_expr input in

  match result with
  | Expr_local_call { func_name = "some_function"; args; _ } ->
      Alcotest.(check int) "Args count" 2 (List.length args)
  | _ -> Alcotest.fail "Non-macro call should be unchanged"

(** Test nested macro expansion *)
let test_nested_expansion () =
  reset ();
  register_macro (make_unless_macro ());

  (* unless (unless false true) (x + 1) *)
  let input = Expr_local_call {
    func_name = "unless";
    arity = 2;
    args = [
      Expr_local_call {
        func_name = "unless";
        arity = 2;
        args = [
          Expr_lit (Lit_atom "false");
          Expr_lit (Lit_atom "true");
        ]
      };
      Expr_binary (Op_add, Expr_var "X", Expr_lit (Lit_int 1));
    ]
  } in

  let result = expand_expr input in

  (* Should expand both levels *)
  match result with
  | Expr_if { cond = Expr_unary (Op_not, Expr_if _); _ } ->
      () (* Nested expansion worked *)
  | _ -> Alcotest.fail "Expected nested if after expansion"

let () =
  Alcotest.run "Macro Expansion" [
    "basic", [
      Alcotest.test_case "unless expansion" `Quick test_unless_expansion;
      Alcotest.test_case "non-macro unchanged" `Quick test_non_macro_unchanged;
      Alcotest.test_case "nested expansion" `Quick test_nested_expansion;
    ];
    "hygiene", [
      Alcotest.test_case "variable hygiene" `Quick test_hygiene;
    ];
  ]
