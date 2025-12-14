(* Example: Demonstrating [@expose] attribute for unhygienic macro bindings

   This example shows the difference between hygienic (default) and
   unhygienic ([@expose]) variable bindings in macros.

   The [@expose] attribute prevents a macro-introduced variable from being
   renamed, making it visible with its original name in the expanded code.
   This is useful when you want a macro to intentionally introduce a
   binding that can be used later.

   Compare with Elixir's var!(name) which serves the same purpose.
*)

open Merlot_stdlib

(* Example 1: Timing macro that exposes elapsed time

   This macro times an expression and makes 'elapsed' available
   after the expression runs. Without [@expose], 'elapsed' would be
   renamed to 'elapsed__timed_0' for hygiene.
*)
external monotonic_time : unit -> int = "erlang" "monotonic_time"

let timed = Macro.defmacro (fun body ->
  Macro.quote (
    let start = monotonic_time () in
    let result = Macro.unquote body in
    (* This binding is exposed - NOT renamed *)
    let elapsed [@expose] = monotonic_time () - start in
    let _ = Io.format "Elapsed: ~p~n" [elapsed] in
    result
  )
)

(* Example 2: Debug macro that exposes the value

   The debug macro exposes 'dbg' so you can see what the value was
   in the expanded code output.
*)
let debug = Macro.defmacro (fun expr ->
  Macro.quote (
    let dbg [@expose] = Macro.unquote expr in
    let _ = Io.format "DEBUG ~s = ~p~n" [Macro.to_string expr; dbg] in
    dbg
  )
)

(* Example 3: Compare hygienic vs non-hygienic

   This macro shows both - 'hidden' is hygienic (will be renamed),
   while 'visible' is exposed (keeps its name).
*)
let compare = Macro.defmacro (fun expr ->
  Macro.quote (
    (* Hygienic: renamed to hidden__compare_N *)
    let hidden = Macro.unquote expr in
    (* Exposed: stays as 'visible' *)
    let visible [@expose] = hidden * 2 in
    let _ = Io.format "visible = ~p~n" [visible] in
    visible
  )
)

(* Demo the macros *)
let demo () =
  Io.put_chars "=== Timing macro ===\n";
  let result = timed (
    let _ = Lists.seq 1 100 in
    42
  ) in
  Io.format "Result: ~p~n" [result];

  Io.put_chars "\n=== Debug macro ===\n";
  let x = debug (10 + 20) in
  Io.format "Got: ~p~n" [x];

  Io.put_chars "\n=== Compare hygienic vs exposed ===\n";
  let v = compare 5 in
  Io.format "Final: ~p~n" [v]

(* Main entry point *)
let main () =
  demo ()
