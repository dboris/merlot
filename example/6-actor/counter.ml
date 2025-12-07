(* Counter Actor Example

   Demonstrates the Actor module for building stateful processes.
   An actor encapsulates state and handles messages in a type-safe way.
*)

open Merlot_stdlib

(* Counter handler: processes messages and returns next state
   Returns `Continue state or `Stop state *)
let counter_handler state msg =
  match msg with
  | `Inc -> `Continue (state + 1)
  | `Dec -> `Continue (state - 1)
  | `Add n -> `Continue (state + n)
  | `Reset -> `Continue 0
  | `Get caller ->
      let _ = Process.send caller state in
      `Continue state
  | `Stop -> `Stop state

(* Start a counter with initial value *)
let start_counter init =
  Actor.start init counter_handler

(* Helper to get current value *)
let get_value counter =
  let me = Process.self () in
  Actor.send counter (`Get me);
  Process.receive (fun x -> x)

(* Main demonstration *)
let main () =
  (* Create a counter starting at 0 *)
  let counter = start_counter 0 in

  (* Increment a few times *)
  Actor.send counter `Inc;
  Actor.send counter `Inc;
  Actor.send counter `Inc;

  (* Get current value (should be 3) *)
  let v1 = get_value counter in

  (* Add 10 *)
  Actor.send counter (`Add 10);
  let v2 = get_value counter in  (* should be 13 *)

  (* Decrement *)
  Actor.send counter `Dec;
  let v3 = get_value counter in  (* should be 12 *)

  (* Reset and check *)
  Actor.send counter `Reset;
  let v4 = get_value counter in  (* should be 0 *)

  (* Stop the actor *)
  Actor.send counter `Stop;

  (* Return results *)
  (v1, v2, v3, v4)
