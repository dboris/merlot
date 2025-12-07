(* Counter - A stateful process example

   Demonstrates:
   - Spawning processes with Process.spawn
   - Sending messages with Process.send
   - Selective receive with pattern matching
   - Polymorphic variants for message types
   - Tail-recursive process loops
*)

open Merlot_stdlib

(* A counter process that maintains state and responds to messages *)
let rec counter count =
  Process.receive (function
    | `Inc ->
        counter (count + 1)
    | `Dec ->
        counter (count - 1)
    | `Get pid ->
        let _ = Process.send pid (`Value count) in
        counter count
    | `Stop ->
        count)

(* Start a counter with initial value *)
let start_counter initial =
  Process.spawn (fun () -> counter initial)

(* Helper to get the current count *)
let get_count counter_pid =
  let me = Process.self () in
  let _ = Process.send counter_pid (`Get me) in
  Process.receive (function
    | `Value n -> n)

(* Main demonstration *)
let main () =
  (* Start a counter at 0 *)
  let c = start_counter 0 in

  (* Increment 3 times *)
  let _ = Process.send c `Inc in
  let _ = Process.send c `Inc in
  let _ = Process.send c `Inc in

  (* Decrement once *)
  let _ = Process.send c `Dec in

  (* Get the result (should be 2) *)
  let result = get_count c in

  (* Stop the counter *)
  let _ = Process.send c `Stop in

  result
