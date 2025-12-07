(* Selective receive patterns *)

(* FFI declarations *)
external spawn : (unit -> 'a) -> int = "erlang" "spawn"
external self : unit -> int = "erlang" "self"
external send : int -> 'a -> 'a = "erlang" "!"
external receive : ('a -> 'b) -> 'b = "erlang" "receive"
external receive_timeout : int -> ('a -> 'b) -> 'b = "erlang" "receive_timeout"
external make_ref : unit -> int = "erlang" "make_ref"

(* Priority receive - handle high priority messages first *)
let rec priority_loop () =
  (* First try to match high priority messages *)
  receive_timeout 0 (function
    | `High msg ->
        (* Process high priority *)
        let _ = msg in
        priority_loop ()
    | `Low _ ->
        (* Low priority messages wait - this won't match with timeout 0 *)
        priority_loop ())

(* Reference-based selective receive for request/response *)
let call server request =
  let ref = make_ref () in
  let me = self () in
  let _ = send server (`Call (ref, me, request)) in
  (* Selectively receive only the response with matching ref *)
  receive (function
    | `Reply (r, response) when r = ref -> response)

(* Server that handles calls *)
let rec server_loop state =
  receive (function
    | `Call (ref, from, `Get) ->
        let _ = send from (`Reply (ref, state)) in
        server_loop state
    | `Call (ref, from, `Set new_state) ->
        let _ = send from (`Reply (ref, `Ok)) in
        server_loop new_state
    | `Call (ref, from, `Inc) ->
        let _ = send from (`Reply (ref, state + 1)) in
        server_loop (state + 1)
    | `Stop -> state)

let start_server initial =
  spawn (fun () -> server_loop initial)

(* Selective receive based on sender *)
let receive_from sender =
  receive (function
    | `Msg (from, content) when from = sender -> content)

(* Flush all messages of a specific type *)
let rec flush_type () =
  receive_timeout 0 (function
    | `Flush _ -> flush_type ()
    | _ -> ())

(* Receive with multiple patterns - demonstrates selective behavior *)
let rec worker () =
  receive (function
    | `Task (id, work) ->
        (* Process work *)
        let result = work + 1 in
        let _ = (id, result) in
        worker ()
    | `Priority task ->
        (* Handle priority immediately *)
        let _ = task in
        worker ()
    | `Quit -> ())

(* Accumulate messages until a specific one arrives *)
let rec collect_until_done acc =
  receive (function
    | `Item x -> collect_until_done (x :: acc)
    | `Done -> acc)

(* Demonstrates that non-matching messages stay in mailbox *)
let selective_demo () =
  let me = self () in
  (* Send messages in order: A, B, C *)
  let _ = send me (`Msg "A") in
  let _ = send me (`Msg "B") in
  let _ = send me (`Msg "C") in
  let _ = send me `Done in
  (* But receive B first (selective), then A, then C *)
  let b = receive (function | `Msg "B" -> "got B") in
  let a = receive (function | `Msg "A" -> "got A") in
  let c = receive (function | `Msg "C" -> "got C") in
  let _ = receive (function | `Done -> "done") in
  (a, b, c)
