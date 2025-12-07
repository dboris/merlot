(* Erlang process primitives *)

(* FFI declarations *)
external spawn : (unit -> 'a) -> int = "erlang" "spawn"
external self : unit -> int = "erlang" "self"
external send : int -> 'a -> 'a = "erlang" "!"
external receive : ('a -> 'b) -> 'b = "erlang" "receive"
external receive_timeout : int -> ('a -> 'b) -> 'b = "erlang" "receive_timeout"

(* Simple ping-pong process *)
let rec pong () =
  receive (function
    | `Ping sender ->
        let _ = send sender `Pong in
        pong ()
    | `Stop -> ())

(* Start a pong process and return its PID *)
let start_pong () =
  spawn pong

(* Send a ping and wait for pong *)
let ping pong_pid =
  let me = self () in
  let _ = send pong_pid (`Ping me) in
  receive (function
    | `Pong -> `Got_pong)

(* Receive with timeout *)
let wait_with_timeout () =
  receive_timeout 1000 (function
    | `Message x -> x)

(* Counter process - demonstrates stateful receive loop *)
let rec counter_loop count =
  receive (function
    | `Inc -> counter_loop (count + 1)
    | `Dec -> counter_loop (count - 1)
    | `Get sender ->
        let _ = send sender count in
        counter_loop count
    | `Stop -> count)

let start_counter () =
  spawn (fun () -> counter_loop 0)
