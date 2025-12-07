(** Timer - Erlang timer module bindings *)

type tref = int

(** Send a message to a process after a delay (milliseconds) *)
external send_after : int -> Process.pid -> 'a -> tref = "erlang" "send_after"

(** Cancel a timer *)
external cancel : tref -> unit = "erlang" "cancel_timer"

(** Sleep for a number of milliseconds *)
external sleep : int -> unit = "timer" "sleep"

(** Execute a function after a delay *)
let apply_after ms f =
  let me = Process.self () in
  let _ = Process.spawn (fun () ->
    sleep ms;
    let _ = Process.send me (`Timer_apply f) in ()
  ) in ()

(** Send a message repeatedly at an interval *)
external send_interval : int -> Process.pid -> 'a -> tref = "timer" "send_interval"
