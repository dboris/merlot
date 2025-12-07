(** Process - Erlang process primitives for Merlot

    Low-level process operations for spawning, messaging, and monitoring.
    For a higher-level stateful actor abstraction, see {!module:Actor}.

    {2 Basic Usage}

    {[
      let worker = Process.spawn (fun () ->
        Process.receive (function
          | `Ping sender -> Process.send sender `Pong
          | `Stop -> ()))

      let _ = Process.send worker (`Ping (Process.self ()))
      Process.receive (function `Pong -> "got pong")
    ]}

    {2 Linking and Monitoring}

    - {b Links} are bidirectional: if either process exits abnormally, both die
    - {b Monitors} are unidirectional: you receive a message when the monitored process exits
*)

(** Process identifier. Internally an Erlang pid. *)
type pid = int

(** Unique reference, used for correlating requests with responses. *)
type ref = int

(** {1 Spawning} *)

(** [spawn f] creates a new process running [f ()]. Returns the new process's pid. *)
external spawn : (unit -> 'a) -> pid = "erlang" "spawn"

(** {1 Process Identity} *)

(** [self ()] returns the pid of the calling process. *)
external self : unit -> pid = "erlang" "self"

(** [is_alive pid] returns [true] if the process exists and hasn't exited. *)
external is_alive : pid -> bool = "erlang" "is_process_alive"

(** {1 Messaging} *)

(** [send dest msg] sends [msg] to process [dest]. Returns [msg] (Erlang convention). *)
external send : pid -> 'a -> 'a = "erlang" "!"

(** [send_ dest msg] sends [msg] to [dest] and returns unit. *)
let send_ pid msg =
  let _ = send pid msg in ()

(** [receive handler] blocks until a message matches [handler].
    The [handler] is a pattern-matching function that extracts a result from messages. *)
external receive : ('a -> 'b) -> 'b = "erlang" "receive"

(** [receive_timeout ms handler] waits up to [ms] milliseconds for a matching message.
    Raises on timeout. *)
external receive_timeout : int -> ('a -> 'b) -> 'b = "erlang" "receive_timeout"

(** {1 References} *)

(** [make_ref ()] creates a globally unique reference for correlating request/response pairs. *)
external make_ref : unit -> ref = "erlang" "make_ref"

(** {1 Process Lifecycle} *)

(** [exit reason] terminates the current process with [reason].
    Use ["normal"] for a clean exit. *)
external exit : 'a -> 'b = "erlang" "exit"

(** {1 Linking} *)

(** [link pid] creates a bidirectional link to [pid].
    If either process exits abnormally, both will exit with the same reason. *)
external link : pid -> unit = "erlang" "link"

(** [unlink pid] removes the link to [pid]. *)
external unlink : pid -> unit = "erlang" "unlink"

(** {1 Monitoring} *)

(** [monitor pid] monitors [pid] (unidirectional).
    When [pid] exits, you'll receive a [DOWN] message. Returns a monitor reference. *)
external monitor : pid -> ref = "erlang" "monitor"

(** [demonitor ref] removes the monitor identified by [ref]. *)
external demonitor : ref -> unit = "erlang" "demonitor"

(** {1 Process Registry} *)

(** [register name pid] associates [name] with [pid] globally.
    The [name] becomes an Erlang atom. *)
external register : string -> pid -> unit = "erlang" "register"

(** [whereis name] returns the pid registered under [name], or raises if not found. *)
external whereis : string -> pid = "erlang" "whereis"

(** {1 Spawn Variants} *)

(** [spawn_link f] spawns a process running [f ()] and links it to the current process.
    If either exits abnormally, both will exit. *)
let spawn_link f =
  let pid = spawn f in
  link pid;
  pid

(** [spawn_monitor f] spawns a process running [f ()] and monitors it.
    Returns [(pid, monitor_ref)]. *)
let spawn_monitor f =
  let pid = spawn f in
  let ref = monitor pid in
  (pid, ref)

(** {1 Request/Response Patterns} *)

(** [call pid request] makes a synchronous call to [pid] and waits for a response.
    Sends [`Call (ref, sender_pid, request)] and expects [`Reply (ref, response)]. *)
let call pid request =
  let r = make_ref () in
  let me = self () in
  let _ = send pid (`Call (r, me, request)) in
  receive (function
    | `Reply (r', response) when r' = r -> response)

(** [call_timeout ms pid request] is like [call] but times out after [ms] milliseconds.
    Returns [`Ok response] on success. *)
let call_timeout timeout pid request =
  let r = make_ref () in
  let me = self () in
  let _ = send pid (`Call (r, me, request)) in
  receive_timeout timeout (function
    | `Reply (r', response) when r' = r -> `Ok response)

(** [cast pid msg] sends an asynchronous message (no response expected).
    Wraps [msg] in [`Cast msg] for gen_server-style protocols. *)
let cast pid msg =
  let _ = send pid (`Cast msg) in ()
