(** Supervisor - OTP-style process supervision for Merlot

    Supervisors monitor child processes and restart them when they crash.
    This enables fault-tolerant applications using the "let it crash" philosophy.

    {2 Restart Strategies}

    - [`OneForOne]: If a child dies, only that child is restarted
    - [`OneForAll]: If a child dies, all children are terminated and restarted
    - [`RestForOne]: If a child dies, children started after it are terminated and all are restarted

    {2 Restart Types}

    - [`Permanent]: Always restart (default for workers)
    - [`Temporary]: Never restart, even on crash
    - [`Transient]: Only restart if terminated abnormally

    {2 Example}

    {[
      let children = [
        Supervisor.worker "counter" (fun () -> Actor.start 0 counter_handler);
        Supervisor.worker "logger" (fun () -> Actor.start [] logger_handler);
      ]

      let main () =
        Supervisor.start_link `OneForOne children
    ]}
*)

(** Restart strategy: determines how failures affect other children. *)
type strategy = [ `OneForOne | `OneForAll | `RestForOne ]

(** When to restart a child process. *)
type restart_type = [ `Permanent | `Temporary | `Transient ]

(** Child specification: [(id, start_function, restart_type)].
    The [id] is used for logging and [which_children].
    The [start_function] should spawn and return a process pid. *)
type child_spec = string * (unit -> int) * restart_type

(** [worker id start_fn] creates a permanent worker child spec.
    The worker will always be restarted on exit. *)
external worker : string -> (unit -> int) -> child_spec
  = "merlot_supervisor" "worker"

(** [temporary_worker id start_fn] creates a temporary worker.
    Temporary workers are never restarted, even on crash. *)
external temporary_worker : string -> (unit -> int) -> child_spec
  = "merlot_supervisor" "temporary_worker"

(** [transient_worker id start_fn] creates a transient worker.
    Transient workers are restarted only on abnormal exit. *)
external transient_worker : string -> (unit -> int) -> child_spec
  = "merlot_supervisor" "transient_worker"

(** [start strategy children] starts a supervisor with the given strategy.
    Children are started in order. Returns the supervisor's pid. *)
external start : strategy -> child_spec list -> int
  = "merlot_supervisor" "start"

(** [start_link strategy children] starts a supervisor linked to the current process.
    If the supervisor exits, the current process will also exit. Returns the supervisor's pid. *)
external start_link : strategy -> child_spec list -> int
  = "merlot_supervisor" "start_link"

(** [stop supervisor_pid] terminates the supervisor and all its children. *)
external stop : int -> unit = "merlot_supervisor" "stop"

(** [which_children supervisor_pid] returns information about all children
    as a list of [(child_id, child_pid)] pairs. *)
external which_children : int -> (string * int) list
  = "merlot_supervisor" "which_children"

(** [count_children supervisor_pid] returns the number of children. *)
let count_children sup =
  List.length (which_children sup)
