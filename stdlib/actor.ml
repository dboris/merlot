(** Actor - OTP-style actor (gen_server) for Merlot

    Actors are stateful processes that handle messages in a type-safe way.
    This provides a higher-level abstraction over raw {!module:Process} primitives.

    {2 Handler Return Values}

    The message handler must return one of:
    - [`Continue new_state] - Continue running with updated state
    - [`Stop final_state] - Terminate the actor

    {2 Example: Counter Actor}

    {[
      let counter_handler state msg =
        match msg with
        | `Inc -> `Continue (state + 1)
        | `Dec -> `Continue (state - 1)
        | `Get from ->
            let _ = Process.send from state in
            `Continue state
        | `Stop -> `Stop state

      let start_counter () =
        Actor.start 0 counter_handler

      (* Usage *)
      let counter = start_counter ()
      Actor.send counter `Inc
      Actor.send counter `Inc
      Actor.send counter (`Get (Process.self ()))
    ]}

    {2 Comparison with Process}

    | Feature | Process | Actor |
    |---------|---------|-------|
    | State | Manual via recursion | Built-in, handler receives state |
    | Message loop | Manual [receive] calls | Automatic |
    | Shutdown | Manual [exit] | Return [`Stop state] |
*)

(** {1 Starting Actors} *)

(** [start initial_state handler] starts an actor with [initial_state].
    The [handler] receives [(state, msg)] and returns [`Continue new_state]
    or [`Stop final_state]. Returns the actor's pid. *)
external start : 'state -> ('state -> 'msg -> [> `Continue of 'state | `Stop of 'state ]) -> int
  = "merlot_actor" "start"

(** [start_link initial_state handler] starts an actor linked to the current process.
    If either exits abnormally, both will exit. Returns the actor's pid. *)
external start_link : 'state -> ('state -> 'msg -> [> `Continue of 'state | `Stop of 'state ]) -> int
  = "merlot_actor" "start_link"

(** {1 Messaging} *)

(** [send pid msg] sends [msg] to the actor (asynchronous, fire-and-forget).
    Returns immediately without waiting for the message to be processed. *)
external send : int -> 'msg -> unit = "merlot_actor" "send"

(** {1 Inspection} *)

(** [is_alive pid] returns [true] if the actor is still running. *)
external is_alive : int -> bool = "erlang" "is_process_alive"
