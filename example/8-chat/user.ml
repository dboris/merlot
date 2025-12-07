(** User - Chat user actor

    Each user is an actor that:
    - Has a name and tracks which room they're in
    - Receives messages from the room and displays them
    - Can send messages to the room
*)

open Merlot_stdlib

(** User state *)
type state = {
  name: string;
  room: int option;  (* Room pid if joined *)
  inbox: Message.t list;  (* Received messages *)
}

(** Messages a user can receive *)
type msg =
  | Join of int  (* Join room with pid *)
  | Leave
  | Receive of Message.t  (* Receive a message from room *)
  | Send of string  (* Send a message to room *)
  | GetInbox of int  (* Request inbox, reply to pid *)

(** Handle user messages *)
let handle state msg =
  match msg with
  | Join room_pid ->
      Io.format "~s joined a room~n" [state.name];
      `Continue { state with room = Some room_pid }

  | Leave ->
      Io.format "~s left the room~n" [state.name];
      `Continue { state with room = None }

  | Receive message ->
      (* Add to inbox and print *)
      let formatted = Message.format message in
      Io.format "~s received: ~s~n" [state.name; formatted];
      `Continue { state with inbox = message :: state.inbox }

  | Send content ->
      (match state.room with
       | Some room_pid ->
           (* Create message and send to room *)
           let message = Message.create state.name content in
           let _ = Process.send room_pid (`Broadcast message) in
           `Continue state
       | None ->
           Io.format "~s: Not in a room!~n" [state.name];
           `Continue state)

  | GetInbox reply_to ->
      let _ = Process.send reply_to state.inbox in
      `Continue state

(** Start a new user actor *)
let start name =
  let initial_state = { name; room = None; inbox = [] } in
  Actor.start initial_state handle

(** Send a message as this user *)
let send user_pid content =
  Actor.send user_pid (Send content)

(** Tell user to join a room *)
let join user_pid room_pid =
  Actor.send user_pid (Join room_pid)

(** Tell user to leave the room *)
let leave user_pid =
  Actor.send user_pid Leave

(** Deliver a message to the user *)
let deliver user_pid message =
  Actor.send user_pid (Receive message)
