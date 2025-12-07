(** Room - Chat room actor

    A room manages:
    - A list of member user pids
    - Message history
    - Broadcasting messages to all members
*)

open Merlot_stdlib

(** Room state *)
type state = {
  name: string;
  members: int list;  (* User pids *)
  history: Message.t list;  (* Message history, newest first *)
  max_history: int;  (* Maximum messages to keep *)
}

(** Messages a room can receive *)
type msg =
  [ `Join of int  (* User pid joining *)
  | `Leave of int  (* User pid leaving *)
  | `Broadcast of Message.t  (* Broadcast message to all members *)
  | `GetHistory of int  (* Request history, reply to pid *)
  | `GetMembers of int  (* Request member count, reply to pid *)
  ]

(** Broadcast a message to all members except sender *)
let broadcast_to_members members message sender_name =
  Lists.iter (fun member_pid ->
    User.deliver member_pid message
  ) members

(** Trim history to max length *)
let trim_history history max_len =
  let rec take n lst =
    if n <= 0 then []
    else match lst with
      | [] -> []
      | x :: xs -> x :: take (n - 1) xs
  in
  take max_len history

(** Handle room messages *)
let handle state msg =
  match msg with
  | `Join user_pid ->
      Io.format "Room ~s: user joined~n" [state.name];
      (* Notify user they joined *)
      let _ = Process.send user_pid (User.Join (Process.self ())) in
      `Continue { state with members = user_pid :: state.members }

  | `Leave user_pid ->
      let new_members = Lists.filter (fun pid -> pid <> user_pid) state.members in
      Io.format "Room ~s: user left~n" [state.name];
      (* Notify user they left *)
      let _ = Process.send user_pid User.Leave in
      `Continue { state with members = new_members }

  | `Broadcast message ->
      Io.format "Room ~s: broadcasting from ~s~n" [state.name; message.Message.sender];
      (* Send to all members *)
      broadcast_to_members state.members message message.Message.sender;
      (* Add to history *)
      let new_history = trim_history (message :: state.history) state.max_history in
      `Continue { state with history = new_history }

  | `GetHistory reply_to ->
      let _ = Process.send reply_to state.history in
      `Continue state

  | `GetMembers reply_to ->
      let _ = Process.send reply_to (Lists.length state.members) in
      `Continue state

(** Start a new room actor *)
let start name =
  let initial_state = {
    name;
    members = [];
    history = [];
    max_history = 100;
  } in
  Actor.start initial_state handle

(** Add a user to the room *)
let join room_pid user_pid =
  Actor.send room_pid (`Join user_pid)

(** Remove a user from the room *)
let leave room_pid user_pid =
  Actor.send room_pid (`Leave user_pid)

(** Broadcast a message to the room *)
let broadcast room_pid message =
  Actor.send room_pid (`Broadcast message)

(** Get message history (synchronous) *)
let get_history room_pid =
  let self = Process.self () in
  Actor.send room_pid (`GetHistory self);
  Process.receive (fun history -> history)

(** Get member count (synchronous) *)
let get_member_count room_pid =
  let self = Process.self () in
  Actor.send room_pid (`GetMembers self);
  Process.receive (fun count -> count)
