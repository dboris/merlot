(** Chat Server - Multi-module chat room demonstration

    This example demonstrates:
    - Multiple modules working together (Message, User, Room)
    - Cross-module function calls
    - Actor-based architecture with message passing
    - State management across multiple processes

    Architecture:
    - Message: Pure data module for message types
    - User: Actor representing a chat user
    - Room: Actor managing a chat room with members and history
    - Chat_server: Main entry point that orchestrates the demo
*)

open Merlot_stdlib

(** Run the chat demo *)
let run () =
  Io.format "=== Chat Room Demo ===~n~n" [];

  (* Create a chat room *)
  Io.format "Creating room 'general'...~n" [];
  let room = Room.start "general" in

  (* Create some users *)
  Io.format "Creating users...~n" [];
  let alice = User.start "Alice" in
  let bob = User.start "Bob" in
  let charlie = User.start "Charlie" in

  (* Users join the room *)
  Io.format "~nUsers joining the room...~n" [];
  Room.join room alice;
  Room.join room bob;
  Room.join room charlie;

  (* Small delay to let joins process *)
  Timer.sleep 100;

  (* Users send messages *)
  Io.format "~nSending messages...~n" [];
  User.send alice "Hello everyone!";
  Timer.sleep 50;

  User.send bob "Hi Alice! Welcome to the chat.";
  Timer.sleep 50;

  User.send charlie "Hey folks, what's up?";
  Timer.sleep 50;

  User.send alice "Just testing out this new chat system!";
  Timer.sleep 50;

  User.send bob "It seems to work great!";
  Timer.sleep 100;

  (* Check room status *)
  Io.format "~n=== Room Status ===~n" [];
  let member_count = Room.get_member_count room in
  Io.format "Members in room: ~p~n" [member_count];

  (* Get message history *)
  let history = Room.get_history room in
  let history_len = Lists.length history in
  Io.format "Messages in history: ~p~n" [history_len];

  (* Print history (reversed to show oldest first) *)
  Io.format "~n=== Message History ===~n" [];
  let reversed = Lists.reverse history in
  Lists.iter (fun msg ->
    let formatted = Message.format msg in
    Io.format "  ~s~n" [formatted]
  ) reversed;

  (* Charlie leaves *)
  Io.format "~nCharlie leaving...~n" [];
  Room.leave room charlie;
  Timer.sleep 50;

  (* More messages after charlie left *)
  User.send alice "Bye Charlie!";
  Timer.sleep 50;

  User.send bob "See you later!";
  Timer.sleep 100;

  (* Final status *)
  Io.format "~n=== Final Status ===~n" [];
  let final_count = Room.get_member_count room in
  Io.format "Members remaining: ~p~n" [final_count];

  let final_history = Room.get_history room in
  Io.format "Total messages: ~p~n" [Lists.length final_history];

  Io.format "~n=== Demo Complete ===~n" []

(** Main entry point *)
let main () = run ()
