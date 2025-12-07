(** Message - Chat message types and formatting

    Defines the types for chat messages and provides formatting utilities.
    This module is used by both Room and User modules.
*)

open Merlot_stdlib

(** A chat message with sender name, content, and timestamp *)
type t = {
  sender: string;
  content: string;
  timestamp: int;
}

(** Create a new message with current timestamp *)
let create sender content =
  let timestamp = Erlang.now () in
  { sender; content; timestamp }

(** Format a message for display *)
let format msg =
  "[" ^ msg.sender ^ "] " ^ msg.content

