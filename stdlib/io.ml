(** IO - Erlang IO operations *)

(** Print a string to stdout *)
external put_chars : string -> unit = "io" "put_chars"

(** Format and print (simplified - format string is passed through) *)
external format : string -> 'a list -> unit = "io" "format"

(** Read a line from stdin *)
external get_line : string -> string = "io" "get_line"

(** Formatted write to string *)
external format_to_string : string -> 'a list -> string = "io_lib" "format"

(** Print helpers *)
let print s = put_chars s
let println s = put_chars (s ^ "\n")
let print_int n = format "~p" [n]
let print_term t = format "~p~n" [t]
