(** Erlang - Low-level Erlang BIF bindings *)

(** {1 Type Checks} *)

external is_atom : 'a -> bool = "erlang" "is_atom"
external is_binary : 'a -> bool = "erlang" "is_binary"
external is_boolean : 'a -> bool = "erlang" "is_boolean"
external is_float : 'a -> bool = "erlang" "is_float"
external is_function : 'a -> bool = "erlang" "is_function"
external is_integer : 'a -> bool = "erlang" "is_integer"
external is_list : 'a -> bool = "erlang" "is_list"
external is_number : 'a -> bool = "erlang" "is_number"
external is_pid : 'a -> bool = "erlang" "is_pid"
external is_port : 'a -> bool = "erlang" "is_port"
external is_reference : 'a -> bool = "erlang" "is_reference"
external is_tuple : 'a -> bool = "erlang" "is_tuple"
external is_map : 'a -> bool = "erlang" "is_map"

(** {1 List Operations} *)

external hd : 'a list -> 'a = "erlang" "hd"
external tl : 'a list -> 'a list = "erlang" "tl"
external length : 'a list -> int = "erlang" "length"
external append : 'a list -> 'a list -> 'a list = "erlang" "++"

(** {1 Tuple Operations} *)

external tuple_size : 'a -> int = "erlang" "tuple_size"
external element : int -> 'a -> 'b = "erlang" "element"
external setelement : int -> 'a -> 'b -> 'a = "erlang" "setelement"

(** {1 Arithmetic} *)

external abs : int -> int = "erlang" "abs"
external max : 'a -> 'a -> 'a = "erlang" "max"
external min : 'a -> 'a -> 'a = "erlang" "min"

(** {1 Conversions} *)

external atom_to_list : 'a -> char list = "erlang" "atom_to_list"
external list_to_atom : char list -> 'a = "erlang" "list_to_atom"
external integer_to_list : int -> char list = "erlang" "integer_to_list"
external list_to_integer : char list -> int = "erlang" "list_to_integer"
external float_to_list : float -> char list = "erlang" "float_to_list"
external list_to_float : char list -> float = "erlang" "list_to_float"
external list_to_tuple : 'a list -> 'b = "erlang" "list_to_tuple"
external tuple_to_list : 'a -> 'b list = "erlang" "tuple_to_list"

(** {1 Binary/String Operations} *)

external list_to_binary : char list -> string = "erlang" "list_to_binary"
external binary_to_list : string -> char list = "erlang" "binary_to_list"
external iolist_to_binary : 'a -> string = "erlang" "iolist_to_binary"
external byte_size : string -> int = "erlang" "byte_size"
external bit_size : string -> int = "erlang" "bit_size"

(** {1 System} *)

external halt : unit -> 'a = "erlang" "halt"
external halt_code : int -> 'a = "erlang" "halt"
external error : 'a -> 'b = "erlang" "error"
external throw : 'a -> 'b = "erlang" "throw"
external now : unit -> int = "erlang" "system_time"
external monotonic_time : unit -> int = "erlang" "monotonic_time"
external unique_integer : unit -> int = "erlang" "unique_integer"

(** {1 Term Comparison} *)

external term_to_binary : 'a -> string = "erlang" "term_to_binary"
external binary_to_term : string -> 'a = "erlang" "binary_to_term"
external phash2 : 'a -> int = "erlang" "phash2"
external phash2_range : 'a -> int -> int = "erlang" "phash2"

(** {1 Process Dictionary} *)

external get : 'a -> 'b = "erlang" "get"
external put : 'a -> 'b -> 'c = "erlang" "put"
external erase : 'a -> 'b = "erlang" "erase"
external get_keys : unit -> 'a list = "erlang" "get_keys"
