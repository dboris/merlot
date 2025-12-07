(** Keyword - Elixir Keyword list bindings for Merlot

    Keyword lists are ordered lists of [(atom, value)] tuples, commonly used
    for options and configuration in Elixir/Erlang. Unlike maps, keyword lists:

    - Preserve insertion order
    - Allow duplicate keys
    - Keys must be atoms (strings in Merlot become atoms)

    Requires Elixir to be installed alongside Erlang/OTP.

    {2 Argument Order}

    Arguments follow OCaml convention (key first, data last) for pipe compatibility:
    {[
      Keyword.empty ()
      |> Keyword.put "name" "Alice"
      |> Keyword.put "timeout" 5000
      |> Keyword.delete "temp"
    ]}

    {2 Example: Function Options}

    {[
      (* Common pattern: optional arguments as keyword list *)
      let fetch url opts =
        let timeout = Keyword.get_default "timeout" 30000 opts in
        let retry = Keyword.get_default "retry" true opts in
        (* ... *)

      (* Usage *)
      let result = fetch "http://example.com" [("timeout", 5000); ("retry", false)]
    ]}

    {2 Differences from Map}

    | Feature | Keyword | Map |
    |---------|---------|-----|
    | Key type | Atoms only | Any type |
    | Duplicates | Allowed | Not allowed |
    | Order | Preserved | Not guaranteed |
    | Performance | O(n) lookup | O(log n) lookup |
*)

(** Keyword list type - a list of [(key, value)] pairs where keys are strings
    (which become atoms in Erlang). *)
type 'v t = (string * 'v) list

(** {1 Creation} *)

(** [empty ()] creates an empty keyword list. *)
let empty () : 'v t = []

(** {1 Inspection} *)

(** [keys kw] returns all keys in order. May contain duplicates. *)
external keys : 'v t -> string list = "Elixir.Keyword" "keys"

(** [values kw] returns all values in order. *)
external values : 'v t -> 'v list = "Elixir.Keyword" "values"

(** [is_keyword value] returns [true] if [value] is a valid keyword list. *)
external is_keyword : 'a -> bool = "Elixir.Keyword" "keyword?"

(** {1 Lookup} *)

(** [get key kw] returns the first value for [key]. Raises if not found. *)
external get : string -> 'v t -> 'v = "Elixir.Keyword" "fetch!"

(** [get_values key kw] returns all values for [key] (keyword lists allow duplicates).
    Returns an empty list if [key] is not found. *)
external get_values : string -> 'v t -> 'v list = "Elixir.Keyword" "get_values"

(** [get_default key default kw] returns the first value for [key], or [default] if not found. *)
external get_default : string -> 'v -> 'v t -> 'v = "Elixir.Keyword" "get"

(** [has_key key kw] returns [true] if [key] is in the list. *)
external has_key : string -> 'v t -> bool = "Elixir.Keyword" "has_key?"

(** {1 Modification} *)

(** [put key value kw] sets [key] to [value], removing all existing entries for the key. *)
external put : string -> 'v -> 'v t -> 'v t = "Elixir.Keyword" "put"

(** [put_new key value kw] sets [key] to [value] only if [key] doesn't exist. *)
external put_new : string -> 'v -> 'v t -> 'v t = "Elixir.Keyword" "put_new"

(** [delete key kw] removes all entries for [key]. *)
external delete : string -> 'v t -> 'v t = "Elixir.Keyword" "delete"

(** [delete_first key kw] removes only the first entry for [key]. *)
external delete_first : string -> 'v t -> 'v t = "Elixir.Keyword" "delete_first"

(** [pop key kw] removes [key] and returns [(first_value, list_without_key)].
    Raises if [key] is not found. *)
external pop : string -> 'v t -> 'v * 'v t = "Elixir.Keyword" "pop!"

(** [pop_first key kw] removes only the first entry for [key] and returns [(value, updated_list)].
    Raises if [key] is not found. *)
external pop_first : string -> 'v t -> 'v * 'v t = "Elixir.Keyword" "pop_first"

(** {1 Bulk Operations} *)

(** [take keys kw] keeps only entries with keys in [keys]. *)
external take : string list -> 'v t -> 'v t = "Elixir.Keyword" "take"

(** [drop keys kw] removes all entries with keys in [keys]. *)
external drop : string list -> 'v t -> 'v t = "Elixir.Keyword" "drop"

(** [split keys kw] returns [(entries_with_keys, entries_without_keys)]. *)
external split : string list -> 'v t -> 'v t * 'v t = "Elixir.Keyword" "split"

(** {1 Combining} *)

(** [merge kw1 kw2] combines both lists. Values from [kw2] win on key conflicts. *)
external merge : 'v t -> 'v t -> 'v t = "Elixir.Keyword" "merge"
