(** Map - Elixir Map module bindings for Merlot

    Key-value map operations with efficient lookup, insertion, and deletion.
    Maps are implemented as Erlang maps in the BEAM, providing O(log n) access.
    Requires Elixir to be installed alongside Erlang/OTP.

    {2 Argument Order}

    Arguments follow OCaml convention (key first, map last) for pipe compatibility:
    {[
      Map.empty ()
      |> Map.put "name" "Alice"
      |> Map.put "age" 30
      |> Map.delete "temp"
    ]}

    The compiler automatically reorders arguments when calling [Elixir.Map].

    {2 Example}

    {[
      let users = Map.from_list [("alice", 1); ("bob", 2)]
      let alice_id = Map.get "alice" users        (* 1 *)
      let updated = Map.put "charlie" 3 users     (* adds charlie *)
      let keys = Map.keys updated                 (* ["alice"; "bob"; "charlie"] *)
    ]}
*)

(** Map type - opaque, backed by Erlang maps *)
type ('k, 'v) t

(** {1 Creation} *)

(** [empty ()] creates an empty map. *)
external empty : unit -> ('k, 'v) t = "Elixir.Map" "new"

(** [from_list pairs] creates a map from a list of [(key, value)] tuples.
    If duplicate keys exist, later entries override earlier ones. *)
external from_list : ('k * 'v) list -> ('k, 'v) t = "Elixir.Map" "new"

(** {1 Inspection} *)

(** [to_list map] converts the map to a list of [(key, value)] tuples.
    Order of entries is not guaranteed. *)
external to_list : ('k, 'v) t -> ('k * 'v) list = "Elixir.Map" "to_list"

(** [keys map] returns all keys. Order is not guaranteed. *)
external keys : ('k, 'v) t -> 'k list = "Elixir.Map" "keys"

(** [values map] returns all values. Order is not guaranteed. *)
external values : ('k, 'v) t -> 'v list = "Elixir.Map" "values"

(** [size map] returns the number of key-value pairs. *)
external size : ('k, 'v) t -> int = "Elixir.Kernel" "map_size"

(** [is_empty map] returns [true] if the map has no entries. *)
external is_empty : ('k, 'v) t -> bool = "Elixir.Enum" "empty?"

(** {1 Lookup} *)

(** [get key map] returns the value for [key]. Raises if not found. *)
external get : 'k -> ('k, 'v) t -> 'v = "Elixir.Map" "fetch!"

(** [get_default key default map] returns the value for [key], or [default] if not found. *)
external get_default : 'k -> 'v -> ('k, 'v) t -> 'v = "Elixir.Map" "get"

(** [has_key key map] returns [true] if [key] is in the map. *)
external has_key : 'k -> ('k, 'v) t -> bool = "Elixir.Map" "has_key?"

(** {1 Modification} *)

(** [put key value map] sets [key] to [value], adding or updating the entry. *)
external put : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t = "Elixir.Map" "put"

(** [put_new key value map] sets [key] to [value] only if [key] doesn't exist. *)
external put_new : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t = "Elixir.Map" "put_new"

(** [update key f map] transforms the value at [key] using [f]. Raises if [key] not found. *)
external update : 'k -> ('v -> 'v) -> ('k, 'v) t -> ('k, 'v) t = "Elixir.Map" "update!"

(** [delete key map] removes [key] from the map. No-op if [key] doesn't exist. *)
external delete : 'k -> ('k, 'v) t -> ('k, 'v) t = "Elixir.Map" "delete"

(** [pop key map] removes [key] and returns [(value, updated_map)]. Raises if not found. *)
external pop : 'k -> ('k, 'v) t -> 'v * ('k, 'v) t = "Elixir.Map" "pop!"

(** {1 Bulk Operations} *)

(** [take keys map] keeps only entries with keys in [keys]. *)
external take : 'k list -> ('k, 'v) t -> ('k, 'v) t = "Elixir.Map" "take"

(** [drop keys map] removes all entries with keys in [keys]. *)
external drop : 'k list -> ('k, 'v) t -> ('k, 'v) t = "Elixir.Map" "drop"

(** [split keys map] returns [(map_with_keys, map_without_keys)]. *)
external split : 'k list -> ('k, 'v) t -> ('k, 'v) t * ('k, 'v) t = "Elixir.Map" "split"

(** {1 Combining Maps} *)

(** [merge map1 map2] combines both maps. Values from [map2] win on key conflicts. *)
external merge : ('k, 'v) t -> ('k, 'v) t -> ('k, 'v) t = "Elixir.Map" "merge"

(** [equal map1 map2] returns [true] if both maps have the same keys and values. *)
external equal : ('k, 'v) t -> ('k, 'v) t -> bool = "Elixir.Map" "equal?"
