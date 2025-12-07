(** Enum - Elixir Enum module bindings for Merlot

    Provides functional operations on enumerables (lists).
    Requires Elixir to be installed alongside Erlang/OTP.

    {2 Argument Order}

    Arguments follow OCaml convention (function first, data last) for
    seamless pipe operator compatibility:
    {[
      [1;2;3;4]
      |> Enum.filter (fun x -> x mod 2 = 0)
      |> Enum.map (fun x -> x * x)
      |> Enum.sum
      (* Result: 20 *)
    ]}

    The compiler automatically reorders arguments when calling [Elixir.Enum].
*)

(** {1 Transformations} *)

(** [map f list] applies [f] to each element, returning a new list. *)
external map : ('a -> 'b) -> 'a list -> 'b list = "Elixir.Enum" "map"

(** [filter pred list] keeps only elements where [pred x] is [true]. *)
external filter : ('a -> bool) -> 'a list -> 'a list = "Elixir.Enum" "filter"

(** [reject pred list] removes elements where [pred x] is [true] (opposite of [filter]). *)
external reject : ('a -> bool) -> 'a list -> 'a list = "Elixir.Enum" "reject"

(** [reduce f init list] folds [list] with [f], starting from [init].
    The reducer receives [(acc, elem)] and returns the new accumulator. *)
external reduce : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b = "Elixir.Enum" "reduce"

(** [flat_map f list] maps [f] over [list] and flattens the results. *)
external flat_map : ('a -> 'b list) -> 'a list -> 'b list = "Elixir.Enum" "flat_map"

(** {1 Predicates} *)

(** [any pred list] returns [true] if [pred x] holds for any element. *)
external any : ('a -> bool) -> 'a list -> bool = "Elixir.Enum" "any?"

(** [all pred list] returns [true] if [pred x] holds for all elements. *)
external all : ('a -> bool) -> 'a list -> bool = "Elixir.Enum" "all?"

(** [find pred list] returns the first element where [pred x] is [true].
    Raises if no element matches. *)
external find : ('a -> bool) -> 'a list -> 'a = "Elixir.Enum" "find"

(** [member elem list] returns [true] if [elem] is in [list]. *)
external member : 'a -> 'a list -> bool = "Elixir.Enum" "member?"

(** [empty list] returns [true] if [list] has no elements. *)
external empty : 'a list -> bool = "Elixir.Enum" "empty?"

(** {1 Counting} *)

(** [count list] returns the number of elements. *)
external count : 'a list -> int = "Elixir.Enum" "count"

(** [count_by pred list] counts elements where [pred x] is [true]. *)
external count_by : ('a -> bool) -> 'a list -> int = "Elixir.Enum" "count"

(** {1 Slicing} *)

(** [take n list] returns the first [n] elements. *)
external take : int -> 'a list -> 'a list = "Elixir.Enum" "take"

(** [drop n list] returns [list] with the first [n] elements removed. *)
external drop : int -> 'a list -> 'a list = "Elixir.Enum" "drop"

(** [take_while pred list] takes elements from the front while [pred] holds. *)
external take_while : ('a -> bool) -> 'a list -> 'a list = "Elixir.Enum" "take_while"

(** [drop_while pred list] drops leading elements while [pred] holds. *)
external drop_while : ('a -> bool) -> 'a list -> 'a list = "Elixir.Enum" "drop_while"

(** [at index list] returns the element at zero-based [index]. Raises if out of bounds. *)
external at : int -> 'a list -> 'a = "Elixir.Enum" "at"

(** [slice start len list] extracts [len] elements starting at [start]. *)
external slice : int -> int -> 'a list -> 'a list = "Elixir.Enum" "slice"

(** [split pos list] splits at [pos], returning [(before, from_pos_onward)]. *)
external split : int -> 'a list -> 'a list * 'a list = "Elixir.Enum" "split"

(** [chunk_every n list] groups elements into sublists of size [n]. *)
external chunk_every : int -> 'a list -> 'a list list = "Elixir.Enum" "chunk_every"

(** {1 Ordering} *)

(** [sort list] sorts in ascending order using default comparison. *)
external sort : 'a list -> 'a list = "Elixir.Enum" "sort"

(** [sort_by key_fn list] sorts by keys extracted with [key_fn]. *)
external sort_by : ('a -> 'b) -> 'a list -> 'a list = "Elixir.Enum" "sort_by"

(** [reverse list] reverses the element order. *)
external reverse : 'a list -> 'a list = "Elixir.Enum" "reverse"

(** [shuffle list] randomly reorders elements. *)
external shuffle : 'a list -> 'a list = "Elixir.Enum" "shuffle"

(** {1 Aggregations} *)

(** [sum list] returns the sum of all integer elements. *)
external sum : int list -> int = "Elixir.Enum" "sum"

(** [min list] returns the smallest element. List must be non-empty. *)
external min : 'a list -> 'a = "Elixir.Enum" "min"

(** [max list] returns the largest element. List must be non-empty. *)
external max : 'a list -> 'a = "Elixir.Enum" "max"

(** {1 Uniqueness} *)

(** [uniq list] removes duplicates, keeping first occurrence. *)
external uniq : 'a list -> 'a list = "Elixir.Enum" "uniq"

(** [uniq_by key_fn list] removes duplicates by key, keeping first occurrence of each key. *)
external uniq_by : ('a -> 'b) -> 'a list -> 'a list = "Elixir.Enum" "uniq_by"

(** {1 Combining} *)

(** [zip list1 list2] pairs elements from both lists. Stops at shorter list. *)
external zip : 'a list -> 'b list -> ('a * 'b) list = "Elixir.Enum" "zip"

(** [unzip pairs] splits a list of pairs into two lists. *)
external unzip : ('a * 'b) list -> 'a list * 'b list = "Elixir.Enum" "unzip"

(** [join sep list] concatenates elements into a string with [sep] between them. *)
external join : string -> 'a list -> string = "Elixir.Enum" "join"

(** [intersperse sep list] inserts [sep] between each pair of elements. *)
external intersperse : 'a -> 'a list -> 'a list = "Elixir.Enum" "intersperse"

(** [concat lists] flattens a list of lists into a single list. *)
external concat : 'a list list -> 'a list = "Elixir.Enum" "concat"

(** {1 Indexing} *)

(** [with_index list] pairs each element with its zero-based index. *)
external with_index : 'a list -> ('a * int) list = "Elixir.Enum" "with_index"

(** [first list] returns the first element. Raises if empty. *)
external first : 'a list -> 'a = "Elixir.List" "first"

(** [last list] returns the last element. Raises if empty. *)
external last : 'a list -> 'a = "Elixir.List" "last"

(** [random list] returns a random element. List must be non-empty. *)
external random : 'a list -> 'a = "Elixir.Enum" "random"

(** {1 Grouping} *)

(** [group_by key_fn list] groups elements by keys extracted with [key_fn].
    Returns [(key, elements)] pairs. *)
external group_by : ('a -> 'b) -> 'a list -> ('b * 'a list) list = "Elixir.Enum" "group_by"
