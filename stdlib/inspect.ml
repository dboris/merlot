(** Inspect - Erlang-style term inspection and debugging

    Provides functions for inspecting and printing any Erlang term.
    Uses Erlang's built-in term formatting (~p) which handles all
    data types including records, tuples, lists, maps, and binaries.

    Usage:
    {[
      open Merlot_stdlib

      type person = { name: string; age: int }
      let alice = { name = "Alice"; age = 30 }

      (* Get string representation *)
      let s = Inspect.to_string alice
      (* => "#{age => 30,name => <<\"Alice\">>}" *)

      (* Print to stdout *)
      Inspect.print alice

      (* Pipeline-friendly: prints and returns the value *)
      let result =
        compute_something ()
        |> Inspect.inspect   (* prints intermediate value *)
        |> further_processing
    ]}

    The output format is Erlang-native:
    - Records: [#{field => value, ...}]
    - Tuples: [{a, b, c}]
    - Lists: [[1, 2, 3]]
    - Strings: [<<"hello">>]
    - Atoms: ['atom_name']

    For OCaml-style output, use [Printf.printf "%p"] instead.
*)

(** Flatten an iolist to a binary/string. *)
external iolist_to_binary : 'a -> string = "erlang" "iolist_to_binary"

(** Format to iolist. *)
external io_lib_format : string -> 'a list -> string = "io_lib" "format"

(** Format and print (simplified - format string is passed through). *)
external io_format : string -> 'a list -> unit = "io" "format"

(** Convert any term to its Erlang string representation.
    Uses Erlang's ~p format specifier for pretty-printing. *)
let to_string term =
  iolist_to_binary (io_lib_format "~p" [term])

(** Print any term to stdout with a newline.
    Equivalent to [Io.format "~p~n" [term]]. *)
let print term = io_format "~p~n" [term]

(** Print any term to stdout without a newline. *)
let print_inline term = io_format "~p" [term]

(** Inspect a value: prints it and returns it unchanged.
    Perfect for debugging in pipelines.
    {[
      data
      |> transform
      |> Inspect.inspect  (* see intermediate value *)
      |> further_transform
    ]} *)
let inspect term =
  print term;
  term

(** Inspect with a label prefix.
    {[
      Inspect.inspect_label "result" (compute ())
      (* prints: result: <value> *)
    ]} *)
let inspect_label label term =
  io_format "~s: ~p~n" [label; term];
  term

(** {1 String Output}

    For printing strings (like pp function output) without Erlang binary wrapper. *)

(** Print a string to stdout with newline.
    Unlike [print], this uses ~s format so strings display cleanly
    without the <<"...">> wrapper.
    {[
      Inspect.puts (pp_person alice)  (* prints: {name = "Alice"; age = 30} *)
    ]} *)
let puts s = io_format "~s~n" [s]

(** Print a string to stdout without newline. *)
let puts_inline s = io_format "~s" [s]

(** {1 Type-Aware Pretty Printing} *)

(** Convert any value to an OCaml-style string representation.
    This is a compiler intrinsic - at compile time, the compiler inspects
    the type of the argument and generates a call to the appropriate
    auto-generated [pp_<typename>] function.

    For primitive types (int, float, string, bool), it uses direct formatting.
    For user-defined types (records, variants), it calls the auto-generated
    pp function.

    {[
      type person = { name: string; age: int }
      type color = Red | Green | Blue

      let main () =
        let alice = { name = "Alice"; age = 30 } in
        Inspect.puts (Inspect.pp alice);  (* {name = "Alice"; age = 30} *)
        Inspect.puts (Inspect.pp Red);    (* Red *)
        Inspect.puts (Inspect.pp 42);     (* 42 *)
    ]}

    Note: This function is polymorphic at the type level but monomorphic
    at each call site - the compiler knows the concrete type of each argument. *)
let pp : 'a -> string = fun _ -> failwith "Inspect.pp should be expanded at compile-time"
