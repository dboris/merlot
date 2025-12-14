(** Merlot Standard Library

    Provides OCaml bindings to Erlang/OTP and Elixir standard library modules.

    Usage:
    {[
      open Merlot_stdlib

      let main () =
        (* Process-based concurrency *)
        let pid = Process.spawn (fun () ->
          Process.receive (function
            | `Ping sender -> Process.send sender `Pong
            | `Stop -> ())
        ) in
        Process.send pid (`Ping (Process.self ()));

        (* Elixir-style pipelines *)
        [1; 2; 3; 4; 5]
        |> Enum.filter (fun x -> x mod 2 = 0)
        |> Enum.map (fun x -> x * x)
        |> Enum.sum
    ]}

    Module organization:
    - OCaml-compatible: [List], [Option], [String], [Result]
    - Elixir modules: [Enum], [Map], [Keyword]
    - Erlang/OTP: [Process], [Lists], [Erlang], [Timer], [Io]
    - OTP patterns: [Actor], [Supervisor]
*)

(** Data-first pipe operator.
    [x |. f a b] is equivalent to [f x a b] (x becomes the first argument).
    Useful for Elixir-style pipelines where data comes first.

    Example:
    {[
      numbers |. Enum.filter is_even |. Enum.map square |. Enum.sum
    ]}

    Note: The compiler inlines this at compile time, so there's no runtime overhead.
*)
external ( |. ) : 'a -> ('a -> 'b) -> 'b = "merlot" "pipe_first"

(** {1 OCaml-Compatible Modules} *)

(** OCaml-compatible List module *)
module List = List

(** OCaml-compatible Option module *)
module Option = Option

(** OCaml-compatible String module (Elixir-backed) *)
module String = String

(** Result type with combinators *)
module Result = Result

(** {1 Elixir Modules} *)

(** Elixir Enum - functional operations on enumerables *)
module Enum = Enum

(** Elixir Map - key-value maps *)
module Map = Map

(** Elixir Keyword - keyword lists (atom-keyed association lists) *)
module Keyword = Keyword

(** {1 Erlang/OTP Modules} *)

(** Process management and message passing *)
module Process = Process

(** Erlang BIFs and primitives *)
module Erlang = Erlang

(** Erlang lists module (direct bindings) *)
module Lists = Lists

(** Timer operations *)
module Timer = Timer

(** I/O operations *)
module Io = Io

(** {1 OTP Patterns} *)

(** Actor pattern for stateful processes *)
module Actor = Actor

(** Supervisor for fault-tolerant process trees *)
module Supervisor = Supervisor

(** {1 Metaprogramming} *)

(** Compile-time macro primitives *)
module Macro = Macro

(** {1 Debugging} *)

(** Term inspection and debugging utilities *)
module Inspect = Inspect
