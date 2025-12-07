(* Greeting - An I/O example

   Demonstrates:
   - Printing to stdout with Io.put_chars
   - Reading input with Io.get_line
   - String concatenation
*)

open Merlot_stdlib

(* Helper to print with newline *)
let println s = Io.put_chars (s ^ "\n")

(* Print a greeting banner *)
let print_banner () =
  println "================================";
  println "  Welcome to Merlot on BEAM!";
  println "================================"

(* Ask for name and greet *)
let greet () =
  Io.put_chars "What is your name? ";
  let name = String_utils.trim (Io.get_line "") in
  println "";
  println ("Hello, " ^ name ^ "!");
  println ("Nice to meet you, " ^ name ^ "!");
  name

(* Main function *)
let main () =
  print_banner ();
  println "";
  let name = greet () in
  println "";
  println "Goodbye!";
  name
