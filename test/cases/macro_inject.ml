(* Test [@expose] attribute for unhygienic variable binding *)

(* Macro module - stubs recognized at compile time *)
module Macro = struct
  let quote x = x
  let unquote _ = failwith "macro"
  let to_string _ = failwith "macro"
  let defmacro f = f
end

(* Define a macro that uses [@expose] to mark a binding as unhygienic.
   When this macro is expanded, 'tmp' should NOT be renamed with a hygiene suffix.

   Normally, 'let tmp = ...' in a macro body would become 'let tmp__macro_N = ...'
   to avoid capturing the caller's 'tmp' variable. But with [@expose], the name
   stays as 'tmp' so it CAN be referenced by the caller. *)
let macro_swap a b =
  Macro.quote (
    (* This 'tmp' is exposed, so it won't be renamed *)
    let tmp [@expose] = Macro.unquote a in
    (* Simulate swap effect - in real code this would mutate *)
    let _ = Macro.unquote b in
    tmp
  )

(* Stub for the macro *)
let swap _ _ = failwith "macro stub"

(* Simple test - doesn't actually use the injected variable from caller scope,
   just tests that the IR generation works *)
let test () =
  let x = 1 in
  let y = 2 in
  swap x y
