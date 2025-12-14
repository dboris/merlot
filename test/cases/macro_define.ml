(* Test macro definition and expansion *)

(* Macro module stubs - recognized at compile time *)
module Macro = struct
  let quote x = x
  let unquote _ = failwith "macro"
  let to_string _ = failwith "macro"
end

(* Define a macro using the macro_ prefix convention.
   The function body should use Macro.quote with Macro.unquote for splicing parameters. *)
let macro_unless condition body =
  Macro.quote (
    if not (Macro.unquote condition) then
      Macro.unquote body
    else
      ()
  )

(* Stub for the macro - allows OCaml typechecker to pass.
   The actual call gets expanded by the macro system. *)
let unless _ _ = failwith "macro stub"

(* Use the macro - this should expand at compile time *)
let safe_divide x y =
  unless (y = 0) (x / y)
