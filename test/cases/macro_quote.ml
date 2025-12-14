(* Test macro quoting *)

(* Macro module stubs - recognized at compile time *)
module Macro = struct
  let quote x = x
  let u _ = failwith "macro"
  let s _ = failwith "macro"
  let stringify _ = failwith "macro"
end

(* Simple quoted expression - quote wraps the AST *)
let quoted_add = Macro.quote (1 + 2)

(* Function that uses quote *)
let make_add_ast x y =
  Macro.quote (x + y)
