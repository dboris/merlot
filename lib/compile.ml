(** Compile - Uses OCaml's compiler-libs to parse and typecheck

    This module provides a clean interface to OCaml's frontend,
    consuming source files and producing TypedTree structures.
*)

(** Include directories for module lookup *)
let include_dirs : string list ref = ref []

(** Add an include directory *)
let add_include_dir dir =
  include_dirs := dir :: !include_dirs

(** Initialize the compiler environment *)
let init () =
  Clflags.nopervasives := false;
  Clflags.strict_sequence := true;
  Clflags.strict_formats := true;
  (* Initialize path first, then add our include directories *)
  Compmisc.init_path ();
  List.iter (Load_path.add_dir ~hidden:false) !include_dirs;
  ()

(** Parse an implementation file (.ml) *)
let parse_implementation filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf filename;
  let parsetree =
    try Parse.implementation lexbuf
    with e ->
      close_in ic;
      raise e
  in
  close_in ic;
  parsetree

(** Get the module name from a filename *)
let module_name_of_file filename =
  filename
  |> Filename.basename
  |> Filename.remove_extension
  |> String.capitalize_ascii

(** Type-check a parsed implementation *)
let typecheck_implementation filename parsetree =
  (* Note: don't call init_path here, it's done in init() *)
  let env = Compmisc.initial_env () in
  let prefix = Filename.remove_extension filename in
  let unit_info = Unit_info.make ~source_file:filename Unit_info.Impl prefix in
  let impl = Typemod.type_implementation unit_info env parsetree in
  impl.structure

(** Parse and typecheck a .ml file, returning the TypedTree *)
let compile_implementation filename =
  init ();
  let parsetree = parse_implementation filename in
  let typedtree = typecheck_implementation filename parsetree in
  typedtree
