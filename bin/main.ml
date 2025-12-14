(** Merlot - OCaml to BEAM compiler

    Usage:
      merlot <file.ml> [-o output.beam] [-I dir] [--emit-core] [--emit-docs]
      merlot init <project-name>
*)

let version = Printf.sprintf "%s (built %s)" Version_info.version Version_info.build_time

(** Find stdlib directory relative to executable or build location *)
let find_stdlib_dir () =
  (* Try to find stdlib relative to executable *)
  let exe_dir = Filename.dirname Sys.executable_name in
  let candidates = [
    (* When running from _build/default/bin *)
    Filename.concat exe_dir "../stdlib/.merlot_stdlib.objs/byte";
    (* When installed *)
    Filename.concat exe_dir "../lib/merlot/stdlib";
    (* Development: relative to current dir *)
    "_build/default/stdlib/.merlot_stdlib.objs/byte";
  ] in
  List.find_opt Sys.file_exists candidates

(* ============================================================================
   Project Initialization (merlot init)
   ============================================================================ *)

(** Template for main.ml *)
let main_ml_template project_name = Printf.sprintf {|open Merlot_stdlib

let main () =
  Io.put_chars "Hello from %s!\n"
|} project_name

(** Template for Makefile *)
let makefile_template _project_name = {|.PHONY: all clean run shell

# Merlot compiler - adjust path as needed
MERLOT ?= merlot

# Stdlib BEAM path - auto-detect from opam
MERLOT_STDLIB ?= $(shell opam var merlot:lib 2>/dev/null)/beam

# Source files - compiled beam files go in ebin/
SOURCES = $(wildcard src/*.ml)

all: ebin $(SOURCES:src/%.ml=compile-%)

ebin:
	mkdir -p ebin

# Compile each source file; merlot produces Merlot.<Capitalized>.beam
compile-%: src/%.ml
	$(MERLOT) $< -o ebin/

run: all
	@erl -noshell -pa ebin -pa $(MERLOT_STDLIB) -eval "'Merlot.Main':main(ok), halt()."

shell: all
	@erl -pa ebin -pa $(MERLOT_STDLIB) -eval '[code:load_file(list_to_atom(filename:basename(F, ".beam"))) || F <- filelib:wildcard("ebin/*.beam")]'

clean:
	rm -rf ebin/*.beam ebin/*.core erl_crash.dump
|}

(** Template for .merlin - ocaml-lsp integration.
    If stdlib_path is provided, includes it directly. *)
let merlin_template stdlib_path =
  let stdlib_line = match stdlib_path with
    | Some path -> Printf.sprintf "B %s\n" path
    | None -> {|# Merlot stdlib - adjust path based on your installation:
# - Development: /path/to/merlot/_build/default/stdlib/.merlot_stdlib.objs/byte
# - Installed: run `opam var merlot:lib`/stdlib
# B /path/to/merlot/stdlib
|}
  in
  Printf.sprintf {|S src
B ebin

%s|} stdlib_line

(** Template for .vscode/settings.json *)
let vscode_settings_template = {|{
  "ocaml.server.args": ["--fallback-read-dot-merlin"]
}
|}

(** Template for .gitignore *)
let gitignore_template = {|# Build artifacts
ebin/*.beam
ebin/*.core
*.cmi
*.cmo
*.cmx
*.o
erl_crash.dump

# Editor
*~
.#*
\#*#
|}

(** Template for README *)
let readme_template project_name = Printf.sprintf {|# %s

A Merlot project.

## Build

```bash
make
```

## Run

```bash
make run
```

## Clean

```bash
make clean
```

## Interactive Shell

Start an Erlang shell with your modules loaded:

```bash
make shell
```

Then call your modules interactively:

```erlang
%% Call functions (module names are quoted atoms with Merlot. prefix)
'Merlot.Main':main(ok).

%% Hot reload after recompiling (in another terminal: make)
l('Merlot.Main').

%% Exit the shell
q().
```

## Editor Setup

For ocaml-lsp support in VS Code (OCaml Platform extension), install:

```bash
opam install dot-merlin-reader
```

This project includes `.vscode/settings.json` and `.merlin` for out-of-the-box IDE support.
|} (String.capitalize_ascii project_name)

(** Create a file with content *)
let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

(** Initialize a new Merlot project *)
let init_project name =
  if Sys.file_exists name then begin
    Printf.eprintf "Error: Directory '%s' already exists\n" name;
    `Error (false, "Directory already exists")
  end else begin
    try
      (* Create project structure *)
      Sys.mkdir name 0o755;
      Sys.mkdir (Filename.concat name "src") 0o755;
      Sys.mkdir (Filename.concat name "ebin") 0o755;
      Sys.mkdir (Filename.concat name ".vscode") 0o755;

      (* Write template files *)
      write_file (Filename.concat name "src/main.ml") (main_ml_template name);
      write_file (Filename.concat name "Makefile") (makefile_template name);
      write_file (Filename.concat name ".gitignore") gitignore_template;
      write_file (Filename.concat name ".merlin") (merlin_template (find_stdlib_dir ()));
      write_file (Filename.concat name ".vscode/settings.json") vscode_settings_template;
      write_file (Filename.concat name "README.md") (readme_template name);

      Printf.printf "Created project '%s'\n" name;
      Printf.printf "\n";
      Printf.printf "  cd %s\n" name;
      Printf.printf "  make run\n";
      Printf.printf "\n";
      `Ok ()
    with
    | Sys_error msg ->
        Printf.eprintf "Error: %s\n" msg;
        `Error (false, msg)
  end

(* ============================================================================
   Compilation (merlot compile / merlot <file>)
   ============================================================================ *)

(** Run erlc to compile Core Erlang to BEAM *)
let run_erlc core_file =
  let output_dir = Filename.dirname core_file in
  let output_opt = if output_dir = "." then "" else Printf.sprintf "-o %s " (Filename.quote output_dir) in
  let cmd = Printf.sprintf "erlc %s+from_core %s" output_opt (Filename.quote core_file) in
  let exit_code = Sys.command cmd in
  if exit_code = 0 then Ok ()
  else Error (Printf.sprintf "erlc failed with exit code %d" exit_code)


(** Determine output format from -o filename extension *)
let output_wants_core output =
  match output with
  | Some f -> Filename.check_suffix f ".core"
  | None -> false

(** Compile a single .ml file to BEAM (or Core Erlang with --emit-core or -o *.core) *)
let compile_file ~input ~output ~include_dirs ~no_stdlib ~emit_core ~emit_docs =
  (* Detect output format: --emit-core flag OR -o foo.core *)
  let emit_core = emit_core || output_wants_core output in
  try
    (* Add stdlib directory unless disabled *)
    (if not no_stdlib then
      match find_stdlib_dir () with
      | Some dir -> Merlot.Compile.add_include_dir dir
      | None -> ());

    (* Add user include directories *)
    List.iter Merlot.Compile.add_include_dir include_dirs;

    (* Parse and typecheck *)
    let typedtree = Merlot.Compile.compile_implementation input in

    (* Get module name from filename *)
    let module_name = Merlot.Compile.module_name_of_file input in

    (* Convert to BEAM IR *)
    let beam_module = Merlot.Typed_to_beam.convert_structure ~module_name typedtree in

    (* Determine output directory: if -o is a directory, use it; otherwise use input's dir *)
    let output_dir, output_file =
      match output with
      | Some path when Sys.is_directory path ->
          (* -o points to a directory - output files go there with auto-generated names *)
          let dir = if String.length path > 0 && path.[String.length path - 1] = '/' then path else path ^ "/" in
          (dir, None)
      | Some path ->
          (* -o is a specific file *)
          let dir = Filename.dirname path in
          let dir = if dir = "." then "" else dir ^ "/" in
          (dir, Some path)
      | None ->
          (* No -o specified - use input file's directory *)
          let dir = Filename.dirname input in
          let dir = if dir = "." then "" else dir ^ "/" in
          (dir, None)
    in
    (* Use the actual module name from beam_module (respects [@@@elixir_struct]) *)
    let actual_module_name = beam_module.name in
    let core_file = output_dir ^ actual_module_name ^ ".core" in
    let beam_file = output_dir ^ actual_module_name ^ ".beam" in

    (* Extract and generate documentation if requested *)
    if emit_docs then begin
      let doc = Merlot.Doc_extract.extract_module_doc module_name typedtree in
      let md_file = output_dir ^ actual_module_name ^ ".md" in
      let chunk_file = output_dir ^ actual_module_name ^ ".chunk" in
      Merlot.Doc_extract.write_markdown md_file doc;
      Merlot.Doc_extract.write_docs_chunk chunk_file doc;
      Printf.printf "Generated docs: %s, %s\n" md_file chunk_file
    end;

    (* Write Core Erlang *)
    Merlot.Core_erlang.write_module core_file beam_module;

    if emit_core then begin
      (* User wants .core file - we're done *)
      let final_output = match output_file with Some f -> f | None -> core_file in
      if final_output <> core_file then
        Sys.rename core_file final_output;
      Printf.printf "Compiled %s -> %s\n" input final_output;
      Ok ()
    end else begin
      (* Compile to BEAM using erlc *)
      match run_erlc core_file with
      | Ok () ->
          (* Remove intermediate .core file *)
          Sys.remove core_file;
          (* Rename .beam if custom output specified *)
          let final_output = match output_file with Some f -> f | None -> beam_file in
          if final_output <> beam_file then
            Sys.rename beam_file final_output;
          Printf.printf "Compiled %s -> %s\n" input final_output;
          Ok ()
      | Error msg ->
          Error msg
    end
  with
  | Sys_error msg ->
      Error (Printf.sprintf "System error: %s" msg)
  | Syntaxerr.Error err ->
      let loc = Syntaxerr.location_of_error err in
      Error (Printf.sprintf "Syntax error at %s" (Location.show_filename loc.loc_start.pos_fname))
  | Typecore.Error (loc, env, err) ->
      let error = Typecore.report_error ~loc env err in
      let buf = Buffer.create 256 in
      let fmt = Format.formatter_of_buffer buf in
      Location.print_report fmt error;
      Format.pp_print_flush fmt ();
      Error (Printf.sprintf "Type error:\n%s" (Buffer.contents buf))
  | Failure msg ->
      Error (Printf.sprintf "Compilation failed: %s" msg)
  | Env.Error _ as e ->
      Error (Printf.sprintf "Environment error: %s" (Printexc.to_string e))
  | e ->
      Error (Printf.sprintf "Unexpected error: %s" (Printexc.to_string e))

(* ============================================================================
   Command-line interface using Cmdliner
   ============================================================================ *)

open Cmdliner

(* --- Init command --- *)

let project_name =
  let doc = "Name of the project to create." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc)

let init_cmd_run name = init_project name

let init_cmd =
  let doc = "Create a new Merlot project" in
  let man = [
    `S Manpage.s_description;
    `P "Creates a new Merlot project with a standard directory structure:";
    `Pre "  <name>/\n  ├── src/main.ml    # Entry point\n  ├── ebin/          # Compiled .beam files\n  ├── Makefile       # Build script\n  ├── .gitignore\n  └── README.md";
    `S Manpage.s_examples;
    `P "Create a new project:";
    `Pre "  merlot init myapp";
    `P "Then build and run:";
    `Pre "  cd myapp\n  make run";
  ] in
  let info = Cmd.info "init" ~doc ~man in
  Cmd.v info Term.(ret (const init_cmd_run $ project_name))

(* --- Compile command (default) --- *)

let input_file =
  let doc = "OCaml source file to compile." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let output_file =
  let doc = "Output file. Format is detected from extension: .core emits Core Erlang, .beam (default) emits BEAM bytecode." in
  Arg.(value & opt (some string) None & info ["o"; "output"] ~docv:"OUTPUT" ~doc)

let include_dirs =
  let doc = "Add directory to module search path." in
  Arg.(value & opt_all dir [] & info ["I"] ~docv:"DIR" ~doc)

let no_stdlib =
  let doc = "Don't automatically include the standard library." in
  Arg.(value & flag & info ["no-stdlib"] ~doc)

let emit_core =
  let doc = "Emit Core Erlang (.core) instead of BEAM bytecode (.beam)." in
  Arg.(value & flag & info ["emit-core"] ~doc)

let emit_docs =
  let doc = "Generate documentation files (.md and .chunk for EEP-48)." in
  Arg.(value & flag & info ["emit-docs"] ~doc)

let compile_cmd_run input output include_dirs no_stdlib emit_core emit_docs =
  match compile_file ~input ~output ~include_dirs ~no_stdlib ~emit_core ~emit_docs with
  | Ok () -> `Ok ()
  | Error msg ->
      Printf.eprintf "Error: %s\n" msg;
      `Error (false, msg)

let compile_cmd =
  let doc = "Compile an OCaml file to BEAM bytecode" in
  let man = [
    `S Manpage.s_description;
    `P "Compiles an OCaml source file to BEAM bytecode for the Erlang VM.";
    `P "The standard library (Merlot_stdlib) is automatically included unless --no-stdlib is passed.";
    `S Manpage.s_examples;
    `P "Compile a file:";
    `Pre "  merlot compile hello.ml";
    `P "Or without the compile subcommand:";
    `Pre "  merlot hello.ml";
  ] in
  let info = Cmd.info "compile" ~doc ~man in
  Cmd.v info Term.(ret (const compile_cmd_run $ input_file $ output_file $ include_dirs $ no_stdlib $ emit_core $ emit_docs))

(* --- Main command group --- *)

let default_cmd =
  Term.(ret (const compile_cmd_run $ input_file $ output_file $ include_dirs $ no_stdlib $ emit_core $ emit_docs))

let main_cmd =
  let doc = "OCaml to BEAM bytecode compiler" in
  let man = [
    `S Manpage.s_description;
    `P "Merlot compiles OCaml source files to BEAM bytecode for the Erlang VM.";
    `P "The standard library (Merlot_stdlib) is automatically included unless --no-stdlib is passed.";
    `S Manpage.s_commands;
    `P "Use $(b,merlot init) to create a new project.";
    `P "Use $(b,merlot compile) (or just $(b,merlot <file>)) to compile.";
    `S Manpage.s_examples;
    `P "Create a new project:";
    `Pre "  merlot init myapp";
    `P "Compile a file:";
    `Pre "  merlot hello.ml";
    `P "Specify output:";
    `Pre "  merlot hello.ml -o hello.beam";
    `P "Emit Core Erlang (for debugging):";
    `Pre "  merlot hello.ml -o hello.core";
    `Pre "  merlot hello.ml --emit-core";
    `P "Generate documentation:";
    `Pre "  merlot hello.ml --emit-docs";
    `P "With include path:";
    `Pre "  merlot client.ml -I ./lib";
    `S Manpage.s_bugs;
    `P "Report bugs at https://github.com/dboris/merlot/issues";
  ] in
  let info = Cmd.info "merlot" ~version ~doc ~man in
  Cmd.group info ~default:default_cmd [init_cmd; compile_cmd]

(** Preprocess argv to support `merlot file.ml` without explicit `compile` subcommand.
    If the first argument looks like a file (ends with .ml), prepend "compile". *)
let preprocess_argv () =
  let argv = Sys.argv in
  if Array.length argv >= 2 then
    let first_arg = argv.(1) in
    (* If first arg is not a subcommand and looks like a file or flag, it's for compile *)
    if first_arg <> "init" && first_arg <> "compile" && first_arg <> "--help" && first_arg <> "--version" then
      (* Insert "compile" after the program name *)
      let new_argv = Array.make (Array.length argv + 1) "" in
      new_argv.(0) <- argv.(0);
      new_argv.(1) <- "compile";
      Array.blit argv 1 new_argv 2 (Array.length argv - 1);
      new_argv
    else
      argv
  else
    argv

let () = exit (Cmd.eval ~argv:(preprocess_argv ()) main_cmd)
