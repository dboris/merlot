let cases_dir = "cases"
let expected_dir = "expected"

let list_test_cases () =
  Sys.readdir cases_dir
  |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".ml")
  |> List.map Filename.remove_extension
  |> List.sort String.compare

let compile_to_string filename =
  Merlot.Compile.init ();
  let typedtree = Merlot.Compile.compile_implementation filename in
  let module_name = Merlot.Compile.module_name_of_file filename in
  let beam_module = Merlot.Typed_to_beam.convert_structure ~module_name typedtree in
  Merlot.Core_erlang.module_to_string beam_module

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let test_case name () =
  let input_file = Filename.concat cases_dir (name ^ ".ml") in
  let expected_file = Filename.concat expected_dir (name ^ ".core") in
  let actual = compile_to_string input_file in
  let expected = read_file expected_file in
  Alcotest.(check string) "Core Erlang output matches" expected actual

let () =
  let test_cases = list_test_cases () in
  let tests = List.map (fun name ->
    Alcotest.test_case name `Quick (test_case name)
  ) test_cases in
  Alcotest.run "Merlot" [
    "snapshot", tests;
  ]
