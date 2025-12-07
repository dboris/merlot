(lang dune 3.20)

(name merlot)

(generate_opam_files true)

(source
 (github dboris/merlot))

(authors "Boris D. <borisd@gmx.com>")

(maintainers "Boris D. <borisd@gmx.com>")

(license MIT)

(package
 (name merlot)
 (synopsis "OCaml to BEAM/Erlang compiler")
 (description "Merlot compiles OCaml to Core Erlang, targeting the BEAM virtual machine")
 (depends
  (ocaml (>= 5.2))
  dune
  (cmdliner (>= 1.2))
  (alcotest :with-test))
 (tags
  (ocaml erlang beam compiler)))
