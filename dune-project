(lang dune 3.0)
(name faker)
(generate_opam_files true)
(license MIT)
(package
 (name faker)
 (synopsis "Generate massive amounts of fake data - an OCaml port of faker-js")
 (description "A port of @faker-js/faker 10.6.0 to OCaml with seed-for-seed output parity (English locale).")
 (depends
  (ocaml (>= 5.0))
  (alcotest :with-test)))
