(lang dune 2.7)
(name ppx_pyformat)
(version "0.1.2")

(generate_opam_files true)

(source (github bn-d/ppx_pyformat))
(license MIT)
(authors "Boning <me@boni.ng>")
(maintainers "Boning <me@boni.ng>")
(documentation "https://github.com/bn-d/ppx_pyformat")

(package
 (name ppx_pyformat)
 (synopsis "Ppxlib based string format rewriter inspired by Python string `format`")
 (depends
  (ocaml (>= 4.08))
  (ppxlib (>= 0.23.0))
  (ppx_make (>= 0.3.0))
  (menhir (>= 20200624))
  (ounit2 :with-test)))
(using menhir 2.1)
