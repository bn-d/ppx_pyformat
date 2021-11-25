(lang dune 2.7)
(name ppx_pyformat)
(version "0.1.0")

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
  (ppxlib (>= 0.10.0))
  (ppx_make (>= 0.3.0))
  menhir
  (ounit2 :with-test)))
(using menhir 2.1)
