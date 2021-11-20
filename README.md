# ppx_pyformat
[![OCaml](https://img.shields.io/badge/-OCaml-EC6813?logo=ocaml&labelColor=grey)](#)
[![CI Workflow](https://github.com/bn-d/ppx_pyformat/actions/workflows/build.yml/badge.svg)](https://github.com/bn-d/ppx_pyformat/actions/workflows/build.yml)

ppxlib based string format rewriter inspired by Python string `format`. This rewriter provides the ability to do complex variable substitutions and value formatting.

## Installation
`ppx_pyformat` can be installed via [OCaml Package Manager](https://opam.ocaml.org/packages/ppx_pyformat/).
```sh
$ opam install ppx_pyformat
```

## Usage
To use this rewriter, add `(preprocess (pps ppx_pyformat))` to the library/executable configuration in `dune` file.

## Syntax
```ocaml
let _ =
  let action = "Hello" in
  Printf.printf [%pyformat "{action} {0}!"; "World"]
```

The rewriter takes a required format string following with optional input values. The rewritter will parse the format string, substitute/format the replacement fields and finally return everything as a single `string`. This works very similarly to its archetype, the `str.format()` method in Python.

### Format String Syntax
The format string syntax of `ppx_pyformat` is closely modelled on [that of Python 3.10](https://docs.python.org/3.10/library/string.html#format-string-syntax). A lot of Python format strings should be compatible with `ppx_format`. But even with the best effort to match Python's behaviors, there are still key differences due the differenceis in languages. Several additional features are also implemented to overcome the shortfall of static typing and enable more capbility.

Format strings contain “replacement fields” surrounded by curly braces `{}`. Anything that is not contained in braces is considered literal text, which is copied unchanged to the output. If you need to include a brace character in the literal text, it can be escaped by doubling: `{{` and `}}`.

The grammar for a replacement field is as follows:

```
replacement_field ::= "{" [arg_name] ["[" element_index "]"] "}"
```

#### Argument name

#### Element index

#### Conversion

#### Format Specification

Main difference with Python

No attribute name.
Index pending.
Conversion support any function.
No number type for int and float.

## TODO
Default field is positional
Support negative index in list
Support hashtable for index

## Copyright
Licensed under the [MIT License](LICENSE).
Part of the documentation and string format syntax borrowed from Python is licensed under the [Python Software Founddation License Version 2](https://docs.python.org/3/license.html):
Copyright © 2001-2021 Python Software Foundation. All rights reserved.
