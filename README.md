# ppx_pyformat
[![OCaml](https://img.shields.io/badge/-OCaml-EC6813?logo=ocaml&labelColor=grey)](#)
[![CI Workflow](https://github.com/bn-d/ppx_pyformat/actions/workflows/build.yml/badge.svg)](https://github.com/bn-d/ppx_pyformat/actions/workflows/build.yml)

ppxlib based rewriter inspired by Python string `format()`. This rewriter provides the ability to do complex variable substitutions and value formatting.

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
  print_string [%pyformat "{action} {0}!"; "World"]
```

The rewriter takes a required format string following with optional input arguments. The rewriter will parse the format string, substitute/format the replacement fields and finally return everything as a single `string`. This works very similarly to its archetype, the `str.format()` method in Python.

### Format String Syntax
The format string syntax of `ppx_pyformat` is closely modeled after [that of Python3.10](https://docs.python.org/3.10/library/string.html#format-string-syntax). Most format strings should be compatible with `ppx_pyformat` with minor modification. But even with the best effort to match Python `format`'s behaviors, there are still key differences due the differences in languages. Several additional features are implemented to overcome the shortfall of static typing and enable more capabilities.

Format strings contain “replacement fields” surrounded by curly braces `{}`. Anything that is not contained in braces is considered literal text, which is copied unchanged to the output. If the user needs to include a brace character in the literal text, it can be escaped by doubling: `{{` and `}}`.

The grammar for a replacement field is as follows:

```python
replacement_field ::= "{" [arg_name] ["[" element_index "]"] ["!" conversion] ["!" format_spec] "}"
```

#### Argument name
The argument name is either a number or a variable name. If it is a number, it refers to its index (starting from `0`) in the input arguments. If it is a variable name, it refers to an OCaml value that is accessible under the current scope.
The argument names can be all omitted and the number 0, 1, 2, ... will be automatically inserted in that order. The argument names can NOT be partially omitted.

#### Element index
The element index takes an integer index. When provided, the correlating argument name must represent a `list`. `List.nth` will be used to get the element at the index.

#### Conversion
The conversion field takes an OCaml function name. The function must only take 1 argument with the type of the argument name, and return `string`, `float` or `int`. The conversion function will be applied after indexing before formatting. With the appropriate conversion function, values of any type can be used with `ppx_pyformat`.

#### Format Specification
“Format specifications” are used within replacement fields contained within a format string to define how individual values are presented.

The general form of a standard format specifier is:

```python
format_spec     ::=  [[fill]align][sign][#][0][width][grouping_option][.precision][type]
fill            ::=  <any character>
align           ::=  "<" | ">" | "=" | "^"
sign            ::=  "+" | "-" | " "
width           ::=  digit+
grouping_option ::=  "_" | ","
precision       ::=  digit+
type            ::=  "b" | "c" | "d" | "e" | "E" | "f" | "F" | "g" | "G" | "o" | "s" | "x" | "X" | "%"
```

The `type` must be provided accordingly if the argument name (after indexing and conversion if provided) represent a value of either `int` or `float`. Otherwise, `string` will be assumed. Type option `n` is NOT supported.

Rest of the format specification should be the same as [that of Python3.10](https://docs.python.org/3.10/library/string.html#format-specification-mini-language).

## TODO
- Support negative index in list
- Support hashtable in index

## Copyright
Licensed under the [MIT License](LICENSE).

Part of the documentation and string format syntax borrowed from Python is licensed under the [Python Software Founddation License Version 2](https://docs.python.org/3/license.html):
Copyright © 2001-2021 Python Software Foundation. All rights reserved.
