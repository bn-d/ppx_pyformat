module P = Ppxlib

let rules : P.Context_free.Rule.t list = []
(*[ P.Extension.V3.declare
    "ppx_pyformat.pyformat"
    P.Extension.Context.expression
    P.Ast_pattern. ]*)

let () = P.Driver.register_transformation ~rules "ppx_pyformat"
