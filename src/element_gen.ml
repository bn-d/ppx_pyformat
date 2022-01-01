open Types
module P = Ppxlib
module Ast_builder = Ppxlib.Ast_builder.Default
module Ast_helper = Ppxlib.Ast_helper

let string_expr_of_element ~loc (element : element) : P.expression =
  match element with
  | Text str -> Ast_builder.estring ~loc str
  | _ -> failwith "not impl"
