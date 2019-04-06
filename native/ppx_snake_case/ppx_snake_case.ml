open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let structure_item_mapper mapper item = 
   begin match item with
      | { pstr_desc =
          Pstr_extension (({ txt = "example"; loc }, _), _)} ->
            Str.type_ ~loc Nonrecursive [
              Type.mk ~loc { txt = "example"; loc }
              ~manifest:(Typ.constr { txt = Lident "int"; loc } [])]
      (* Delegate to the default mapper. *)
      | x -> default_mapper.structure_item mapper x;
  end

let example_mapper argv =
  { 
    default_mapper with
    structure_item = structure_item_mapper;
  }
 
let () = register "snake_case" example_mapper