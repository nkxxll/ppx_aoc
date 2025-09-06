open Ppxlib

let generate_impl ~ctxt (rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  failwith ""

let generate_intf ~ctxt (rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  failwith ""

(* implementation generator *)
let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

(* interface generator *)
let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let my_deriver =
  Deriving.add "aoccli" ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
