open Ppxlib
module List = ListLabels
open Ast_builder.Default

(* todo here next:
   - we have working accessor derivers
   - baby steps:
    - we parse a record of days that should all be named like "day<number>"
    - we build a match statement that executes the right function
    ```
    let match_aoc struct day task =
      match day, task with
      <number>, <task> -> struct.day<number>.<task> ()
    ```
*)

(* TODO if the name is not day<number> it should not be passed to this function *)
let make_cases ~loc (field : label_declaration) =
  let name = field.pld_name.txt in
  (* Extract day number from "dayN" *)
  let day_number = Scanf.sscanf name "day%d" (fun n -> n) in
  let day_expr =
    pexp_field
      ~loc
      (evar ~loc "aoc_record") (* the record expression, e.g., a variable "record" *)
      (Located.lident ~loc name)
  in
  (* Case 1: Some 1 *)
  let case1 =
    case
      ~lhs:
        (ppat_tuple
           ~loc
           [ ppat_constant ~loc (Pconst_integer (string_of_int day_number, None))
           ; ppat_construct
               ~loc
               (Located.lident ~loc "Some")
               (Some (ppat_constant ~loc (Pconst_integer ("1", None))))
           ])
      ~guard:None
      ~rhs:
        [%expr
          let task1, _ = [%e day_expr] in
          task1 ()]
  in
  (* Case 2: Some 2 *)
  let case2 =
    case
      ~lhs:
        (ppat_tuple
           ~loc
           [ ppat_constant ~loc (Pconst_integer (string_of_int day_number, None))
           ; ppat_construct
               ~loc
               (Located.lident ~loc "Some")
               (Some (ppat_constant ~loc (Pconst_integer ("2", None))))
           ])
      ~guard:None
      ~rhs:
        [%expr
          let _, task2 = [%e day_expr] in
          task2 ()]
  in
  (* Case 3: None *)
  let case3 =
    case
      ~lhs:
        (ppat_tuple
           ~loc
           [ ppat_constant ~loc (Pconst_integer (string_of_int day_number, None))
           ; ppat_construct ~loc (Located.lident ~loc "None") None
           ])
      ~guard:None
      ~rhs:
        [%expr
          let task1, task2 = [%e day_expr] in
          task1 ();
          task2 ()]
  in
  [ case1; case2; case3 ]
;;

let make_run_aoc ~loc cases =
  (* match (day, task) with ... *)
  let match_expr =
    pexp_match ~loc (pexp_tuple ~loc [ evar ~loc "day"; evar ~loc "task" ]) cases
  in
  (* let run_aoc aoc_record day task = match ... *)
  pstr_value
    ~loc
    Nonrecursive
    [ value_binding
        ~loc
        ~pat:(ppat_var ~loc { loc; txt = "run_aoc" })
        ~expr:
          (pexp_fun
             ~loc
             Nolabel
             None
             (ppat_var ~loc { loc; txt = "aoc_record" })
             (pexp_fun
                ~loc
                Nolabel
                None
                (ppat_var ~loc { loc; txt = "day" })
                (pexp_fun
                   ~loc
                   Nolabel
                   None
                   (ppat_var ~loc { loc; txt = "task" })
                   match_expr)))
    ]
;;

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun (td : type_declaration) ->
    match td with
    | { ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open; ptype_loc; _ } ->
      let ext =
        Location.error_extensionf
          ~loc:ptype_loc
          "Cannot derive accessors for non record types"
      in
      Ast_builder.Default.pstr_extension ~loc ext []
    | { ptype_kind = Ptype_record fields; _ } ->
      let cases =
        List.filter_map fields ~f:(fun field ->
          match field.pld_type.ptyp_desc with
          | Ptyp_tuple _ -> Some (make_cases ~loc field)
          | _ -> None)
        @ [ [ case
                ~lhs:(ppat_any ~loc)
                ~guard:None
                ~rhs:[%expr print_endline "No solution for this day-task combination"]
            ]
          ]
        |> List.concat
      in
      let m = make_run_aoc ~loc cases in
      m)
;;

(* implementation generator *)
let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let my_deriver = Deriving.add "aocmatch" ~str_type_decl:impl_generator
