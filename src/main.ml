module F = Format
module TS = Tree_sitter_ocaml
module Loc = Tree_sitter_run.Loc

let parse filename =
  let p = TS.Parse.file filename in
  match p.program with
  | None ->
      prerr_endline "checkml: parse error";
      exit 1
  | Some m -> m

module Error = struct
  type t = Ref of Loc.t | While of Loc.t | For of Loc.t

  let make_ref loc = Ref loc
  let make_while loc = While loc
  let make_for loc = For loc
  let name = function Ref _ -> "ref" | While _ -> "while" | For _ -> "for"
  let loc = function Ref l | While l | For l -> l

  let pp fmt e =
    let loc = loc e in
    F.fprintf fmt "File %s, line %d, column %d\n" !Cmdline.file
      (loc.Loc.start.row + 1) loc.Loc.start.column;
    F.fprintf fmt "\027[31mError\027[0m: Don't use %s!\n\n" (name e)
end

let report lst =
  List.iter (fun item -> F.printf "%a" Error.pp item) lst;
  if lst = [] then exit 0 else exit 1

let rec check m =
  match m with
  | `Opt_sheb_opt_stru (_, Some structure) -> check_structure [] structure
  | _ -> []

and check_value_name res = function
  | `Id (loc, id) -> if id = "ref" then Error.make_ref loc :: res else res
  | `Paren_op _ -> res

and check_value_path res = function
  | `Value_name n -> check_value_name res n
  | `Module_path_DOT_value_name _ -> res

and check_simple_expression res : TS.CST.simple_expression -> Error.t list =
  function
  | `Value_path vp -> check_value_path res vp
  | `Cst _ -> res
  | _ -> res

and check_simple_expression_ext res :
    TS.CST.simple_expression_ext -> Error.t list = function
  | `Simple_exp simple_expression ->
      check_simple_expression res simple_expression
  | `Exte _ -> res

and check_app_exp res (f, _args) = check_simple_expression_ext res f

and check_exp res : TS.CST.expression -> Error.t list = function
  | `Simple_exp simple_expression ->
      check_simple_expression res simple_expression
  | `Prod_exp _ -> res
  | `Cons_exp _ -> res
  | `App_exp app_exp -> check_app_exp res app_exp
  | _ -> res

and check_let_binding_body res (_params, _, _, _, exp) =
  match (exp : TS.CST.sequence_expression_ext) with
  | `Seq_exp_ (`Exp e) -> check_exp res e
  | `Exte _ -> res
  | _ -> res

and check_let_binding res ((_pat_ext, body, _attr) : TS.CST.let_binding) =
  match body with Some body -> check_let_binding_body res body | None -> res

and check_value_def res (_let, binding, _and) = check_let_binding res binding
and check_module_def res _ = res

and check_structure_item res : TS.CST.structure_item -> Error.t list = function
  | `Value_defi v -> check_value_def res v
  | `Module_defi m -> check_module_def res m
  | _ -> res

and check_structure_item_ext res : TS.CST.structure_item_ext -> Error.t list =
  function
  | `Stru_item s -> check_structure_item res s
  | `Item_exte _ -> res

and check_def_or_exp res = function
  | `Stru_item_ext s -> check_structure_item_ext res s
  | `Topl_dire _ -> res
  | `Exp_item _ -> res

and check_def_or_exp_rest_elem res = function
  | `Rep_SEMISEMI_choice_stru_item_ext (_, `Stru_item_ext structure_item_ext) ->
      check_structure_item_ext res structure_item_ext
  | `Rep_SEMISEMI_choice_stru_item_ext (_, `Topl_dire _toplevel_directive) ->
      res
  | `Rep1_SEMISEMI_exp_item (_, _expression_item) -> res

and check_def_or_exp_rest res l =
  List.fold_left check_def_or_exp_rest_elem res l

and check_structure res = function
  | `Rep1_SEMISEMI _ -> res
  | `Rep_SEMISEMI_choice_stru_item_ext_rep_choice_rep_SEMISEMI_choice_stru_item_ext_rep_SEMISEMI
      (_, e1, e2, _) ->
      check_def_or_exp res e1 |> Fun.flip check_def_or_exp_rest e2

let main () =
  Arg.parse Cmdline.options (fun x -> Cmdline.file := x) "Usage";
  let m = parse !Cmdline.file in
  check m |> List.rev |> report

let _ = main ()
