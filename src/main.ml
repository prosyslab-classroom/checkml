module F = Format
module TS = Tree_sitter_ocaml
module Loc = Tree_sitter_run.Loc

let read_file filename =
  if not (Sys.file_exists filename) then (
    prerr_endline ("checkml: file not found: " ^ filename);
    exit 1)
  else TS.Parse.file filename

let parse filename =
  let p = read_file filename in
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

and check_simple_exp_ext res : TS.CST.simple_expression_ext -> Error.t list =
  function
  | `Simple_exp simple_exp -> check_simple_exp res simple_exp
  | `Exte _ -> res

and check_simple_exp res : TS.CST.simple_expression -> Error.t list = function
  | `Value_path vp -> check_value_path res vp
  | `Cst _ -> res
  (* Ex) (fun x -> ...) *)
  | `Paren_exp paren_exp -> (
      match paren_exp with
      | `LPAR_seq_exp_ext_RPAR (_, seq_expr_ext, _) ->
          check_seq_exp_ext res seq_expr_ext
      | _ -> res)
  | _ -> res

(* Ex) foo param1 param2 *)
and check_app_exp res (f, args) =
  let res = check_simple_exp_ext res f in
  List.fold_left
    (fun res arg ->
      match arg with
      | `Choice_simple_exp simple_exp_ext ->
          check_simple_exp_ext res simple_exp_ext
      | _ -> res)
    res args

and check_exp_ext res = function `Exp exp -> check_exp res exp | _ -> res

and check_exp res : TS.CST.expression -> Error.t list = function
  | `Simple_exp simple_exp -> check_simple_exp res simple_exp
  | `Prod_exp _ -> res
  | `Cons_exp _ -> res
  | `App_exp app_exp -> check_app_exp res app_exp
  (* TODO: Go into the body of the while loop *)
  | `While_exp ((loc, _id), _, _seq_exp_ext, _do) -> Error.make_while loc :: res
  (* TODO: Go into the body of the for loop *)
  | `For_exp ((loc, _id), _, _, _, _seq_exp_ext1, _, _seq_exp_ext2, _do) ->
      Error.make_for loc :: res
  | `Let_exp (def, _in, seq_exp_ext) ->
      check_value_def res def |> Fun.flip check_seq_exp_ext seq_exp_ext
  | `If_exp if_exp -> check_if_exp res if_exp
  | `Match_exp match_exp -> check_match_exp res match_exp
  | `Func_exp func_exp -> check_func_exp res func_exp
  | `Fun_exp fun_exp -> check_fun_exp res fun_exp
  | `Try_exp try_exp -> check_try_exp res try_exp
  | _ -> res

and check_if_exp res if_exp =
  let _, _, seq_expr_ext, then_clause, else_clause_opt = if_exp in
  let res = check_seq_exp_ext res seq_expr_ext in
  let _, exp_ext = then_clause in
  let res = check_exp_ext res exp_ext in
  match else_clause_opt with
  | Some (_, exp_ext) -> check_exp_ext res exp_ext
  | None -> res

and check_match_exp res match_exp =
  let _, seq_expr_ext, _, cases = match_exp in
  let res = check_seq_exp_ext res seq_expr_ext in
  let _, case, more_cases = cases in
  let res = check_match_case res case in
  List.fold_left (fun res (_, case) -> check_match_case res case) res more_cases

and check_match_case res match_case =
  let _, _, _, maybe_sequence_exp_ext = match_case in
  match maybe_sequence_exp_ext with
  | `Seq_exp_ext seq_expr_ext -> check_seq_exp_ext res seq_expr_ext
  | _ -> res

and check_func_exp res func_exp =
  let _, _, cases = func_exp in
  let _, case, more_cases = cases in
  let res = check_match_case res case in
  List.fold_left (fun res (_, case) -> check_match_case res case) res more_cases

and check_fun_exp res fun_exp =
  let _, _, _, _, _, seq_expr_ext = fun_exp in
  check_seq_exp_ext res seq_expr_ext

and check_try_exp res try_exp =
  let _, _, seq_expr_ext, _, cases = try_exp in
  let res = check_seq_exp_ext res seq_expr_ext in
  let _, case, more_cases = cases in
  let res = check_match_case res case in
  List.fold_left (fun res (_, case) -> check_match_case res case) res more_cases

and check_seq_exp_ext res exp =
  match (exp : TS.CST.sequence_expression_ext) with
  | `Seq_exp_ (`Exp e) -> check_exp res e
  | `Seq_exp_ (`Seq_exp seq_exp) -> check_seq_exp res seq_exp
  | `Exte _ -> res

and check_seq_exp res seq_exp =
  let exp_ext, _, seq_exp_opt = seq_exp in
  let res = check_exp_ext res exp_ext in
  match seq_exp_opt with
  | Some (_, seq_exp_ext) -> check_seq_exp_ext res seq_exp_ext
  | None -> res

and check_let_binding_body res (_params, _, _, _, exp) =
  check_seq_exp_ext res exp

and check_let_binding res ((_pat_ext, body, _attr) : TS.CST.let_binding) =
  match body with Some body -> check_let_binding_body res body | None -> res

and check_value_def res (_let, binding, _and) = check_let_binding res binding

and check_simple_module_exp res simple_module_exp =
  match simple_module_exp with
  | `Typed_module_exp (_, module_expression_ext, _, _) ->
      check_module_exp_ext res module_expression_ext
  | `Paren_module_exp (_, module_expression_ext, _) ->
      check_module_exp_ext res module_expression_ext
  | `Packed_module (_, _, exp_ext, module_typed_option, _, _) ->
      let res = check_exp_ext res exp_ext in
      check_module_exp_ext res module_typed_option

and check_simple_module_exp_ext res simple_module_exp_ext =
  match simple_module_exp_ext with
  | `Simple_module_exp simple_module_expression ->
      check_simple_module_exp res simple_module_expression
  | `Exte _ -> res

and check_module_exp res module_exp =
  match module_exp with
  | `Stru_ (_, stru_opt, _) -> (
      match stru_opt with
      | Some structure -> check_structure res structure
      | None -> res)
  | `Func (_, _, _, module_expression_ext) ->
      check_module_exp_ext res module_expression_ext
  | `Module_app (module_expression_ext, _) ->
      check_module_exp_ext res module_expression_ext
  | _ -> res

and check_module_exp_ext res module_exp_ext =
  match module_exp_ext with
  | `Module_exp module_exp -> check_module_exp res module_exp
  | `Exte _ -> res

and check_module_binding res (_, _, _, ext_option, _) =
  match ext_option with
  | Some (_, module_exp_ext) -> check_module_exp_ext res module_exp_ext
  | None -> res

(* TODO: Handle modules. *)
and check_module_def res (_, _, _, module_binding, more_bindings) =
  let res = check_module_binding res module_binding in
  (List.fold_left (fun res (_, module_binding) ->
       check_module_binding res module_binding))
    res more_bindings

and check_structure_item res structure_item =
  match structure_item with
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
  | `Exp_item (seq_expr_ext, _) -> check_seq_exp_ext res seq_expr_ext

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
  match !Cmdline.file with
  | "" ->
      prerr_endline "checkml: no input file";
      exit 1
  | _ ->
      let m = parse !Cmdline.file in
      check m |> List.rev |> report

let _ = main ()
