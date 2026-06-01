open Parsetree
open Ast_iterator

let name = "Structural Depth"
let weight = 0.15

(* Measures branching depth: match/if nesting within a single function body *)
let rec branching_depth expr =
  match expr.pexp_desc with
  | Pexp_ifthenelse (_, t, Some f) ->
      1 + max (branching_depth t) (branching_depth f)
  | Pexp_ifthenelse (_, t, None) -> 1 + branching_depth t
  | Pexp_match (_, cases) | Pexp_try (_, cases) ->
      1
      + List.fold_left (fun acc c -> max acc (branching_depth c.pc_rhs)) 0 cases
  | Pexp_function (_, _, Pfunction_cases (cases, _, _)) ->
      1
      + List.fold_left (fun acc c -> max acc (branching_depth c.pc_rhs)) 0 cases
  | Pexp_function (_, _, Pfunction_body e) -> branching_depth e
  | Pexp_let (_, _, body) | Pexp_letmodule (_, _, body) | Pexp_sequence (_, body)
    ->
      branching_depth body
  | _ -> 0

let analyze ast =
  let issues = ref [] in
  let penalty = ref 0.0 in
  let add line msg sug p =
    issues := { Metric.line; message = msg; suggestion = sug } :: !issues;
    penalty := !penalty +. p
  in
  let iter =
    {
      default_iterator with
      value_binding =
        (fun self vb ->
          let depth = branching_depth vb.pvb_expr in
          if depth >= 4 then
            add
              (Ast_walker.get_line vb.pvb_loc)
              (Printf.sprintf "branching depth %d" depth)
              "break into smaller functions"
              (0.1 *. float_of_int (depth - 3));
          default_iterator.value_binding self vb);
    }
  in
  iter.structure iter ast;
  let score = max 0.0 (1.0 -. !penalty) in
  let sorted =
    List.sort (fun a b -> compare a.Metric.line b.Metric.line) !issues
  in
  (score, sorted)
