open Parsetree
open Ast_iterator

let name = "Functional Purity"
let weight = 0.35

(* Detects: ref creation, := assignment, while, for loops *)
let analyze ast =
  let issues = ref [] in
  let count = ref 0 in
  let add line msg sug =
    issues := { Metric.line; message = msg; suggestion = sug } :: !issues;
    incr count
  in
  let iter = { default_iterator with
    expr = fun self e ->
      (match e.pexp_desc with
      | Pexp_while _ ->
        add (Ast_walker.get_line e.pexp_loc)
          "while loop"
          "consider recursion or List.fold"
      | Pexp_for _ ->
        add (Ast_walker.get_line e.pexp_loc)
          "for loop"
          "consider List.init or recursion"
      | Pexp_apply
          ({ pexp_desc = Pexp_ident { txt = Longident.Lident ":="; _ }; _ }, _) ->
        add (Ast_walker.get_line e.pexp_loc)
          "mutable assignment (:=)"
          "consider returning a new value instead"
      | Pexp_apply
          ({ pexp_desc = Pexp_ident { txt = Longident.Lident "ref"; _ }; _ }, _) ->
        add (Ast_walker.get_line e.pexp_loc)
          "mutable reference (ref)"
          "consider passing values as function arguments"
      | _ -> ());
      default_iterator.expr self e
  } in
  iter.structure iter ast;
  let score = max 0.0 (1.0 -. float_of_int !count *. 0.15) in
  let sorted = List.sort (fun a b -> compare a.Metric.line b.Metric.line) !issues in
  (score, sorted)
