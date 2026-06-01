open Parsetree
open Ast_iterator

let name = "Pattern Quality"
let weight = 0.30

(* Measures depth of nested if-then-else chains *)
let rec if_depth expr =
  match expr.pexp_desc with
  | Pexp_ifthenelse (_, t, Some f) -> 1 + max (if_depth t) (if_depth f)
  | Pexp_ifthenelse (_, t, None) -> 1 + if_depth t
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
      expr =
        (fun self e ->
          (match e.pexp_desc with
          | Pexp_ifthenelse _ ->
              let depth = if_depth e in
              if depth >= 3 then
                add
                  (Ast_walker.get_line e.pexp_loc)
                  (Printf.sprintf "nested if-then-else (depth %d)" depth)
                  "consider using match"
                  (0.1 *. float_of_int (depth - 2))
          | _ -> ());
          default_iterator.expr self e);
    }
  in
  iter.structure iter ast;
  let score = max 0.0 (1.0 -. !penalty) in
  let sorted =
    List.sort (fun a b -> compare a.Metric.line b.Metric.line) !issues
  in
  (score, sorted)
