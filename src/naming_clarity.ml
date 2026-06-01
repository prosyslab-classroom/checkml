open Parsetree
open Ast_iterator

let name = "Naming Clarity"
let weight = 0.20

(* camelCase: length > 1 and has an uppercase letter after the first char *)
let is_camel_case s =
  let n = String.length s in
  if n <= 1 then false
  else
    String.fold_left
      (fun (i, found) c -> (i + 1, found || (i > 0 && c >= 'A' && c <= 'Z')))
      (0, false) s
    |> snd

(* Collects names from let-binding patterns, skipping _ and operators *)
let rec names_of_pattern pat =
  match pat.ppat_desc with
  | Ppat_var { txt; _ } -> [ (txt, pat.ppat_loc) ]
  | Ppat_tuple pats -> List.concat_map names_of_pattern pats
  | Ppat_constraint (p, _) -> names_of_pattern p
  | _ -> []

let analyze ast =
  let issues = ref [] in
  let total = ref 0 in
  let bad = ref 0 in
  let add line msg sug =
    issues := { Metric.line; message = msg; suggestion = sug } :: !issues;
    incr bad
  in
  let iter =
    {
      default_iterator with
      value_binding =
        (fun self vb ->
          let names = names_of_pattern vb.pvb_pat in
          List.iter
            (fun (n, loc) ->
              incr total;
              let line = Ast_walker.get_line loc in
              if String.length n = 1 && n <> "_" then
                add line
                  (Printf.sprintf "single-char binding '%s'" n)
                  "use a descriptive name"
              else if is_camel_case n then
                add line
                  (Printf.sprintf "camelCase binding '%s'" n)
                  "use snake_case per OCaml convention")
            names;
          default_iterator.value_binding self vb);
    }
  in
  iter.structure iter ast;
  let ratio =
    if !total = 0 then 0.0 else float_of_int !bad /. float_of_int !total
  in
  let score = max 0.0 (1.0 -. ratio) in
  let sorted =
    List.sort (fun a b -> compare a.Metric.line b.Metric.line) !issues
  in
  (score, sorted)
