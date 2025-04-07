open Parsetree
open Ast_iterator
open Asttypes
open Longident

type target = Ref | While | For

module ExprSet = struct
  include Set.Make (struct
    type t = target * Location.t

    let compare = compare
  end)

  let pp_sep fmt () = Format.fprintf fmt "\n"
  let pp_print_list pp_v = Format.pp_print_list ~pp_sep pp_v

  let pp_expr fmt (a, loc) =
    Format.fprintf fmt "%a: %s" Location.print_loc loc
      (match a with Ref -> "ref" | While -> "while" | For -> "for")

  let pp fmt s = Format.fprintf fmt "%a\n" (pp_print_list pp_expr) (elements s)
end

let parse_ocaml_file filename =
  let ic = open_in filename in
  let lb = Lexing.from_channel ic in
  Location.init lb filename;
  let ast = Parse.implementation lb in
  close_in ic;
  ast

let detect_ref_and_while ast =
  let found = ref ExprSet.empty in

  let iter : iterator =
    {
      default_iterator with
      expr =
        (fun this expr ->
          (match expr.pexp_desc with
          | Pexp_apply
              ({ pexp_desc = Pexp_ident { txt = Lident "ref"; _ }; _ }, _) ->
              found := ExprSet.add (Ref, expr.pexp_loc) !found
          | Pexp_while _ -> found := ExprSet.add (While, expr.pexp_loc) !found
          | Pexp_for _ -> found := ExprSet.add (For, expr.pexp_loc) !found
          | _ -> ());
          default_iterator.expr this expr);
    }
  in

  iter.structure iter ast;
  !found

let () =
  let files = Array.to_list Sys.argv |> List.tl in
  let alarms =
    List.fold_left
      (fun acc filename ->
        filename |> parse_ocaml_file |> detect_ref_and_while
        |> ExprSet.union acc)
      ExprSet.empty files
  in
  if ExprSet.is_empty alarms then exit 0
  else (
    Format.printf "%a" ExprSet.pp alarms;
    exit 1)
