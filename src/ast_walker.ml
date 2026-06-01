let get_line loc = loc.Location.loc_start.Lexing.pos_lnum

let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf filename;
  let ast =
    try Parse.implementation lexbuf
    with exn ->
      close_in ic;
      raise exn
  in
  close_in ic;
  ast
