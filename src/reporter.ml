let bar score width =
  let filled = min width (int_of_float (score *. float_of_int width +. 0.5)) in
  String.make filled '#' ^ String.make (width - filled) '-'

let pp_issue issue =
  Printf.printf "  ! line %3d: %s\n             -> %s\n"
    issue.Metric.line issue.Metric.message issue.Metric.suggestion

let pp_result r =
  Printf.printf "\n[%-20s] %.2f  [%s]\n" r.Scorer.name r.Scorer.score (bar r.Scorer.score 20);
  List.iter pp_issue r.Scorer.issues

let report filename results score =
  Printf.printf "=== OCaml Beauty: %s ===\n" filename;
  List.iter pp_result results;
  Printf.printf "\n%-22s %.2f  [%s]\n" "Overall" score (bar score 20)
