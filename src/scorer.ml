type result = {
  name : string;
  weight : float;
  score : float;
  issues : Metric.issue list;
}

let run metrics ast =
  List.map
    (fun (module M : Metric.METRIC) ->
      let score, issues = M.analyze ast in
      { name = M.name; weight = M.weight; score; issues })
    metrics

let total results =
  List.fold_left (fun acc r -> acc +. (r.weight *. r.score)) 0.0 results
