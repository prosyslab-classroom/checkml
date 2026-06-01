type issue = { line : int; message : string; suggestion : string }

module type METRIC = sig
  val name : string
  val weight : float

  (* returns (score in [0,1], list of issues found) *)
  val analyze : Parsetree.structure -> float * issue list
end
