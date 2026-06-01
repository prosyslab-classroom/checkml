(* Bad OCaml: imperative style *)

let sumList lst =
  let total = ref 0 in
  let current = ref lst in
  while !current <> [] do
    total := !total + List.hd !current;
    current := List.tl !current
  done;
  !total

let squareAll lst =
  let n = List.length lst in
  let result = ref [] in
  for i = 0 to n - 1 do
    let x = List.nth lst i in
    result := (x * x) :: !result
  done;
  List.rev !result

let classify x =
  if x > 100 then
    if x > 1000 then if x > 10000 then "huge" else "large" else "medium"
  else "small"
