(* Good OCaml: functional style *)

let sum_list lst = List.fold_left ( + ) 0 lst
let square_all lst = List.map (fun x -> x * x) lst
let filter_positive lst = List.filter (fun n -> n > 0) lst
let process numbers = numbers |> filter_positive |> square_all |> sum_list

type shape = Circle of float | Rect of float * float

let area = function Circle r -> Float.pi *. r *. r | Rect (w, h) -> w *. h
