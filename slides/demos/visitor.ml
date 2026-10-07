(* The visitor as a record of functions: one field per constructor, the arms of a match. *)
type set = Empty | Insert of int * set
type 'r visitor = { on_empty : 'r; on_insert : int -> set -> 'r }
let accept v = function
  | Empty -> v.on_empty
  | Insert (n, s) -> v.on_insert n s
let is_empty s = accept { on_empty = true; on_insert = (fun _ _ -> false) } s
let () = Printf.printf "%b %b\n" (is_empty Empty) (is_empty (Insert (5, Insert (3, Empty))))
