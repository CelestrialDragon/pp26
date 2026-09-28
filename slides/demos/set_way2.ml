(* Way 2: a set is its operations. The type lists what a set answers; the kinds live in the code.
   ocaml set_way2.ml *)

type set = < contains : int -> bool; is_empty : bool >

let empty : set = object
  method contains _ = false
  method is_empty = true
end

let insert (s : set) n : set = object
  method contains x = x = n || s#contains x
  method is_empty = false
end

let evens : set = object
  method contains x = x mod 2 = 0
  method is_empty = false
end

let union (s : set) (t : set) : set = object
  method contains x = s#contains x || t#contains x
  method is_empty = s#is_empty && t#is_empty
end

(* let equal (s : set) (t : set) = ???
   s and t can only be called. No number of calls decides whether they agree on every int. *)

let () =
  let s = insert (insert (insert empty 3) 5) 3 in                (* 3 inserted twice *)
  let u = union s evens in
  Printf.printf "%b %b %b\n" empty#is_empty s#is_empty (union empty empty)#is_empty;
  List.iter (fun x -> Printf.printf "%b " (s#contains x)) [3; 4; 5; 6]; print_newline ();
  List.iter (fun x -> Printf.printf "%b " (u#contains x)) [3; 4; 5; 6]; print_newline ()
