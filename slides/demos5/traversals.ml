(* Session 5, block 1, part 2: two usual traversals of a list.

     ocaml traversals.ml     prints  to be or not  two times

   Both visit every element, in order. They differ in who writes the loop. *)

let words = ["to"; "be"; "or"; "not"]

(* 1. We give the list a function. The loop is the library's: List.iter calls our
      function on every element, and we do not see the recursion. *)
let () = List.iter (fun w -> print_string (w ^ " ")) words
let () = print_newline ()

(* 2. We write the recursion ourselves: what to do on the empty list, and what to
      do with the first element and the rest. Here we could stop before the end.
      Can the function that we give to List.iter do it? Part 3 asks. *)
let rec print = function
  | [] -> ()
  | w :: rest -> print_string (w ^ " "); print rest

let () = print words
let () = print_newline ()
