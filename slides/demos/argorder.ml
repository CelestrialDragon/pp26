(* Session 4, block 1: in which order are the arguments of a call evaluated?

     ocaml -noprompt < argorder.ml     the toplevel prints each phrase's value:
                                       second first - : int = 30

   Each argument prints its name before it gives its value, so the output shows
   the order. OCaml evaluates them from right to left here; its manual says that
   the order is unspecified. A program whose result depends on it is wrong. *)
let f a b = a + b;;
f (print_string "first "; 10) (print_string "second "; 20);;
