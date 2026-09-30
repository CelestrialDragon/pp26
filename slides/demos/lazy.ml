(* Session 4, block 2: call-by-need, by hand in OCaml.

     ocaml -noprompt < lazy.ml         once - : int = 42
                                       - : int = 42

   lazy e does not compute e: it makes a value that will. The first
   Lazy.force computes it, prints "once", and keeps 42; the second one
   finds 42 and computes nothing. *)
let x = lazy (print_string "once "; 6 * 7);;
Lazy.force x;;
Lazy.force x;;
