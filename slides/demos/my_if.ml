(* Session 4, block 1: can if be a function? A first try.

     ocaml -noprompt < my_if.ml        Stack overflow during evaluation (looping recursion?).

   Under call-by-value, the arguments of my_if are computed before my_if is
   called: the else branch too, fact (n - 1), even when n = 0. So fact 0 calls
   fact (-1), which calls fact (-2), and so on until the stack is full. *)
let my_if c t e = if c then t else e;;
let rec fact n = my_if (n = 0) 1 (n * fact (n - 1));;
fact 5;;
