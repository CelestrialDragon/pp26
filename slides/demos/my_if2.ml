(* Session 4, block 1: can if be a function? The branches as functions.

     ocaml -noprompt < my_if2.ml       - : int = 120

   Each branch is now fun () -> ...: a function whose body is not run when the
   function is made. my_if calls only the branch that it chose, with (); the
   other one is never run. Session 5 names such a function a thunk. *)
let my_if c t e = if c then t () else e ();;
let rec fact n = my_if (n = 0) (fun () -> 1) (fun () -> n * fact (n - 1));;
fact 5;;
