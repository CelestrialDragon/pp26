(* Session 5, block 1, part 1: the two loops of the slides "Three closures made in
   a loop", in OCaml.

     ocaml capture.ml        prints  [3; 3; 3]  then  [0; 1; 2]

   A closure keeps the variable that it uses, not a copy of the value that the
   variable had when the closure was made. What the three functions answer
   depends on how many variables the loop has: one, or one per round. *)

let show l =
  print_endline ("[" ^ String.concat "; " (List.map string_of_int l) ^ "]")

(* As JavaScript's var: ONE variable for the whole loop. In OCaml a variable that
   changes is a reference, so the sharing is visible in the code: the three
   closures hold the same i. We call them after the loop, when i holds 3. *)
let () =
  let fs = ref [] in
  let i = ref 0 in
  while !i < 3 do
    fs := (fun () -> !i) :: !fs;        (* !i is read when the function is called *)
    incr i
  done;
  show (List.rev_map (fun f -> f ()) !fs)

(* As JavaScript's let: for binds a NEW i at every round, and an i is never
   changed. Each closure holds its own i. This is the only thing an OCaml for can
   do: to get the first answer we had to ask for a reference. *)
let () =
  let fs = ref [] in
  for i = 0 to 2 do
    fs := (fun () -> i) :: !fs
  done;
  show (List.rev_map (fun f -> f ()) !fs)
