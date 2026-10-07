(* Block 2: the list of all the natural numbers, and take. The same two definitions are in
   demos/Take.hs, line for line.   ocaml demos/take.ml   ends on a stack overflow *)
let rec from n = n :: from (n + 1)

let rec take n l =
  if n = 0 then []
  else match l with
    | [] -> []
    | x :: rest -> x :: take (n - 1) rest

let () =
  List.iter (Printf.printf "%d ") (take 5 [10; 20; 30; 40; 50; 60; 70]);
  print_newline ()

let five = take 5 (from 0)
