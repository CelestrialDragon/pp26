(* Block 2, part 3: the Fibonacci numbers as a sequence whose rest is a lazy value.
   ocaml demos/fibs_lazy.ml *)

type 'a node =
  | Nil
  | Cons of 'a * 'a seq
and 'a seq = 'a node Lazy.t

let additions = ref 0
let ( + ) a b = incr additions; Stdlib.( + ) a b

let rec zip_with f (a : 'a seq) (b : 'b seq) : 'c seq =
  lazy (match Lazy.force a, Lazy.force b with
    | Cons (x, a'), Cons (y, b') -> Cons (f x y, zip_with f a' b')
    | _ -> Nil)

let tail (s : 'a seq) : 'a seq =
  lazy (match Lazy.force s with
    | Nil -> Nil
    | Cons (_, rest) -> Lazy.force rest)

let rec fibs =
  lazy (Cons (0, lazy (Cons (1, zip_with ( + ) fibs (tail fibs)))))

let rec take n (s : 'a seq) =
  if n = 0 then []
  else match Lazy.force s with
    | Nil -> []
    | Cons (x, rest) -> x :: take (Stdlib.( - ) n 1) rest

let () =
  let l = take 25 fibs in
  print_endline (String.concat " " (List.map string_of_int (List.filteri (fun i _ -> i < 10) l)));
  Printf.printf "first 25: last=%d additions=%d\n" (List.nth l 24) !additions
