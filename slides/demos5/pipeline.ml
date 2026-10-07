(* Block 2, part 2: how far a sequence is computed. The program of the slides
   "How far is a sequence computed?" and "take 5 squares, step by step".   ocaml demos/pipeline.ml *)
type 'a node = Nil | Cons of 'a * 'a seq
and 'a seq = unit -> 'a node

let produced = ref 0
let rec nats n : int seq = fun () -> incr produced; Cons (n, nats (n + 1))

let rec take n (s : 'a seq) =
  if n = 0 then []
  else match s () with
    | Nil -> []
    | Cons (x, rest) -> x :: take (n - 1) rest

(* map and filter return at once: their body is under fun () ->. When x does not
   pass, filter asks for the next element itself, with its last () *)
let rec map f (s : 'a seq) : 'b seq =
  fun () -> match s () with
    | Nil -> Nil
    | Cons (x, rest) -> Cons (f x, map f rest)

let rec filter p (s : 'a seq) : 'a seq =
  fun () -> match s () with
    | Nil -> Nil
    | Cons (x, rest) -> if p x then Cons (x, filter p rest) else filter p rest ()

(* The pipeline of the slide. Making it computes nothing: produced=0. take 5 asks
   for five squares, and nats produces nine numbers, 0 to 8: five of them are even. *)
let even n = n mod 2 = 0
let square n = n * n

let squares = map square (filter even (nats 0))
let () = Printf.printf "produced=%d\n" !produced

let five = take 5 squares
let () =
  Printf.printf "[%s]   produced=%d\n" (String.concat "; " (List.map string_of_int five)) !produced
