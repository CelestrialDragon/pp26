(* Session 5, block 2: thunks and sequences. The programs of six slides, in the
   order of the deck.

     ocaml stream.ml

   Each part below starts with a heading that names its slide. *)

(* prints a list of numbers as OCaml writes it *)
let show l = "[" ^ String.concat "; " (List.map string_of_int l) ^ "]"

(* ---- A thunk, called twice ---------------------------------------------- *)

(* The body of t is not run when t is made: "the thunk is made" is printed
   first. It is run at every call: "computing" is printed two times. *)
let () =
  let t = fun () -> print_endline "computing"; 6 * 7 in
  print_endline "the thunk is made";
  let a = t () in
  let b = t () in
  Printf.printf "%d %d\n" a b

(* ---- nats and take ------------------------------------------------------ *)

(* A sequence: its rest is a thunk, not yet run. *)
type 'a node =
  | Nil
  | Cons of 'a * 'a seq
and 'a seq = unit -> 'a node

(* produced counts the cells that nats builds: one each time its thunk is called *)
let produced = ref 0

let rec nats n : int seq =
  fun () -> incr produced; Cons (n, nats (n + 1))

(* take calls the sequence n times: n cells are built, and no more *)
let rec take n (c : 'a seq) =
  if n = 0 then []
  else match c () with
    | Nil -> []
    | Cons (x, rest) -> x :: take (n - 1) rest

let () =
  produced := 0;
  let l = take 5 (nats 0) in
  Printf.printf "take 5 (nats 0) = %s   produced=%d\n" (show l) !produced

(* ---- map and filter on a sequence --------------------------------------- *)

(* Both return at once: their body is under fun () ->. filter asks for the next
   element itself when x does not pass. *)
let rec map f (c : 'a seq) : 'b seq =
  fun () -> match c () with
    | Nil -> Nil
    | Cons (x, rest) -> Cons (f x, map f rest)

let rec filter p (c : 'a seq) : 'a seq =
  fun () -> match c () with
    | Nil -> Nil
    | Cons (x, rest) -> if p x then Cons (x, filter p rest) else filter p rest ()

(* Making the sequence computes nothing: produced=0. take 5 asks for five
   squares: nine numbers are produced, 0 to 8, of which five are even. *)
let () =
  produced := 0;
  let evens = filter (fun n -> n mod 2 = 0) (nats 0) in
  let squares = map (fun n -> n * n) evens in
  Printf.printf "made, nothing asked: produced=%d\n" !produced;
  let l = take 5 squares in
  Printf.printf "take 5 squares = %s   produced=%d\n" (show l) !produced

(* ---- Stopping, by not asking -------------------------------------------- *)

(* exists asks for the next element only when it needs one. When p x is true,
   || does not compute its right side: the rest is never asked for. *)
let rec exists p (c : 'a seq) =
  match c () with
  | Nil -> false
  | Cons (x, rest) -> p x || exists p rest

let () =
  produced := 0;
  let b = exists (fun n -> n > 2) (nats 0) in
  Printf.printf "exists (n > 2) (nats 0) = %b   produced=%d\n" b !produced

let rec of_list l : 'a seq =
  fun () -> match l with [] -> Nil | x :: xs -> Cons (x, of_list xs)

(* The text of block 1, as a sequence of characters that counts the elements
   asked for: the newline is found at the third, and y and o are never asked. *)
let () =
  let asked = ref 0 in
  let rec chars l : char seq =
    fun () -> match l with [] -> Nil | x :: xs -> incr asked; Cons (x, chars xs) in
  let found = exists (fun c -> c = '\n') (chars ['h'; 'i'; '\n'; 'y'; 'o']) in
  Printf.printf "found=%b asked=%d\n" found !asked

(* ---- The library: Seq --------------------------------------------------- *)

(* The same pipeline with OCaml's own sequences: Seq.t is the type we wrote,
   Seq.ints 0 is our nats 0. *)
let () =
  let nats = Seq.ints 0 in
  let l = List.of_seq (Seq.take 5 (Seq.map (fun n -> n * n) (Seq.filter (fun n -> n mod 2 = 0) nats))) in
  Printf.printf "Seq: %s\n" (show l)

(* ---- A thunk and a lazy value, each asked twice ------------------------- *)

(* ticks counts the runs of the body. A thunk runs its body at every call:
   ticks=2. A lazy value runs it at the first force and keeps the result:
   ticks=1. *)
let () =
  let ticks = ref 0 in
  let t = fun () -> incr ticks; 6 * 7 in
  let a = t () and b = t () in
  Printf.printf "thunk: %d %d ticks=%d\n" a b !ticks;
  let ticks = ref 0 in
  let l = lazy (incr ticks; 6 * 7) in
  let a = Lazy.force l and b = Lazy.force l in
  Printf.printf "lazy:  %d %d ticks=%d\n" a b !ticks

(* ---- fibs: how many additions? ------------------------------------------ *)

(* The Fibonacci numbers, with thunks and with lazy values, the additions
   counted by plus. With thunks, fibs and tail fibs compute the same numbers
   again: the count grows as fast as the numbers. With lazy values each number
   is computed once: n - 2 additions for the first n numbers. *)
let adds = ref 0
let plus a b = incr adds; a + b

let rec zip_with f (a : 'a seq) (b : 'b seq) : 'c seq =
  fun () -> match a (), b () with
    | Cons (x, a'), Cons (y, b') -> Cons (f x y, zip_with f a' b')
    | _ -> Nil

let tail (c : 'a seq) : 'a seq =
  fun () -> match c () with Nil -> Nil | Cons (_, rest) -> rest ()

let rec fibs : int seq =
  fun () -> Cons (0, fun () -> Cons (1, zip_with plus fibs (tail fibs)))

(* the same, where the rest is a lazy value: computed once, then kept *)
type 'a lnode = LNil | LCons of 'a * 'a lstream
and 'a lstream = 'a lnode Lazy.t

let rec ltake n (s : 'a lstream) =
  if n = 0 then []
  else match Lazy.force s with
    | LNil -> []
    | LCons (x, rest) -> x :: ltake (n - 1) rest

let rec lzip_with f (a : 'a lstream) (b : 'b lstream) : 'c lstream =
  lazy (match Lazy.force a, Lazy.force b with
    | LCons (x, a'), LCons (y, b') -> LCons (f x y, lzip_with f a' b')
    | _ -> LNil)

let ltail (s : 'a lstream) : 'a lstream =
  lazy (match Lazy.force s with LNil -> LNil | LCons (_, rest) -> Lazy.force rest)

(* a function, so that every measure starts from a stream nobody has forced *)
let make_lfibs () : int lstream =
  let rec lfibs =
    lazy (LCons (0, lazy (LCons (1, lzip_with plus lfibs (ltail lfibs))))) in
  lfibs

let () =
  List.iter (fun n ->
    adds := 0;
    let l = take n fibs in
    let a1 = !adds in
    adds := 0;
    let l' = ltake n (make_lfibs ()) in
    assert (l = l');
    Printf.printf "fibs, first %d: last=%d   thunks: %d additions   lazy: %d additions\n"
      n (List.nth l (n - 1)) a1 !adds) [10; 20; 25]
