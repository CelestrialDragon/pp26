(* Session 5, block 2, appendix: memoising a function. The programs of four
   slides, in the order of the deck.

     ocaml memo.ml

   The answers to the appendix's two exercises are in memo_answers.ml. *)

(* runs counts the runs of a function's body *)
let runs = ref 0

(* ---- fib 25, without memoisation ---------------------------------------- *)

let rec fib n =
  incr runs;
  if n < 2 then n else fib (n - 1) + fib (n - 2)

(* the problem: a function keeps nothing from one call to the next, and a lazy
   value keeps one result, for one argument *)
let () =
  runs := 0; let a = fib 25 in let b = fib 25 in
  Printf.printf "a=%d b=%d   the body ran %d times\n" a b !runs;
  runs := 0; let v = lazy (fib 25) in
  let a = Lazy.force v in let b = Lazy.force v in let r = !runs in
  let c = fib 24 in
  Printf.printf "lazy (fib 25), forced twice: %d %d, %d runs; then fib 24 = %d, %d more\n" a b r c (!runs - r)

(* ---- memo: a function that keeps a table ---------------------------------- *)

(* Order 2: memo takes a function and returns a function. The table belongs to
   the function returned: nobody else can reach it. *)
let memo (f : 'a -> 'b) : 'a -> 'b =
  let table = Hashtbl.create 16 in
  fun x ->
    match Hashtbl.find_opt table x with
    | Some y -> y
    | None ->
        let y = f x in
        Hashtbl.add table x y;
        y

(* ---- memo fib: the calls that the table sees ------------------------------ *)

(* The first call of mfib 25 runs the body 242 785 times: the table sees only
   that first call, since fib calls fib, not mfib. The second call finds 25 in
   the table: 0 runs. *)
let () =
  let mfib = memo fib in
  runs := 0; let a = mfib 25 in let r1 = !runs in
  runs := 0; let b = mfib 25 in
  Printf.printf "memo fib 25 = %d, the body ran %d times; again = %d, %d times\n" a r1 b !runs

(* ---- memo_rec: the function receives the function to call ------------------ *)

(* Order 3: memo_rec takes a function that takes a function. fib' calls self,
   and memo_rec gives it g, which looks in the table first: every argument from
   0 to 25 is computed once, 26 runs. *)
let memo_rec (f : ('a -> 'b) -> 'a -> 'b) : 'a -> 'b =
  let table = Hashtbl.create 16 in
  let rec g x =
    match Hashtbl.find_opt table x with
    | Some y -> y
    | None ->
        let y = f g x in
        Hashtbl.add table x y;
        y
  in
  g

let fib' self n =
  incr runs;
  if n < 2 then n else self (n - 1) + self (n - 2)

let () =
  let mfib = memo_rec fib' in
  runs := 0; let a = mfib 25 in
  Printf.printf "memo_rec fib' 25 = %d, the body ran %d times\n" a !runs
