(* Session 5, block 2: what a thunk gives, and what lazy adds. The programs of
   four slides, in the order of the deck.

     ocaml delay.ml *)

(* ---- exists, with the right fold ---------------------------------------- *)

(* Session 4's right fold: to call f on x, OCaml first computes the fold of the
   rest, its second argument. *)
let rec fold_right f xs z = match xs with
  | [] -> z
  | x :: xs -> f x (fold_right f xs z)

let text = ['h'; 'i'; '\n'; 'y'; 'o']
let visited = ref 0

(* Block 1's exists, as a right fold. Our function is given the rest, and too
   late: it has been computed already. The five characters are visited. *)
let exists p l =
  fold_right (fun x rest -> incr visited; p x || rest) l false

let () =
  let b = exists (fun c -> c = '\n') text in
  Printf.printf "fold_right:            found=%b visited=%d\n" b !visited

(* ---- The right fold, with the rest as a thunk --------------------------- *)

(* The same fold, where f receives the rest as a thunk: fun () -> ... is not
   run before f calls it. On the slide the two are named fold_right and exists;
   here they need names of their own. *)
let rec fold_right_d f xs z = match xs with
  | [] -> z
  | x :: xs -> f x (fun () -> fold_right_d f xs z)

(* At the newline p x is true, || does not compute its right side, and the rest
   is never run: three characters are visited. *)
let exists_d p l =
  fold_right_d (fun x rest -> incr visited; p x || rest ()) l false

let () =
  visited := 0;
  let b = exists_d (fun c -> c = '\n') text in
  Printf.printf "with the rest delayed: found=%b visited=%d\n" b !visited

(* ---- lazy, made by hand ------------------------------------------------- *)

(* A lazy value is a reference that holds a thunk, and after the first force
   the value. force runs the thunk once, and puts the value in its place. *)
type 'a state =
  | Delayed of (unit -> 'a)
  | Value of 'a

let delay f = ref (Delayed f)

let force s = match !s with
  | Value v -> v
  | Delayed f ->
      let v = f () in
      s := Value v;
      v

(* forced two times, the body runs once: ticks=1 *)
let () =
  let ticks = ref 0 in
  let s = delay (fun () -> incr ticks; 6 * 7) in
  let a = force s in
  let b = force s in
  Printf.printf "by hand: %d %d ticks=%d\n" a b !ticks

(* ---- Lab 2's queue, used twice ------------------------------------------ *)

(* Lab 2's queue: a front and a rear; the rear is reversed when the front is
   empty. rev counts the reversals. *)
let reversals = ref 0
let rev l = incr reversals; List.rev l

let push x (front, rear) = (front, x :: rear)

let pop = function
  | x :: front, rear -> Some (x, (front, rear))
  | [], rear ->
      match rev rear with
      | [] -> None
      | x :: front -> Some (x, (front, []))

(* q is not changed by the first pop, so the second one finds the same empty
   front and reverses the same rear again: reversals=2 *)
let () =
  let q = List.fold_left (fun q x -> push x q) ([], []) [1; 2; 3; 4; 5; 6; 7; 8] in
  let a = pop q in
  let b = pop q in
  let first = function Some (x, _) -> x | None -> 0 in
  Printf.printf "pop q twice: %d %d reversals=%d\n" (first a) (first b) !reversals

(* The reversal suspended in a lazy value, which the two pops share: the first
   one runs it, the second one finds the list. reversals=1 *)
let () =
  reversals := 0;
  let rear = [8; 7; 6; 5; 4; 3; 2; 1] in
  let q = lazy (rev rear) in
  let pop_l q = match Lazy.force q with [] -> None | x :: _ -> Some x in
  let first = function Some x -> x | None -> 0 in
  let a = pop_l q in
  let b = pop_l q in
  Printf.printf "with lazy:   %d %d reversals=%d\n" (first a) (first b) !reversals
