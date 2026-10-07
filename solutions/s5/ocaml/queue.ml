(* Homework, lab 5 — lab 2's queue, with its reversal kept. SOLUTIONS: every hole filled.
   The exercise is stated on the warm-up sheet, under Homework; this file only
   says where to type.

     ocaml warmup/queue.ml         # pass / FAIL / todo, one line per check *)

let ok = ref 0 and bad = ref 0 and todo = ref 0
let check name f =
  match f () with
  | true -> incr ok; Printf.printf "  pass  %s\n%!" name
  | false -> incr bad; Printf.printf "  FAIL  %s\n%!" name
  | exception Failure msg when String.length msg >= 4 && String.sub msg 0 4 = "TODO" ->
      incr todo; Printf.printf "  todo  %s (%s)\n%!" name msg

let reversals = ref 0
let reverse (l : 'a list) : 'a list = incr reversals; List.rev l

(* lab 2's queue: the rear is reversed by pop, when the front is empty *)
type 'a queue = 'a list * 'a list

let push (x : 'a) ((front, rear) : 'a queue) : 'a queue = (front, x :: rear)

let pop (q : 'a queue) : ('a * 'a queue) option =
  match q with
  | x :: front, rear -> Some (x, (front, rear))
  | [], rear ->
      match reverse rear with
      | [] -> None
      | x :: front -> Some (x, (front, []))

(* the same queue, where the front is a lazy value: the reversal is put in the
   queue when the rear becomes longer than the front, and it is not run *)
type 'a lqueue = {
  front : 'a list Lazy.t;
  flen : int;
  rear : 'a list;
  rlen : int;
}

let empty : 'a lqueue = { front = lazy []; flen = 0; rear = []; rlen = 0 }

let balance (q : 'a lqueue) : 'a lqueue =
  if q.rlen <= q.flen then q
  else
    let front = q.front and rear = q.rear in
    { front = lazy (Lazy.force front @ reverse rear);
      flen = q.flen + q.rlen;
      rear = [];
      rlen = 0 }

let lpush (x : 'a) (q : 'a lqueue) : 'a lqueue =
  balance { q with rear = x :: q.rear; rlen = q.rlen + 1 }

let lpop (q : 'a lqueue) : ('a * 'a lqueue) option =
  match Lazy.force q.front with
  | [] -> None
  | x :: rest ->
      Some (x, balance { q with front = lazy rest; flen = q.flen - 1 })

let first = function Some (x, _) -> Some x | None -> None

let () =
  print_endline "lab 2's queue, with its reversal kept";
  let numbers = [1; 2; 3; 4; 5; 6; 7] in
  check "lab 2's queue, popped twice from the same version: 2 reversals" (fun () ->
    let q = List.fold_left (fun q x -> push x q) ([], []) numbers in
    reversals := 0;
    let a = pop q in
    let b = pop q in
    first a = Some 1 && first b = Some 1 && !reversals = 2);
  check "the lazy queue gives the elements in the order they came" (fun () ->
    let q = List.fold_left (fun q x -> lpush x q) empty numbers in
    let rec all q = match lpop q with None -> [] | Some (x, q') -> x :: all q' in
    all q = numbers);
  check "pushing runs no reversal" (fun () ->
    reversals := 0;
    let _ = List.fold_left (fun q x -> lpush x q) empty numbers in
    !reversals = 0);
  check "popped twice from the same version: the second pop runs no reversal" (fun () ->
    let q = List.fold_left (fun q x -> lpush x q) empty numbers in
    reversals := 0;
    let a = lpop q in
    let after_first = !reversals in
    let b = lpop q in
    first a = Some 1 && first b = Some 1 && after_first > 0 && !reversals = after_first)

let () =
  Printf.printf "\n%d pass, %d FAIL, %d todo\n" !ok !bad !todo
