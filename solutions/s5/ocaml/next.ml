(* Lab 5 — what comes next. SOLUTIONS: every hole filled.
   Part C goes with the first block of session 6: we hand over the rest of the computation. *)

open Rope

(* ---- C1. iter, with the rest of the traversal as a function ------------- *)

(* f receives a leaf and k, the function that goes on with the traversal;
   the last argument is what to do when the last leaf has been visited *)
let rec iter_k (f : 'a -> (unit -> 'r) -> 'r) (r : 'a rope) (k : unit -> 'r) : 'r =
  match r with
  | Leaf x -> f x k
  | Cat (l, r) -> iter_k f l (fun () -> iter_k f r k)

(* ---- C2. the sequence again, from iter_k ---------------------------------- *)

let to_seq (r : 'a rope) : 'a seq =
  fun () -> iter_k (fun x k -> Cons (x, k)) r (fun () -> Nil)

(* ---- C3. find, three ways ----------------------------------------------- *)

(* way 1: iter, and an exception to leave it.
   The first two lines are given: OCaml wants an exception that carries an 'a
   to be declared inside the function, where 'a has a name. *)
let find_raise (type a) (p : a -> bool) (r : a rope) : a option =
  let exception Found of a in
  try
    iter (fun x -> if p x then raise (Found x)) r;
    None
  with Found x -> Some x

(* way 2 is find_first, written in Part A with fold_until *)
let find_until : ('a -> bool) -> 'a rope -> 'a option = find_first

(* way 3: two continuations. k receives the leaf found;
   h is called when there is no such leaf in r *)
let rec find_k (p : 'a -> bool) (r : 'a rope) (k : 'a -> 'r) (h : unit -> 'r) : 'r =
  match r with
  | Leaf x -> if p x then k x else h ()
  | Cat (l, r) -> find_k p l k (fun () -> find_k p r k h)

let find_cps (p : 'a -> bool) (r : 'a rope) : 'a option =
  find_k p r (fun x -> Some x) (fun () -> None)

(* ---- C4. a pattern, and going back -------------------------------------- *)

type item =
  | Lit of char          (* this character *)
  | Any                  (* written ?: one character, not a newline *)
  | Star                 (* written *: as many characters as possible, none a newline *)

type pattern = item list

(* given: "f*n" is [Lit 'f'; Star; Lit 'n'] *)
let pattern_of_string (s : string) : pattern =
  List.map
    (function
      | '?' -> Any
      | '*' -> Star
      | c -> Lit c)
    (List.of_seq (String.to_seq s))

(* given: f, counted. We wrap with it every failure continuation that we
   make ourselves, so that the editor can say how many times we went back *)
let going_back (f : unit -> 'r) : unit -> 'r =
 fun () ->
  Probe.back ();
  f ()

(* does the sequence of characters begin with the pattern?
   succ receives the rest of the sequence, after the match, and fail: what to do if what
   follows the match does not work out. fail is what to do when there is no match. *)
let rec match_k (p : pattern) (c : char seq)
    (succ : char seq -> (unit -> 'r) -> 'r) (fail : unit -> 'r) : 'r =
  match p with
  | [] -> succ c fail
  | Lit x :: rest -> (
      match c () with
      | Cons (y, c') when x = y -> match_k rest c' succ fail
      | _ -> fail ())
  | Any :: rest -> (
      match c () with
      | Cons (y, c') when y <> '\n' -> match_k rest c' succ fail
      | _ -> fail ())
  | Star :: rest -> (
      match c () with
      | Cons (y, c') when y <> '\n' ->
          (* take one more character; if that fails, the star ends here *)
          match_k p c' succ (going_back (fun () -> match_k rest c succ fail))
      | _ -> match_k rest c succ fail)

let matches (p : pattern) (c : char seq) : bool =
  match_k p c (fun _ _ -> true) (fun () -> false)

(* given: the first position, counting from 0, where the text begins with the
   pattern, and the rest of the sequence after the match. When the match fails at one
   position, the failure continuation tries the next one. *)
let search (p : pattern) (c : char seq) : (int * char seq) option =
  let rec from (i : int) (c : char seq) =
    match_k p c
      (fun after _ -> Some (i, after))
      (fun () ->
        match c () with
        | Nil -> None
        | Cons (_, rest) -> from (i + 1) rest)
  in
  from 0 c
