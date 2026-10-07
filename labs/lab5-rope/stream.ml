(* Lab 5 — sequences. Fill in the holes marked TODO, level by level.
   Part B goes with block 2 of the lecture: we hand over a computation not yet run.
   A sequence is a function: nothing is computed before it is called. *)

open Rope

(* ---- given ------------------------------------------------------------- *)

(* n, n + 1, n + 2, ... without end *)
let rec nats (n : int) : int seq = fun () -> Cons (n, nats (n + 1))

let rec of_list (xs : 'a list) : 'a seq =
 fun () ->
  match xs with
  | [] -> Nil
  | x :: rest -> Cons (x, of_list rest)

let of_string (s : string) : char seq = of_list (List.of_seq (String.to_seq s))

(* the characters of a list, written last first, as a string *)
let string_of_rev (cs : char list) : string = String.of_seq (List.to_seq (List.rev cs))

(* does the text s contain the text sub? *)
let contains (sub : string) (s : string) : bool =
  let n = String.length sub and m = String.length s in
  let rec at i = i + n <= m && (String.sub s i n = sub || at (i + 1)) in
  at 0

(* ---- B1. four functions on sequences -------------------------------------- *)

(* the first n elements, fewer if the sequence ends; nothing more is asked for *)
let rec take (n : int) (c : 'a seq) : 'a list =
  if n <= 0 then []
  else
    match c () with
    | Nil -> failwith "TODO B1"
    | Cons (x, rest) -> failwith "TODO B1"

let rec map (f : 'a -> 'b) (c : 'a seq) : 'b seq =
 fun () ->
  match c () with
  | Nil -> failwith "TODO B1"
  | Cons (x, rest) -> failwith "TODO B1"

let rec filter (p : 'a -> bool) (c : 'a seq) : 'a seq =
 fun () ->
  match c () with
  | Nil -> failwith "TODO B1"
  | Cons (x, rest) -> failwith "TODO B1"

(* the elements before the first one for which p answers false *)
let rec take_while (p : 'a -> bool) (c : 'a seq) : 'a seq =
 fun () ->
  match c () with
  | Nil -> failwith "TODO B1"
  | Cons (x, rest) -> failwith "TODO B1"

(* ---- B2. the lines of a document ---------------------------------------- *)

(* the characters up to the next newline, last first, and the rest of the sequence;
   node is what the sequence answered when it was asked for the first character *)
let rec read_line (acc : char list) (node : char node) : char list * char seq =
  match node with
  | Nil -> failwith "TODO B2"
  | Cons ('\n', rest) -> failwith "TODO B2"
  | Cons (x, rest) -> failwith "TODO B2"

(* a line is read when it is asked for, and not before *)
let rec lines (c : char seq) : string seq =
 fun () ->
  match c () with
  | Nil -> failwith "TODO B2"
  | first ->
      failwith "TODO B2"

(* ---- B3. the lines that match, with their numbers ----------------------- *)

(* the first line has number 1 *)
let grep (p : string -> bool) (c : string seq) : (int * string) seq =
  let rec go (n : int) (c : string seq) : (int * string) seq =
   fun () ->
    match c () with
    | Nil -> failwith "TODO B3"
    | Cons (s, rest) -> failwith "TODO B3"
  in
  go 1 c
