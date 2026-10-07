(* Lab 5 — the rope. Fill in the holes marked TODO, level by level.
   Part A goes with block 1 of the lecture: we hand over one step of a traversal.

     dune exec ./main.exe         the checks
     dune exec ./edit.exe         the editor *)

type 'a rope =
  | Leaf of 'a
  | Cat of 'a rope * 'a rope

(* ---- given ------------------------------------------------------------- *)

(* the rope calls f on every leaf, from left to right *)
let rec iter (f : 'a -> unit) (r : 'a rope) : unit =
  match r with
  | Leaf x -> f x
  | Cat (l, r) ->
      iter f l;
      iter f r

(* a rope of the elements of a list, as flat as it can be; the list is not empty *)
let of_list (xs : 'a list) : 'a rope =
  let a = Array.of_list xs in
  let rec build lo hi =
    if hi - lo = 1 then Leaf a.(lo)
    else
      let mid = (lo + hi) / 2 in
      let l = build lo mid in
      let r = build mid hi in
      Cat (l, r)
  in
  if xs = [] then invalid_arg "of_list: a rope has at least one leaf";
  build 0 (Array.length a)

let of_string (s : string) : char rope = of_list (List.of_seq (String.to_seq s))

(* ---- A0. fold, and what we get from it ---------------------------------- *)

(* the left subtree first: the order is written with two let *)
let rec fold (leaf : 'a -> 'b) (cat : 'b -> 'b -> 'b) (r : 'a rope) : 'b =
  match r with
  | Leaf x -> failwith "TODO A0"
  | Cat (l, r) ->
      failwith "TODO A0"

(* each of the three is one call to fold *)
let length (r : 'a rope) : int = failwith "TODO A0"
let depth (r : 'a rope) : int = failwith "TODO A0"
let to_list (r : 'a rope) : 'a list = failwith "TODO A0"

(* ---- A1. insert and delete ---------------------------------------------- *)

(* given: the first i leaves, and the others; 0 < i < length r.
   Nothing is copied but the nodes on the way down: the two results share
   every other subtree with r. *)
let rec split (i : int) (r : 'a rope) : 'a rope * 'a rope =
  match r with
  | Leaf _ -> invalid_arg "split: 0 < i < length"
  | Cat (l, r) ->
      let n = length l in
      if i = n then (l, r)
      else if i < n then
        let a, b = split i l in
        (a, Cat (b, r))
      else
        let a, b = split (i - n) r in
        (Cat (l, a), b)

(* x becomes the leaf number i, counting from 0; 0 <= i <= length r *)
let insert (i : int) (x : 'a) (r : 'a rope) : 'a rope =
  failwith "TODO A1"

(* the rope without its leaf number i; 0 <= i < length r, and length r >= 2 *)
let delete (i : int) (r : 'a rope) : 'a rope =
  failwith "TODO A1"

(* ---- A2. undo: a version is a root -------------------------------------- *)

type 'a history = {
  past : 'a list;        (* the versions before the present one, the latest first *)
  present : 'a;
  future : 'a list;      (* the versions undone, the next one to redo first *)
}

let start (x : 'a) : 'a history = { past = []; present = x; future = [] }

(* x becomes the present; what was undone is forgotten *)
let record (x : 'a) (h : 'a history) : 'a history =
  failwith "TODO A2"

(* one version back; the history itself when there is no version before *)
let undo (h : 'a history) : 'a history =
  failwith "TODO A2"

(* one version forward again; the history itself when nothing was undone *)
let redo (h : 'a history) : 'a history =
  failwith "TODO A2"

(* ---- A3. a traversal that can stop -------------------------------------- *)

type 'b go =
  | Continue of 'b
  | Stop of 'b

(* f answers Continue to go on with the next leaf, Stop to end the traversal *)
let rec fold_until (f : 'b -> 'a -> 'b go) (acc : 'b) (r : 'a rope) : 'b go =
  match r with
  | Leaf x -> failwith "TODO A3"
  | Cat (l, r) -> (
      failwith "TODO A3")

(* the first leaf, from the left, for which p answers true; one call to fold_until *)
let find_first (p : 'a -> bool) (r : 'a rope) : 'a option =
  failwith "TODO A3"

(* ---- A4. the sequence, by hand -------------------------------------------- *)

type 'a node =
  | Nil
  | Cons of 'a * 'a seq

and 'a seq = unit -> 'a node

(* the state is a stack of subtrees still to visit, as in session 4's count *)
let to_seq (r : 'a rope) : 'a seq =
  let rec go (stack : 'a rope list) : 'a seq =
   fun () ->
    match stack with
    | [] -> failwith "TODO A4"
    | Leaf x :: rest -> failwith "TODO A4"
    | Cat (l, r) :: rest -> failwith "TODO A4"
  in
  go [ r ]

(* ---- A5. two sequences in step -------------------------------------------- *)

(* the same elements in the same order, whatever the shapes of the ropes *)
let rec same (c1 : 'a seq) (c2 : 'a seq) : bool =
  failwith "TODO A5"

(* the position of the first element that differs, counting from 0; the
   position where one sequence ends before the other; None when they are the same *)
let first_difference (c1 : 'a seq) (c2 : 'a seq) : int option =
  failwith "TODO A5"
