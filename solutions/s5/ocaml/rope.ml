(* Lab 5 — the rope. SOLUTIONS: every hole filled, with the reasoning beside it.
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
  | Leaf x -> leaf x
  | Cat (l, r) ->
      let a = fold leaf cat l in
      let b = fold leaf cat r in
      cat a b

(* each of the three is one call to fold *)
let length (r : 'a rope) : int = fold (fun _ -> 1) ( + ) r
let depth (r : 'a rope) : int = fold (fun _ -> 0) (fun a b -> 1 + max a b) r
let to_list (r : 'a rope) : 'a list = fold (fun x -> [ x ]) ( @ ) r

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
  if i = 0 then Cat (Leaf x, r)
  else if i = length r then Cat (r, Leaf x)
  else
    let l, rest = split i r in
    Cat (l, Cat (Leaf x, rest))

(* the rope without its leaf number i; 0 <= i < length r, and length r >= 2 *)
let delete (i : int) (r : 'a rope) : 'a rope =
  let n = length r in
  if n < 2 || i < 0 || i >= n then invalid_arg "delete"
  else if i = 0 then snd (split 1 r)
  else if i = n - 1 then fst (split i r)
  else
    let l, rest = split i r in
    let _, after = split 1 rest in
    Cat (l, after)

(* ---- A2. undo: a version is a root -------------------------------------- *)

type 'a history = {
  past : 'a list;        (* the versions before the present one, the latest first *)
  present : 'a;
  future : 'a list;      (* the versions undone, the next one to redo first *)
}

let start (x : 'a) : 'a history = { past = []; present = x; future = [] }

(* x becomes the present; what was undone is forgotten *)
let record (x : 'a) (h : 'a history) : 'a history =
  { past = h.present :: h.past; present = x; future = [] }

(* one version back; the history itself when there is no version before *)
let undo (h : 'a history) : 'a history =
  match h.past with
  | [] -> h
  | x :: rest -> { past = rest; present = x; future = h.present :: h.future }

(* one version forward again; the history itself when nothing was undone *)
let redo (h : 'a history) : 'a history =
  match h.future with
  | [] -> h
  | x :: rest -> { past = h.present :: h.past; present = x; future = rest }

(* ---- A3. a traversal that can stop -------------------------------------- *)

type 'b go =
  | Continue of 'b
  | Stop of 'b

(* f answers Continue to go on with the next leaf, Stop to end the traversal *)
let rec fold_until (f : 'b -> 'a -> 'b go) (acc : 'b) (r : 'a rope) : 'b go =
  match r with
  | Leaf x -> f acc x
  | Cat (l, r) -> (
      match fold_until f acc l with
      | Stop a -> Stop a
      | Continue a -> fold_until f a r)

(* the first leaf, from the left, for which p answers true; one call to fold_until *)
let find_first (p : 'a -> bool) (r : 'a rope) : 'a option =
  match fold_until (fun _ x -> if p x then Stop (Some x) else Continue None) None r with
  | Stop found -> found
  | Continue _ -> None

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
    | [] -> Nil
    | Leaf x :: rest -> Cons (x, go rest)
    | Cat (l, r) :: rest -> go (l :: r :: rest) ()
  in
  go [ r ]

(* ---- A5. two sequences in step -------------------------------------------- *)

(* the same elements in the same order, whatever the shapes of the ropes *)
let rec same (c1 : 'a seq) (c2 : 'a seq) : bool =
  match (c1 (), c2 ()) with
  | Nil, Nil -> true
  | Cons (x, k1), Cons (y, k2) -> x = y && same k1 k2
  | _ -> false

(* the position of the first element that differs, counting from 0; the
   position where one sequence ends before the other; None when they are the same *)
let first_difference (c1 : 'a seq) (c2 : 'a seq) : int option =
  let rec go i c1 c2 =
    match (c1 (), c2 ()) with
    | Nil, Nil -> None
    | Cons (x, k1), Cons (y, k2) -> if x = y then go (i + 1) k1 k2 else Some i
    | _ -> Some i
  in
  go 0 c1 c2
