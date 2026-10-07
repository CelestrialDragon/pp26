(* Way 1: a set is data. The kinds are a closed list; every operation is one match on it.
   ocaml set_way1.ml *)

type set =
  | Empty
  | Insert of int * set
  | Evens
  | Union of set * set        (* a union the operation could not compute, kept as data *)

let rec contains s x =
  match s with
  | Empty -> false
  | Insert (n, r) -> x = n || contains r x
  | Evens -> x mod 2 = 0
  | Union (s, t) -> contains s x || contains t x     (* Way 2's union, as data *)

let rec is_empty = function
  | Empty -> true
  | Insert _ -> false
  | Evens -> false
  | Union (s, t) -> is_empty s && is_empty t

(* two finite sets are merged; anything else is kept as a Union *)
let rec union s t =
  match s, t with
  | Empty, t -> t
  | Insert (n, r), t -> Insert (n, union r t)
  | (Evens | Union _), t -> Union (s, t)

(* equal inspects both sets, through a normal form: does the set hold the evens, and which
   other elements does it hold? Two sets are equal when their normal forms are. Every kind
   is covered, Union included: equality is one more function, as Way 1 promises. *)
let rec has_evens = function
  | Empty -> false
  | Insert (_, r) -> has_evens r
  | Evens -> true
  | Union (s, t) -> has_evens s || has_evens t

let rec inserted = function
  | Empty | Evens -> []
  | Insert (n, r) -> n :: inserted r
  | Union (s, t) -> inserted s @ inserted t

let normal s =
  let e = has_evens s in
  e, List.sort_uniq compare (List.filter (fun n -> not (e && n mod 2 = 0)) (inserted s))

let equal s t = normal s = normal t

let () =
  let s = Insert (3, Insert (5, Insert (3, Empty))) in          (* 3 inserted twice *)
  let u = union s Evens in
  Printf.printf "%b %b %b\n" (is_empty Empty) (is_empty s) (is_empty (union Empty Empty));
  List.iter (fun x -> Printf.printf "%b " (contains s x)) [3; 4; 5; 6]; print_newline ();
  List.iter (fun x -> Printf.printf "%b " (contains u x)) [3; 4; 5; 6]; print_newline ();
  Printf.printf "%b %b %b %b\n" (equal s (Insert (5, Insert (3, Empty)))) (equal Evens s)
    (equal Evens (Insert (2, Evens))) (equal (Union (s, Evens)) (Insert (3, Insert (5, Evens))));
  (* property: equal s t holds exactly when contains agrees on every point, for every set
     built from a small grammar in which each constructor appears inside every other *)
  let small = [Empty; Insert (3, Empty); Insert (2, Insert (3, Empty)); Evens] in
  let level xs = xs @ List.concat_map (fun a -> List.map (fun b -> Union (a, b)) xs) xs
                 @ List.map (fun a -> Insert (2, a)) xs @ List.map (fun a -> Insert (5, a)) xs in
  let sets = level (level small) in
  let pts = List.init 21 (fun i -> i - 10) in
  let bad = List.fold_left (fun n s -> List.fold_left (fun n t ->
      if equal s t = List.for_all (fun x -> contains s x = contains t x) pts then n else n + 1) n sets) 0 sets in
  Printf.printf "equal vs pointwise contains on %d x %d sets: %s\n" (List.length sets) (List.length sets)
    (if bad = 0 then "agree" else string_of_int bad ^ " DISAGREE")
