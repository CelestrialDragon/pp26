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

(* equal inspects both sets. Finite against finite: compare elements. Evens against
   anything: compare tags. A Union has no normal form here: this is the open cell. *)
let rec elements = function
  | Empty -> []
  | Insert (n, r) -> n :: elements r
  | Evens | Union _ -> invalid_arg "elements: not finite"

let equal s t =
  match s, t with
  | Union _, _ | _, Union _ -> failwith "equal on a Union: needs a normal form"
  | Evens, Evens -> true
  | Evens, _ | _, Evens -> false
  | _ -> List.for_all (contains t) (elements s) && List.for_all (contains s) (elements t)

let () =
  let s = Insert (3, Insert (5, Insert (3, Empty))) in          (* 3 inserted twice *)
  let u = union s Evens in
  Printf.printf "%b %b %b\n" (is_empty Empty) (is_empty s) (is_empty (union Empty Empty));
  List.iter (fun x -> Printf.printf "%b " (contains s x)) [3; 4; 5; 6]; print_newline ();
  List.iter (fun x -> Printf.printf "%b " (contains u x)) [3; 4; 5; 6]; print_newline ();
  Printf.printf "%b %b\n" (equal s (Insert (5, Insert (3, Empty)))) (equal Evens s)
