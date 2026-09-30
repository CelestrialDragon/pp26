(* Session 5, block 1, parts 2 to 4, on lists: every program of these slides, in
   the order of the deck.

     ocaml lists.ml

   It prints, in turn: three lists made with fold_right and the text read by
   iter; found=true visited=5; found=true visited=3; then the words given by an
   iterator, two at a time and then three. *)

(* ---- Part 2. Traversing a list ------------------------------------------ *)

(* Session 4's two folds. fold starts from acc and goes from the first element to
   the last; it is a loop, its recursive call is the last thing it does. *)
let rec fold f acc = function
  | [] -> acc
  | x :: xs -> fold f (f acc x) xs

(* fold_right first goes to the end of the list: f is called on the LAST element
   first, and on the first element last, with the result for the rest. *)
let rec fold_right f xs z = match xs with
  | [] -> z
  | x :: xs -> f x (fold_right f xs z)

(* Three functions written with fold_right. In each, x is an element and the
   second argument is the result already computed for the rest of the list. *)
let map f l    = fold_right (fun x r -> f x :: r) l []
let filter p l = fold_right (fun x r -> if p x then x :: r else r) l []
let length l   = fold_right (fun _ n -> n + 1) l 0

(* iter: our function is called on every element, for what it does. Its result
   is thrown away: the accumulator is (), which carries nothing. *)
let iter f l = fold (fun () x -> f x) () l

let text = ['h'; 'i'; '\n'; 'y'; 'o']

let show_chars l =
  "[" ^ String.concat "; " (List.map (fun c -> Printf.sprintf "%C" c) l) ^ "]"

let () =
  print_endline (show_chars (map Char.uppercase_ascii text));
  print_endline (show_chars (filter (fun c -> c <> '\n') text));
  Printf.printf "%d\n" (length text);
  iter (fun c -> print_string (String.escaped (String.make 1 c) ^ " ")) text;
  print_newline ()

(* ---- Part 3. Stopping --------------------------------------------------- *)

(* Is there a newline in the text? exists as a fold gives the right answer. The
   counter says what it costs: our function is called on the five characters,
   and the newline is the third. After the newline found is true and || does not
   call p any more, but the fold still calls our function. *)
let visited = ref 0

let exists p l =
  fold (fun found x -> incr visited; found || p x) false l

let () =
  let found = exists (fun c -> c = '\n') text in
  Printf.printf "found=%b visited=%d\n" found !visited

(* A different interface. Our function does not only return the next accumulator:
   its answer says whether to go on. fold_until reads that answer, and stops. *)
type 'b go = Continue of 'b | Stop of 'b

let rec fold_until f acc = function
  | [] -> acc
  | x :: xs ->
      match f acc x with
      | Stop a -> a                            (* the rest, xs, is not visited *)
      | Continue a -> fold_until f a xs

(* It is named exists on the slide. Here the two must live in one file. *)
let exists_until p l =
  fold_until (fun _ x ->
      incr visited;
      if p x then Stop true else Continue false)
    false l

(* The counter is set back to 0 first. Without this line the run prints
   visited=8: the five calls of the run above, and the three of this one. *)
let () =
  visited := 0;
  let found = exists_until (fun c -> c = '\n') text in
  Printf.printf "found=%b visited=%d\n" found !visited

(* ---- Part 4. Iterators -------------------------------------------------- *)

(* An iterator written by hand, as a closure. It is the counter of part 1: a
   function and a state of its own. The state, here, is the part of the list that
   has not been given yet. Each call gives one element and moves here. *)
let iterator l =
  let here = ref l in
  fun () ->
    match !here with
    | [] -> None                               (* at the end, and it stays there *)
    | x :: rest -> here := rest; Some x

(* We call next when we want: two times, then something else, then three times.
   The iterator waits between two calls, which a fold never does. *)
let () =
  let next = iterator ["to"; "be"; "or"; "not"] in
  let show = function Some w -> w | None -> "None" in
  let a = next () in let b = next () in
  print_endline (show a ^ " " ^ show b);
  let c = next () in let d = next () in let e = next () in
  print_endline (show c ^ " " ^ show d ^ " " ^ show e)
