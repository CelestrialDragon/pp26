(* Warm-up, lab 5 — the keyboard exercises of the warm-up sheet. SOLUTIONS: every hole filled.
   Part 1 goes with block 1 of the lecture, Part 2 with block 2. The exercises
   are stated on the sheet, this file only says where to type.

     ocaml warmup/warmup.ml        # pass / FAIL / todo, one line per check *)

let ok = ref 0 and bad = ref 0 and todo = ref 0
let check name f =
  match f () with
  | true -> incr ok; Printf.printf "  pass  %s\n%!" name
  | false -> incr bad; Printf.printf "  FAIL  %s\n%!" name
  | exception Failure msg when String.length msg >= 4 && String.sub msg 0 4 = "TODO" ->
      incr todo; Printf.printf "  todo  %s (%s)\n%!" name msg

(* ======================= Part 1: functions, folds, iterators ============== *)

(* ---- 1. Functions that take and return functions ------------------------ *)

let add (n : int) : int -> int = fun x -> x + n

let add_pair ((n, x) : int * int) : int = x + n

let add3 : int -> int = add 3

let add3' : int -> int = fun x -> add_pair (3, x)

let twice (f : 'a -> 'a) (x : 'a) : 'a = f (f x)

let compose (f : 'b -> 'c) (g : 'a -> 'b) : 'a -> 'c = fun x -> f (g x)

let () =
  print_endline "1. functions that take and return functions";
  check "add3 10 = 13, from add and from add_pair" (fun () -> add3 10 = 13 && add3' 10 = 13);
  check "twice add3 10 = 16" (fun () -> twice add3 10 = 16);
  check "compose: first g, then f" (fun () ->
    compose string_of_int (add 1) 41 = "42" && compose (add 1) (fun x -> x * 2) 5 = 11)

(* ---- 2. A closure with a state of its own ------------------------------- *)

let counter () : unit -> int =
  let n = ref 0 in
  fun () -> incr n; !n

(* each call adds x to the total, and returns the total *)
let accumulator () : int -> int =
  let total = ref 0 in
  fun x -> total := !total + x; !total

let () =
  print_endline "2. a closure with a state of its own";
  check "one accumulator: 5, then 3 more, then 0 more" (fun () ->
    let acc = accumulator () in
    let a = acc 5 in let b = acc 3 in let c = acc 0 in
    (a, b, c) = (5, 8, 8));
  check "two accumulators do not share their total" (fun () ->
    let p = accumulator () and q = accumulator () in
    let _ = p 10 in
    q 1 = 1)

(* ---- 3. Three functions, written with a fold ---------------------------- *)

let rec fold (f : 'b -> 'a -> 'b) (acc : 'b) (l : 'a list) : 'b =
  match l with
  | [] -> acc
  | x :: xs -> fold f (f acc x) xs

let sum (l : int list) : int = fold (fun acc x -> acc + x) 0 l

let count (p : 'a -> bool) (l : 'a list) : int =
  fold (fun n x -> if p x then n + 1 else n) 0 l

let rev (l : 'a list) : 'a list = fold (fun acc x -> x :: acc) [] l

let () =
  print_endline "3. three functions, written with a fold";
  check "sum [1; 2; 3; 4] = 10, and sum [] = 0" (fun () -> sum [1; 2; 3; 4] = 10 && sum [] = 0);
  check "count: the even numbers of [1; 2; 3; 4; 6]" (fun () ->
    count (fun x -> x mod 2 = 0) [1; 2; 3; 4; 6] = 3);
  check "rev [1; 2; 3] = [3; 2; 1]" (fun () -> rev [1; 2; 3] = [3; 2; 1] && rev [] = ([] : int list))

(* ---- 4. A fold that can stop -------------------------------------------- *)

type 'b go =
  | Continue of 'b
  | Stop of 'b

let rec fold_until (f : 'b -> 'a -> 'b go) (acc : 'b) (l : 'a list) : 'b =
  match l with
  | [] -> acc
  | x :: xs ->
      match f acc x with
      | Stop a -> a
      | Continue a -> fold_until f a xs

(* the first element that satisfies p, or None *)
let find_first (p : 'a -> bool) (l : 'a list) : 'a option =
  fold_until (fun _ x -> if p x then Stop (Some x) else Continue None) None l

(* the sum of the elements, up to the first negative one, which is left out *)
let sum_until_negative (l : int list) : int =
  fold_until (fun acc x -> if x < 0 then Stop acc else Continue (acc + x)) 0 l

let () =
  print_endline "4. a fold that can stop";
  check "find_first: the first number over 10" (fun () ->
    find_first (fun x -> x > 10) [3; 12; 7; 40] = Some 12
    && find_first (fun x -> x > 10) [3; 7] = None);
  check "find_first asks p about 2 elements of [3; 12; 7; 40], not 4" (fun () ->
    let calls = ref 0 in
    let _ = find_first (fun x -> incr calls; x > 10) [3; 12; 7; 40] in
    !calls = 2);
  check "sum_until_negative [4; 5; -1; 100] = 9" (fun () ->
    sum_until_negative [4; 5; -1; 100] = 9 && sum_until_negative [1; 2] = 3)

(* ---- 5. An iterator, as a closure --------------------------------------- *)

(* the numbers a, a + 1, ... up to b - 1, one at each call; then None *)
let range (a : int) (b : int) : unit -> int option =
  let next = ref a in
  fun () ->
    if !next >= b then None
    else begin
      let x = !next in
      next := x + 1;
      Some x
    end

(* every element that the iterator still has to give, in order *)
let rec drain (next : unit -> 'a option) : 'a list =
  match next () with
  | None -> []
  | Some x -> x :: drain next

let () =
  print_endline "5. an iterator, as a closure";
  check "range 3 6 gives 3, 4, 5, then None, and None again" (fun () ->
    let next = range 3 6 in
    let a = next () in let b = next () in let c = next () in
    let d = next () in let e = next () in
    (a, b, c, d, e) = (Some 3, Some 4, Some 5, None, None));
  check "drain (range 0 4) = [0; 1; 2; 3]" (fun () -> drain (range 0 4) = [0; 1; 2; 3]);
  check "an iterator that was read keeps its place: drain gives what is left" (fun () ->
    let next = range 0 4 in
    let _ = next () in
    drain next = [1; 2; 3] && drain next = [])

(* ======================= Part 2: thunks, sequences, lazy ================== *)

type 'a node =
  | Nil
  | Cons of 'a * 'a seq

and 'a seq = unit -> 'a node

let rec take (n : int) (s : 'a seq) : 'a list =
  if n <= 0 then []
  else
    match s () with
    | Nil -> []
    | Cons (x, rest) -> x :: take (n - 1) rest

(* ---- 6. A row of the triangle, from the row above it -------------------- *)

let rec zip_with (f : 'a -> 'b -> 'c) (l1 : 'a list) (l2 : 'b list) : 'c list =
  match l1, l2 with
  | x :: xs, y :: ys -> f x y :: zip_with f xs ys
  | _ -> []

(* if row is row n - 1 of the triangle:  0 :: row   has T (n-1, k-1) at position k,
                                         row @ [0]  has T (n-1, k)   at position k *)
let next_row (row : int list) : int list =
  zip_with ( + ) (0 :: row) (row @ [0])

let () =
  print_endline "6. a row of the triangle, from the row above it";
  check "zip_with ( + ) [1; 2; 3] [10; 20; 30] = [11; 22; 33]" (fun () ->
    zip_with ( + ) [1; 2; 3] [10; 20; 30] = [11; 22; 33]);
  check "zip_with stops with the shorter list" (fun () ->
    zip_with ( * ) [1; 2; 3] [10; 20] = [10; 40]);
  check "next_row [1; 3; 3; 1] = [1; 4; 6; 4; 1]" (fun () ->
    next_row [1; 3; 3; 1] = [1; 4; 6; 4; 1] && next_row [1] = [1; 1])

(* ---- 7. A sequence without end ------------------------------------------ *)

let built = ref 0          (* counts the cells that iterate builds *)

(* the sequence x, f x, f (f x), ... : x is the first value, f the step from one to the next *)
let rec iterate (f : 'a -> 'a) (x : 'a) : 'a seq =
  fun () -> incr built; Cons (x, iterate f (f x))

let () =
  print_endline "7. a sequence without end";
  check "take 5 (iterate (fun n -> n * 2) 1) = [1; 2; 4; 8; 16]" (fun () ->
    take 5 (iterate (fun n -> n * 2) 1) = [1; 2; 4; 8; 16]);
  check "making the sequence builds no cell; take 5 builds 5" (fun () ->
    built := 0;
    let s = iterate (fun n -> n + 1) 0 in
    let before = !built in
    let _ = take 5 s in
    before = 0 && !built = 5)

(* ---- 8. The triangle, as the sequence of its rows ----------------------- *)

(* row 0 is [1]; next_row is the step from row n - 1 to row n *)
let pascal : int list seq = iterate next_row [1]

(* row n of the triangle; the top row is row 0 *)
let row (n : int) : int list =
  List.nth (take (n + 1) pascal) n

let () =
  print_endline "8. the triangle, as the sequence of its rows";
  check "its first four rows" (fun () ->
    take 4 pascal = [[1]; [1; 1]; [1; 2; 1]; [1; 3; 3; 1]]);
  check "row 6 = [1; 6; 15; 20; 15; 6; 1]" (fun () -> row 6 = [1; 6; 15; 20; 15; 6; 1]);
  check "row 6 builds 7 rows, and no more" (fun () ->
    built := 0;
    let _ = row 6 in
    !built = 7)

(* the triangle on the screen, once pascal is written: its first rows, then
   sixteen rows with a star for every odd number *)
let centred (width : int) (line : string) : string =
  String.make (max 0 ((width - String.length line) / 2)) ' ' ^ line

let () =
  match take 16 pascal with
  | exception Failure _ -> ()
  | rows when List.length rows < 16 -> ()
  | rows ->
      print_newline ();
      List.iteri (fun i r ->
          if i < 8 then
            print_endline (centred 40 (String.concat "  " (List.map (Printf.sprintf "%2d") r))))
        rows;
      print_newline ();
      List.iter (fun r ->
          print_endline (centred 40
            (String.concat " " (List.map (fun x -> if x mod 2 = 1 then "*" else " ") r))))
        rows;
      print_newline ()

(* ---- 9. The diagonals of the triangle ----------------------------------- *)

(* the sum of diagonal n: row n at column 0, row n - 1 at column 1, row n - 2
   at column 2, ... as long as the column exists in the row:
   T (n, 0) + T (n-1, 1) + T (n-2, 2) + ...   where T (n-k, k) is element k of row n - k *)
let diagonal (n : int) : int =
  let rows = take (n + 1) pascal in
  let rec go (k : int) (acc : int) : int =
    if 2 * k > n then acc
    else go (k + 1) (acc + List.nth (List.nth rows (n - k)) k)
  in
  go 0 0

let () =
  print_endline "9. the diagonals of the triangle";
  check "diagonals 0 to 9 are 1 1 2 3 5 8 13 21 34 55" (fun () ->
    List.init 10 diagonal = [1; 1; 2; 3; 5; 8; 13; 21; 34; 55])

(* ---- 10. A row computed once -------------------------------------------- *)

(* as many lazy values as we ask for: number i holds row i, not yet computed *)
let lazy_rows (n : int) : int list Lazy.t list =
  List.init n (fun i -> lazy (row i))

let () =
  print_endline "10. a row computed once";
  check "ten lazy rows are made: no row is built" (fun () ->
    built := 0;
    let _ = lazy_rows 10 in
    !built = 0);
  check "row 5 is read twice, and built once" (fun () ->
    let rows = lazy_rows 10 in
    built := 0;
    let a = Lazy.force (List.nth rows 5) in
    let first = !built in
    let b = Lazy.force (List.nth rows 5) in
    a = [1; 5; 10; 10; 5; 1] && a = b && first = 6 && !built = 6)

let () =
  Printf.printf "\n%d pass, %d FAIL, %d todo\n" !ok !bad !todo
