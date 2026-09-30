(* Provided. The checks, one per line of `dune exec ./main.exe`; the editor
   shows the same ones in its quest strip. *)

open Rope

type status =
  | Pass
  | Fail of string
  | Todo of string

type check = {
  level : string;        (* "A0", "A1", ... *)
  name : string;
  status : status;
}

let is_todo msg = String.length msg >= 4 && String.sub msg 0 4 = "TODO"

let results : check list ref = ref []

let check level name (f : unit -> bool) =
  let status =
    match f () with
    | true -> Pass
    | false -> Fail "the answer is not the expected one"
    | exception Failure msg when is_todo msg -> Todo msg
    | exception Stack_overflow -> Fail "stack overflow"
    | exception e -> Fail ("raised " ^ Printexc.to_string e)
  in
  results := { level; name; status } :: !results

(* ---- ropes to test on --------------------------------------------------- *)

(* "hi", a newline, "yo": the lecture's document *)
let doc = Cat (Cat (Leaf 'h', Leaf 'i'), Cat (Leaf '\n', Cat (Leaf 'y', Leaf 'o')))

(* the same text in another shape *)
let doc' = Cat (Leaf 'h', Cat (Cat (Leaf 'i', Leaf '\n'), Cat (Leaf 'y', Leaf 'o')))

(* every rope whose leaves are the elements of the list, in order *)
let rec shapes (xs : 'a list) : 'a rope list =
  match xs with
  | [] -> []
  | [ x ] -> [ Leaf x ]
  | _ ->
      let n = List.length xs in
      List.concat
        (List.init (n - 1) (fun i ->
             let left = List.filteri (fun j _ -> j <= i) xs in
             let right = List.filteri (fun j _ -> j > i) xs in
             List.concat_map
               (fun l -> List.map (fun r -> Cat (l, r)) (shapes right))
               (shapes left)))

let upto n = List.init n (fun i -> i)

(* the ropes of 1 to 5 leaves, 23 of them, leaves numbered from 0 *)
let small : int rope list = List.concat_map (fun n -> shapes (upto n)) [ 1; 2; 3; 4; 5 ]

(* the leaves, read without any function of the student *)
let leaves (r : 'a rope) : 'a list =
  let acc = ref [] in
  iter (fun x -> acc := x :: !acc) r;
  List.rev !acc

let rec list_insert i x xs =
  if i = 0 then x :: xs
  else
    match xs with
    | [] -> [ x ]
    | y :: rest -> y :: list_insert (i - 1) x rest

let list_delete i xs = List.filteri (fun j _ -> j <> i) xs

(* a sequence read to its end, without any function of the student *)
let rec drain (c : 'a seq) : 'a list =
  match c () with
  | Nil -> []
  | Cons (x, rest) -> x :: drain rest

(* a sequence that counts the elements asked for *)
let counting (n : int ref) (c : 'a seq) : 'a seq =
  let rec wrap c () =
    match c () with
    | Nil -> Nil
    | Cons (x, rest) ->
        incr n;
        Cons (x, wrap rest)
  in
  wrap c

(* a rope of n leaves that leans to the left, as deep as it is long *)
let left_deep n =
  let r = ref (Leaf 0) in
  for i = 1 to n - 1 do
    r := Cat (!r, Leaf i)
  done;
  !r

let text = "let x = 1\nlet y = 2\n\nprint x\n"

(* ---- Part A ------------------------------------------------------------- *)

let part_a () =
  check "A0" "fold visits the left subtree first" (fun () ->
      let seen = ref [] in
      fold (fun x -> seen := x :: !seen) (fun () () -> ()) doc;
      List.rev !seen = [ 'h'; 'i'; '\n'; 'y'; 'o' ]);
  check "A0" "length doc = 5, depth doc = 3, depth of one leaf = 0" (fun () ->
      length doc = 5 && depth doc = 3 && depth (Leaf 'a') = 0);
  check "A0" "to_list gives the leaves in order, on the 23 ropes of 1 to 5 leaves" (fun () ->
      List.for_all (fun r -> to_list r = leaves r && length r = List.length (leaves r)) small);
  check "A1" "insert 2 'X' doc spells h i X newline y o" (fun () ->
      leaves (insert 2 'X' doc) = [ 'h'; 'i'; 'X'; '\n'; 'y'; 'o' ]);
  check "A1" "insert at every position of every small rope, the first and the last too" (fun () ->
      List.for_all
        (fun r ->
          let xs = leaves r in
          List.for_all
            (fun i -> leaves (insert i 99 r) = list_insert i 99 xs)
            (upto (List.length xs + 1)))
        small);
  check "A1" "delete at every position of every small rope of two leaves or more" (fun () ->
      List.for_all
        (fun r ->
          let xs = leaves r in
          List.length xs < 2
          || List.for_all (fun i -> leaves (delete i r) = list_delete i xs) (upto (List.length xs)))
        small);
  check "A1" "insert copies nothing it can share: the old rope is unchanged and still there" (fun () ->
      let r = insert 2 'X' doc in
      leaves doc = [ 'h'; 'i'; '\n'; 'y'; 'o' ]
      &&
      match r with
      | Cat (l, Cat (Leaf 'X', rest)) ->
          (match doc with
          | Cat (l0, r0) -> l == l0 && rest == r0
          | Leaf _ -> false)
      | _ -> true (* another shape is allowed; the text is what counts *));
  check "A2" "record, record, undo, undo: back at the start, two versions to redo" (fun () ->
      let h = start 1 |> record 2 |> record 3 |> undo |> undo in
      h.present = 1 && h.past = [] && h.future = [ 2; 3 ]);
  check "A2" "undo then redo gives the history back" (fun () ->
      let h = start 1 |> record 2 |> record 3 in
      redo (undo h) = h && undo (redo h) = undo h);
  check "A2" "undo with no past and redo with no future change nothing" (fun () ->
      let h = start 1 in
      undo h = h && redo h = h);
  check "A2" "record after undo forgets what was undone" (fun () ->
      let h = start 1 |> record 2 |> undo |> record 5 in
      h.present = 5 && h.past = [ 1 ] && h.future = []);
  check "A3" "fold_until stops at the newline: 3 leaves visited, not 5" (fun () ->
      let visited = ref 0 in
      let r =
        fold_until
          (fun _ c ->
            incr visited;
            if c = '\n' then Stop true else Continue false)
          false doc
      in
      r = Stop true && !visited = 3);
  check "A3" "fold_until that never stops is a fold from the left: Continue 10" (fun () ->
      List.for_all
        (fun r -> fold_until (fun acc x -> Continue (acc + x)) 0 r = Continue 10)
        (shapes (upto 5)));
  check "A3" "find_first returns the first leaf that fits, and asks no further" (fun () ->
      List.for_all
        (fun r ->
          let asked = ref 0 in
          let found =
            find_first
              (fun x ->
                incr asked;
                x >= 2)
              r
          in
          found = Some 2 && !asked = 3)
        (shapes (upto 5)));
  check "A3" "find_first answers None when no leaf fits" (fun () ->
      List.for_all (fun r -> find_first (fun x -> x > 100) r = None) small);
  check "A4" "the sequence gives the leaves in order, on the 23 small ropes" (fun () ->
      List.for_all (fun r -> drain (to_seq r) = leaves r) small);
  check "A4" "the first element asked for is the leftmost leaf" (fun () ->
      let r = Cat (Cat (Leaf (lazy 1), Leaf (lazy 2)), Leaf (lazy 3)) in
      let c = to_seq r in
      match c () with
      | Cons (x, _) -> Lazy.force x = 1
      | Nil -> false);
  check "A4" "a sequence read twice gives the same element twice" (fun () ->
      let c = to_seq doc in
      match (c (), c ()) with
      | Cons (x, rest), Cons (y, _) -> (
          x = 'h' && y = 'h'
          &&
          match (rest (), rest ()) with
          | Cons (a, _), Cons (b, _) -> a = 'i' && b = 'i'
          | _ -> false)
      | _ -> false);
  check "A4" "a rope 100 000 deep is read to its end" (fun () ->
      let n = ref 0 in
      let rec go c =
        match c () with
        | Nil -> ()
        | Cons (_, rest) ->
            incr n;
            go rest
      in
      go (to_seq (left_deep 100_000));
      !n = 100_000);
  check "A5" "same: two shapes of one text are the same; structural equality says no" (fun () ->
      same (to_seq doc) (to_seq doc') && doc <> doc');
  check "A5" "same on every pair of the 14 ropes of 5 leaves" (fun () ->
      let rs = shapes (upto 5) in
      List.for_all (fun a -> List.for_all (fun b -> same (to_seq a) (to_seq b)) rs) rs);
  check "A5" "same stops at the first difference: 4 leaves asked of 10" (fun () ->
      let n = ref 0 in
      let other = Cat (Leaf 'h', Cat (Leaf 'X', Cat (Leaf '\n', Cat (Leaf 'y', Leaf 'o')))) in
      (not (same (counting n (to_seq doc)) (counting n (to_seq other)))) && !n = 4);
  check "A5" "same: a text and the same text with one more leaf are not the same" (fun () ->
      (not (same (to_seq doc) (to_seq (Cat (doc, Leaf '!')))))
      && not (same (to_seq (Cat (doc, Leaf '!'))) (to_seq doc)));
  check "A5" "first_difference: None, Some 1, and Some 5 where the shorter one ends" (fun () ->
      let other = Cat (Leaf 'h', Cat (Leaf 'X', Cat (Leaf '\n', Cat (Leaf 'y', Leaf 'o')))) in
      first_difference (to_seq doc) (to_seq doc') = None
      && first_difference (to_seq doc) (to_seq other) = Some 1
      && first_difference (to_seq doc) (to_seq (Cat (doc, Leaf '!'))) = Some 5
      && first_difference (to_seq (Cat (doc, Leaf '!'))) (to_seq doc) = Some 5)

(* ---- Part B ------------------------------------------------------------- *)

let part_b () =
  let open Stream in
  check "B1" "take 3 (nats 1) = [1; 2; 3]; take 0 asks for nothing" (fun () ->
      take 3 (nats 1) = [ 1; 2; 3 ]
      && take 0 (fun () -> failwith "asked") = []
      && take 5 (of_list [ 1; 2 ]) = [ 1; 2 ]);
  check "B1" "take 3 asks for 3 elements, not 4" (fun () ->
      let n = ref 0 in
      take 3 (counting n (nats 1)) = [ 1; 2; 3 ] && !n = 3);
  check "B1" "map: the squares; the function runs once per element asked" (fun () ->
      let calls = ref 0 in
      let squares =
        map
          (fun x ->
            incr calls;
            x * x)
          (nats 1)
      in
      !calls = 0 && drain (take_while (fun x -> x < 30) squares) = [ 1; 4; 9; 16; 25 ] && !calls = 6);
  check "B1" "filter: the even numbers; on a finite sequence, to its end" (fun () ->
      let evens = filter (fun x -> x mod 2 = 0) (nats 1) in
      let n = ref 0 in
      let first = counting n evens in
      (match first () with
      | Cons (2, _) -> true
      | _ -> false)
      && !n = 1
      && drain (filter (fun x -> x mod 2 = 0) (of_list [ 1; 2; 3; 4; 5 ])) = [ 2; 4 ]
      && drain (filter (fun _ -> false) (of_list [ 1; 2; 3 ])) = []);
  check "B1" "take_while stops at the first element that does not fit, and leaves the rest" (fun () ->
      drain (take_while (fun x -> x < 3) (of_list [ 1; 2; 3; 1; 1 ])) = [ 1; 2 ]
      && drain (take_while (fun _ -> true) (of_list [ 1; 2 ])) = [ 1; 2 ]
      && drain (take_while (fun _ -> false) (nats 1)) = []);
  check "B1" "a sequence made by map is read twice with the same result" (fun () ->
      let c = map (fun x -> x + 1) (of_list [ 1; 2; 3 ]) in
      drain c = [ 2; 3; 4 ] && drain c = [ 2; 3; 4 ]);
  check "B2" "lines: four lines, the third one empty" (fun () ->
      drain (lines (of_string text)) = [ "let x = 1"; "let y = 2"; ""; "print x" ]);
  check "B2" "lines: a last line without its newline is a line; no character, no line" (fun () ->
      drain (lines (of_string "a\nb")) = [ "a"; "b" ]
      && drain (lines (of_string "")) = []
      && drain (lines (of_string "\n")) = [ "" ]
      && drain (lines (of_string "\n\na")) = [ ""; ""; "a" ]);
  check "B2" "lines: the first line asked, 10 characters read of 29" (fun () ->
      let n = ref 0 in
      let ls = lines (counting n (of_string text)) in
      !n = 0
      &&
      match ls () with
      | Cons (l, _) -> l = "let x = 1" && !n = 10
      | Nil -> false);
  check "B2" "lines: read twice, the same lines twice" (fun () ->
      let ls = lines (of_string text) in
      drain ls = drain ls && List.length (drain ls) = 4);
  check "B2" "lines of an endless text: the first two lines" (fun () ->
      let endless = map (fun n -> if n mod 3 = 0 then '\n' else 'a') (nats 1) in
      take 2 (lines endless) = [ "aa"; "aa" ]);
  check "B3" "grep: the lines with let, numbered from 1" (fun () ->
      drain (grep (contains "let") (lines (of_string text))) = [ (1, "let x = 1"); (2, "let y = 2") ]);
  check "B3" "grep: the first match asked, the lines after it are not read" (fun () ->
      let n = ref 0 in
      let found = grep (contains "y") (counting n (lines (of_string text))) in
      match found () with
      | Cons ((2, "let y = 2"), _) -> !n = 2
      | _ -> false);
  check "B3" "grep: no line matches, no element" (fun () ->
      drain (grep (contains "zzz") (lines (of_string text))) = [])

(* ---- Part C ------------------------------------------------------------- *)

let part_c () =
  let open Next in
  check "C1" "iter_k visits the leaves in order, then calls the last continuation" (fun () ->
      List.for_all
        (fun r ->
          let seen = ref [] in
          iter_k
            (fun x k ->
              seen := x :: !seen;
              k ())
            r
            (fun () -> List.rev !seen)
          = leaves r)
        small);
  check "C1" "iter_k: a function that does not call k ends the traversal" (fun () ->
      let visited = ref 0 in
      iter_k
        (fun c k ->
          incr visited;
          if c = '\n' then true else k ())
        doc
        (fun () -> false)
      && !visited = 3);
  check "C1" "iter_k on a rope 100 000 deep" (fun () ->
      let n = ref 0 in
      iter_k
        (fun _ k ->
          incr n;
          k ())
        (left_deep 100_000)
        (fun () -> !n)
      = 100_000);
  check "C2" "the sequence from iter_k gives the leaves in order, on the 23 small ropes" (fun () ->
      List.for_all (fun r -> drain (Next.to_seq r) = leaves r) small);
  check "C2" "read twice, the same element twice" (fun () ->
      let r = Cat (Cat (Leaf (lazy 1), Leaf (lazy 2)), Leaf (lazy 3)) in
      let c = Next.to_seq r in
      match (c (), c ()) with
      | Cons (x, _), Cons (y, _) -> x == y && Lazy.force x = 1
      | _ -> false);
  check "C2" "same and first_difference of Part A run on it unchanged" (fun () ->
      same (Next.to_seq doc) (Next.to_seq doc')
      && same (Next.to_seq doc) (Rope.to_seq doc')
      && first_difference (Next.to_seq doc) (Next.to_seq (Cat (doc, Leaf '!'))) = Some 5);
  let three name (find : (int -> bool) -> int rope -> int option) =
    check "C3" (name ^ ": the first leaf that fits, 3 leaves asked; None when none fits") (fun () ->
        List.for_all
          (fun r ->
            let asked = ref 0 in
            let found =
              find
                (fun x ->
                  incr asked;
                  x >= 2)
                r
            in
            found = Some 2 && !asked = 3 && find (fun x -> x > 100) r = None)
          (shapes (upto 5)))
  in
  three "find_raise" find_raise;
  three "find_until" find_until;
  three "find_cps" find_cps;
  check "C3" "find_k: the continuation chooses the result" (fun () ->
      find_k (fun c -> c = '\n') doc (fun _ -> "found") (fun () -> "no") = "found"
      && find_k (fun c -> c = 'z') doc (fun _ -> "found") (fun () -> "no") = "no");
  let m p s = matches (pattern_of_string p) (Stream.of_string s) in
  check "C4" "letters and ?: the text begins with the pattern" (fun () ->
      m "let" "let x" && m "l?t" "lot" && m "" "abc" && m "" ""
      && (not (m "let" "lex"))
      && (not (m "let" "le"))
      && (not (m "l?t" "lt"))
      && not (m "a?" "a"));
  check "C4" "*: any number of characters, none too" (fun () ->
      m "f*n" "fun x" && m "f*n" "fn" && m "f*n" "function" && m "*" "" && m "a*" "a"
      && m "*a*b*" "xxaxxbxx"
      && (not (m "f*n" "fix"))
      && not (m "*a" "bbb"));
  check "C4" "? and * do not take a newline" (fun () ->
      (not (m "a?b" "a\nb")) && (not (m "a*b" "a\nb")) && m "a*" "a\nb");
  check "C4" "* takes as much as it can: after f*n on funny there is y left" (fun () ->
      match_k (pattern_of_string "f*n") (Stream.of_string "funny")
        (fun after _ -> drain after)
        (fun () -> [ '?' ])
      = [ 'y' ]);
  check "C4" "* gives back: f*n on function went back once, f*x on funny 4 times" (fun () ->
      Probe.reset ();
      let a = m "f*n" "function" in
      let back_a = !Probe.backs in
      Probe.reset ();
      let b = m "f*x" "funny" in
      let back_b = !Probe.backs in
      a && (not b) && back_a = 1 && back_b = 4);
  check "C4" "calling fail from succ gives the next match: f*n on funny ends at 4, then at 3" (fun () ->
      let ends = ref [] in
      let text = "funny" in
      let n = ref 0 in
      let c = counting n (Stream.of_string text) in
      ignore
        (match_k (pattern_of_string "f*n") c
           (fun after fail ->
             ends := (String.length text - List.length (drain after)) :: !ends;
             fail ())
           (fun () -> ()));
      List.rev !ends = [ 4; 3 ]);
  check "C4" "search: the position of the first match; None when there is none" (fun () ->
      let s p t =
        match search (pattern_of_string p) (Stream.of_string t) with
        | Some (i, after) -> Some (i, List.length (drain after))
        | None -> None
      in
      s "y*2" text = Some (14, 10) && s "pr?nt" text = Some (21, 3) && s "zz" text = None)

let all () : check list =
  results := [];
  Probe.reset ();
  part_a ();
  part_b ();
  part_c ();
  List.rev !results

(* the state of one level: todo if a hole is left, FAIL if a check fails *)
let level_status (cs : check list) (level : string) : string =
  let mine = List.filter (fun c -> c.level = level) cs in
  if List.exists (fun c -> match c.status with Todo _ -> true | _ -> false) mine then "todo"
  else if List.exists (fun c -> match c.status with Fail _ -> true | _ -> false) mine then "FAIL"
  else "pass"

let levels = [ "A0"; "A1"; "A2"; "A3"; "A4"; "A5"; "B1"; "B2"; "B3"; "C1"; "C2"; "C3"; "C4" ]
