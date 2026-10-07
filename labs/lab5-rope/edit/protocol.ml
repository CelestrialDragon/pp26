(* Provided; not to be edited. The editor's state, its commands, and the state
   written as JSON for the page. Every feature calls the functions of rope.ml,
   stream.ml and next.ml; a feature whose hole is not filled is locked.

   What is counted is counted from outside: the functions we hand over count
   their own calls, the sequences we hand over count the elements asked for. *)

open Rope

let rows = 24

(* the traversal of the trace panel, stopped between two leaves *)
type held =
  | Stopped of int * char * (unit -> held)
  | Finished

type panel =
  | No_panel
  | Stream_panel of string * string list * int           (* name, elements, cells produced *)
  | Trace_panel of string list * bool                    (* what happened so far, finished *)
  | Versions_panel of int * int * int * int * int        (* i, j, nodes of i, nodes of j, shared *)

type state = {
  file : string;
  mutable doc : char rope;
  mutable hist : char rope history;
  mutable saved : char rope;
  mutable caret : int;
  mutable top : int;
  mutable cps : bool;                                     (* the sequence of Part C, not of Part A *)
  mutable way : int;                                      (* 0, 1, 2: the three find *)
  mutable message : string;
  mutable counters : (string * int) list;
  mutable lit : int list;                                 (* leaves visited by the last command *)
  mutable sel : (int * int) option;
  mutable panel : panel;
  mutable held : (unit -> held) option;
  mutable other : char rope option;                       (* the version compared with *)
  checks : Checks.check list;
}

(* ---- helpers that use no function of the student ------------------------ *)

let rec size (r : 'a rope) : int =
  match r with
  | Leaf _ -> 1
  | Cat (l, r) -> size l + size r

let flat_cache : (char rope * string) option ref = ref None

let flat (r : char rope) : string =
  match !flat_cache with
  | Some (r', s) when r' == r -> s
  | _ ->
      let b = Buffer.create 1024 in
      iter (Buffer.add_char b) r;
      let s = Buffer.contents b in
      flat_cache := Some (r, s);
      s

let count_lines (s : string) : int =
  let n = ref 0 in
  String.iter (fun c -> if c = '\n' then incr n) s;
  if s <> "" && s.[String.length s - 1] <> '\n' then !n + 1 else !n

let line_of (s : string) (pos : int) : int =
  let n = ref 0 in
  for i = 0 to min pos (String.length s) - 1 do
    if s.[i] = '\n' then incr n
  done;
  !n

let line_start (s : string) (pos : int) : int =
  let i = ref pos in
  while !i > 0 && s.[!i - 1] <> '\n' do
    decr i
  done;
  !i

let line_end (s : string) (pos : int) : int =
  let i = ref pos in
  while !i < String.length s - 1 && s.[!i] <> '\n' do
    incr i
  done;
  !i

(* the position of the start of line number n, counting from 0 *)
let start_of_line (s : string) (n : int) : int =
  let seen = ref 0 and i = ref 0 in
  while !seen < n && !i < String.length s do
    if s.[!i] = '\n' then incr seen;
    incr i
  done;
  min !i (String.length s - 1)

(* the same shape, every leaf with its position *)
let number (base : int) (r : char rope) : (int * char) rope =
  let n = ref (base - 1) in
  let rec go = function
    | Leaf c ->
        incr n;
        Leaf (!n, c)
    | Cat (l, r) ->
        let l' = go l in
        let r' = go r in
        Cat (l', r')
  in
  go r

let split_lines (s : string) : string list =
  match List.rev (String.split_on_char '\n' s) with
  | "" :: rest -> List.rev rest
  | all -> List.rev all

let rec drop n xs = if n <= 0 then xs else match xs with [] -> [] | _ :: r -> drop (n - 1) r
let rec first n xs = if n <= 0 then [] else match xs with [] -> [] | x :: r -> x :: first (n - 1) r

let show_char c = if c = '\n' then "newline" else if c = ' ' then "space" else String.make 1 c

(* ---- levels -------------------------------------------------------------- *)

let locked st level = Checks.level_status st.checks level = "todo"
let cursor_level st = if st.cps then "C2" else "A4"
let seq_of st r = if st.cps then Next.to_seq r else Rope.to_seq r

exception Locked of string

let need st level = if locked st level then raise (Locked level)

let unlocks = function
  | "A0" -> "the file appears, with its length and its depth"
  | "A1" -> "typing and backspace"
  | "A2" -> "undo and redo"
  | "A3" -> "End: to the end of the line"
  | "A4" -> "the screen drawn from a sequence"
  | "A5" -> "the mark modified, and the jump to the first change"
  | "B1" -> "the panel stream"
  | "B2" -> "line numbers; lines read as the screen asks for them"
  | "B3" -> "find the next line that contains a text"
  | "C1" -> "the panel trace"
  | "C2" -> "the screen drawn from the sequence of Part C"
  | "C3" -> "find the next character, three ways"
  | "C4" -> "search with a pattern"
  | _ -> ""

(* ---- making a state ------------------------------------------------------ *)

let clean (s : string) : string =
  let s = String.map (fun c -> if Char.code c > 126 || (Char.code c < 32 && c <> '\n' && c <> '\t') then '?' else c) s in
  if s = "" || s.[String.length s - 1] <> '\n' then s ^ "\n" else s

let make (file : string) (contents : string) : state =
  let doc = Rope.of_string (clean contents) in
  {
    file; doc; hist = start doc; saved = doc; caret = 0; top = 0; cps = false; way = 0;
    message = ""; counters = []; lit = []; sel = None; panel = No_panel; held = None;
    other = None; checks = Checks.all ();
  }

(* ---- the window: the lines on the screen --------------------------------- *)

(* the lines shown, the position of the first character shown, whether lines
   are numbered, and what it cost *)
let window st : string list * int * bool * (string * int) list =
  if locked st "A0" then ([], 0, false, [])
  else
    let have_cursor = not (locked st (cursor_level st)) in
    if have_cursor && not (locked st "B2") then begin
      let leaves = ref 0 and asked = ref 0 in
      let ls = Checks.counting asked (Stream.lines (Checks.counting leaves (seq_of st st.doc))) in
      let rec skip n c off =
        if n = 0 then (c, off)
        else
          match c () with
          | Nil -> ((fun () -> Nil), off)
          | Cons (s, rest) -> skip (n - 1) rest (off + String.length s + 1)
      in
      let rec take n c =
        if n = 0 then []
        else
          match c () with
          | Nil -> []
          | Cons (s, rest) -> s :: take (n - 1) rest
      in
      let c, off = skip st.top ls 0 in
      let shown = take rows c in
      (shown, off, true, [ ("leaves pulled", !leaves); ("lines pulled", !asked) ])
    end
    else if have_cursor then begin
      let leaves = ref 0 in
      let b = Buffer.create 1024 in
      let rec go c line off =
        if line >= st.top + rows then off
        else
          match c () with
          | Nil -> off
          | Cons (x, rest) ->
              incr leaves;
              if line >= st.top then Buffer.add_char b x;
              go rest (if x = '\n' then line + 1 else line) (if line < st.top then off + 1 else off)
      in
      let off = go (seq_of st st.doc) 0 0 in
      (split_lines (Buffer.contents b), off, false, [ ("leaves pulled", !leaves) ])
    end
    else begin
      let l = to_list st.doc in
      let s = String.of_seq (List.to_seq l) in
      let all = split_lines s in
      let off = List.fold_left (fun a l -> a + String.length l + 1) 0 (first st.top all) in
      (first rows (drop st.top all), off, false, [ ("leaves flattened", List.length l) ])
    end

(* ---- sharing between two versions ---------------------------------------- *)

let table_of (r : char rope) : (int * int, char rope) Hashtbl.t =
  let t = Hashtbl.create 1024 in
  let rec go r =
    let n = match r with Leaf _ -> 1 | Cat (l, r) -> let a = go l in let b = go r in a + b in
    Hashtbl.add t (n, Hashtbl.hash r) r;
    n
  in
  ignore (go r);
  t

let is_in t (r : char rope) (n : int) : bool = List.memq r (Hashtbl.find_all t (n, Hashtbl.hash r))

(* the nodes of b, and those of them that are nodes of a as well *)
let shared (a : char rope) (b : char rope) : int * int =
  let t = table_of a in
  let total = ref 0 and common = ref 0 in
  let rec go r =
    let n = size r in
    if is_in t r n then begin
      total := !total + (2 * n) - 1;
      common := !common + (2 * n) - 1
    end
    else begin
      incr total;
      match r with
      | Leaf _ -> ()
      | Cat (l, r) ->
          go l;
          go r
    end
  in
  go b;
  (!total, !common)

let versions st : char rope list = List.rev st.hist.past @ (st.hist.present :: st.hist.future)

(* ---- commands ------------------------------------------------------------ *)

let set_doc st (r : char rope) =
  st.doc <- r;
  st.other <- None

let edit st (r : char rope) =
  set_doc st r;
  if locked st "A2" then st.hist <- start r else st.hist <- record r st.hist

let clamp st = st.caret <- max 0 (min st.caret (size st.doc - 1))

(* the part of the document from position pos on, its leaves numbered *)
let from st pos : (int * char) rope =
  if pos <= 0 then number 0 st.doc else number pos (snd (split pos st.doc))

let find_way st p r =
  match st.way with
  | 0 -> ("raise Found", Next.find_raise p r)
  | 1 -> ("fold_until", Next.find_until p r)
  | _ -> ("k and h", Next.find_cps p r)

let stream_panel name n =
  let open Stream in
  let n = max 1 (min n 40) in
  let produced = ref 0 in
  let src = Checks.counting produced (nats 1) in
  let shown =
    match name with
    | "nats" -> List.map string_of_int (take n src)
    | "evens" -> List.map string_of_int (take n (filter (fun x -> x mod 2 = 0) src))
    | "squares" -> List.map string_of_int (take n (map (fun x -> x * x) src))
    | "small squares" ->
        List.map string_of_int (take n (take_while (fun x -> x < 50) (map (fun x -> x * x) src)))
    | _ -> failwith "stream: nats, evens, squares, small squares"
  in
  Stream_panel (name, shown, !produced)

(* the subtree around the caret that the rope view draws and the trace runs on *)
let focus (limit : int) (r : char rope) (caret : int) : char rope * int * int =
  let rec go r n base depth =
    if n <= limit then (r, base, depth)
    else
      match r with
      | Leaf _ -> (r, base, depth)
      | Cat (l, r) ->
          let nl = size l in
          if caret - base < nl then go l nl base (depth + 1) else go r (n - nl) (base + nl) (depth + 1)
  in
  go r (size r) 0 0

let command st (args : string list) : unit =
  let int s = match int_of_string_opt s with Some n -> n | None -> failwith ("not a number: " ^ s) in
  let n = size st.doc in
  let s = flat st.doc in
  st.counters <- [];
  st.lit <- [];
  st.sel <- None;
  st.message <- "";
  match args with
  | [ "state" ] -> ()
  | [ "caret"; p ] -> st.caret <- int p
  | [ "left" ] -> st.caret <- st.caret - 1
  | [ "right" ] -> st.caret <- st.caret + 1
  | [ "home" ] -> st.caret <- line_start s st.caret
  | [ "up" ] ->
      let a = line_start s st.caret in
      if a > 0 then begin
        let col = st.caret - a in
        let a' = line_start s (a - 1) in
        st.caret <- min (a' + col) (a - 1)
      end
  | [ "down" ] ->
      let a = line_start s st.caret and e = line_end s st.caret in
      if e < n - 1 then st.caret <- min (e + 1 + (st.caret - a)) (line_end s (e + 1))
  | [ "scroll"; k ] ->
      let total = count_lines s in
      st.top <- max 0 (min (st.top + int k) (total - 1));
      let l = line_of s st.caret in
      if l < st.top then st.caret <- start_of_line s st.top
      else if l >= st.top + rows then st.caret <- start_of_line s (st.top + rows - 1)
  | [ "insert"; code ] ->
      need st "A1";
      let code = int code in
      if code = 13 || code = 10 || code = 9 || (code >= 32 && code <= 126) then begin
        let c = if code = 13 then '\n' else Char.chr code in
        edit st (insert st.caret c st.doc);
        st.caret <- st.caret + 1
      end
      else st.message <- "a leaf is one character of ASCII here"
  | [ "backspace" ] ->
      need st "A1";
      if st.caret > 0 then begin
        edit st (delete (st.caret - 1) st.doc);
        st.caret <- st.caret - 1
      end
  | [ "delete" ] ->
      need st "A1";
      if st.caret < n - 1 then edit st (delete st.caret st.doc)
      else st.message <- "the last newline stays: a rope has at least one leaf"
  | [ "undo" ] ->
      need st "A2";
      st.hist <- undo st.hist;
      set_doc st st.hist.present
  | [ "redo" ] ->
      need st "A2";
      st.hist <- redo st.hist;
      set_doc st st.hist.present
  | [ "end" ] ->
      need st "A3";
      let visited = ref [] in
      let found = find_first (fun (i, c) -> visited := i :: !visited; c = '\n') (from st st.caret) in
      (match found with Some (i, _) -> st.caret <- i | None -> ());
      st.lit <- !visited;
      st.counters <- [ ("leaves visited", List.length !visited); ("leaves after the caret", n - st.caret) ];
      st.message <- "End, with find_first. iter would call our function on every leaf after the caret."
  | [ "save" ] ->
      let oc = open_out_bin st.file in
      output_string oc s;
      close_out oc;
      st.saved <- st.doc;
      st.message <- "saved " ^ st.file
  | [ "change" ] ->
      need st "A5";
      need st (cursor_level st);
      let asked = ref 0 in
      let c r = Checks.counting asked (seq_of st r) in
      (match first_difference (c st.saved) (c st.doc) with
      | Some i ->
          st.caret <- i;
          st.message <- "the first change since saving"
      | None -> st.message <- "no change since saving");
      st.counters <- [ ("leaves asked", !asked); ("leaves in the two versions", size st.saved + n) ]
  | [ "seq"; which ] ->
      let cps = which = "cps" in
      if locked st (if cps then "C2" else "A4") then raise (Locked (if cps then "C2" else "A4"));
      st.cps <- cps;
      st.message <- (if cps then "the screen is drawn from the sequence of next.ml" else "the screen is drawn from the sequence of rope.ml")
  | [ "way"; w ] ->
      st.way <- int w mod 3;
      st.message <- "find: " ^ [| "iter and raise Found"; "fold_until"; "k and h" |].(st.way)
  | [ "findline"; text ] ->
      need st "B3";
      need st "B2";
      need st (cursor_level st);
      let here = line_of s st.caret + 1 in
      let asked = ref 0 in
      let ls = Checks.counting asked (Stream.lines (seq_of st st.doc)) in
      let after = Stream.grep (Stream.contains text) ls in
      let rec next c =
        match c () with
        | Nil -> None
        | Cons ((k, l), rest) -> if k > here then Some (k, l) else next rest
      in
      (match next after with
      | Some (k, _) ->
          st.caret <- start_of_line s (k - 1);
          st.sel <- Some (st.caret, line_end s st.caret);
          st.message <- Printf.sprintf "line %d contains it" k
      | None -> st.message <- "no line after this one contains it");
      st.counters <- [ ("lines asked by grep", !asked); ("lines in the document", count_lines s) ]
  | [ "findchar"; c ] ->
      need st "C3";
      if st.way = 1 then need st "A3";
      if String.length c <> 1 then failwith "one character";
      if st.caret + 1 >= n then st.message <- "the caret is at the end"
      else begin
        let visited = ref [] in
        let name, found = find_way st (fun (i, x) -> visited := i :: !visited; x = c.[0]) (from st (st.caret + 1)) in
        (match found with
        | Some (i, _) ->
            st.caret <- i;
            st.sel <- Some (i, i + 1);
            st.message <- "found, with " ^ name
        | None -> st.message <- "not found, with " ^ name);
        st.lit <- !visited;
        st.counters <- [ ("leaves visited", List.length !visited); ("leaves after the caret", n - st.caret - 1) ]
      end
  | [ "pattern"; p ] ->
      need st "C4";
      need st (cursor_level st);
      if st.caret + 1 >= n then st.message <- "the caret is at the end"
      else begin
        let base = st.caret + 1 in
        let asked = ref 0 in
        let c = Checks.counting asked (seq_of st (snd (split base st.doc))) in
        Probe.reset ();
        let found = Next.search (Next.pattern_of_string p) c in
        let cost = !asked and back = !Probe.backs in
        (match found with
        | Some (i, after) ->
            let left = List.length (Checks.drain after) in
            st.caret <- base + i;
            st.sel <- Some (base + i, n - left);
            st.message <- "the pattern matches here"
        | None -> st.message <- "no match after the caret");
        st.counters <- [ ("went back", back); ("cells asked", cost) ]
      end
  | [ "stream"; name; k ] ->
      need st "B1";
      st.panel <- stream_panel name (int k)
  | [ "trace" ] ->
      need st "C1";
      let r, base, _ = focus 12 st.doc st.caret in
      let k = fun () -> Next.iter_k (fun (i, c) k -> Stopped (i, c, k)) (number base r) (fun () -> Finished) in
      st.held <- Some k;
      st.panel <- Trace_panel ([ "iter_k is not called yet: what we hold is a function" ], false)
  | [ "resume" ] ->
      need st "C1";
      (match (st.held, st.panel) with
      | Some k, Trace_panel (lines, _) -> (
          match k () with
          | Stopped (i, c, k') ->
              st.held <- Some k';
              st.lit <- [ i ];
              st.panel <-
                Trace_panel
                  (lines @ [ Printf.sprintf "our function received the leaf %s and k; it did not call k" (show_char c) ], false)
          | Finished ->
              st.held <- None;
              st.panel <- Trace_panel (lines @ [ "the last continuation was called: the traversal is over" ], true))
      | _ -> st.message <- "the traversal is over; start a new one")
  | [ "versions"; i; j ] ->
      need st "A2";
      let vs = versions st in
      let i = int i and j = int j in
      if i < 0 || j < 0 || i >= List.length vs || j >= List.length vs then failwith "no such version";
      let a = List.nth vs i and b = List.nth vs j in
      let total_b, common = shared a b in
      st.other <- (if a == st.doc then Some b else if b == st.doc then Some a else None);
      st.panel <- Versions_panel (i, j, (2 * size a) - 1, total_b, common)
  | [ "panel"; "off" ] ->
      st.panel <- No_panel;
      st.other <- None
  | _ -> st.message <- "unknown command: " ^ String.concat " " args

(* a command, whatever happens: a hole locks the feature, an exception is reported *)
let run st (args : string list) : unit =
  (try command st args with
  | Locked level -> st.message <- Printf.sprintf "locked: level %s gives %s" level (unlocks level)
  | Failure m when Checks.is_todo m ->
      let level = String.trim (String.sub m 4 (String.length m - 4)) in
      st.message <- Printf.sprintf "locked: level %s gives %s" level (unlocks level)
  | Stack_overflow -> st.message <- "stack overflow in " ^ String.concat " " args
  | e -> st.message <- Printf.sprintf "%s raised %s" (String.concat " " args) (Printexc.to_string e));
  if not (locked st "A0") then begin
    (try clamp st with _ -> ());
    let l = line_of (flat st.doc) st.caret in
    if l < st.top then st.top <- l else if l >= st.top + rows then st.top <- l - rows + 1
  end

(* ---- JSON, by hand ------------------------------------------------------- *)

let str (s : string) : string =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | c when Char.code c < 32 || Char.code c > 126 -> Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let arr (xs : string list) : string = "[" ^ String.concat "," xs ^ "]"
let obj (fields : (string * string) list) : string =
  "{" ^ String.concat "," (List.map (fun (k, v) -> str k ^ ":" ^ v) fields) ^ "}"
let num = string_of_int
let bool b = if b then "true" else "false"

let rope_json st : string =
  let r, base, above = focus 32 st.doc st.caret in
  let lit = Hashtbl.create 64 in
  List.iter (fun i -> Hashtbl.replace lit i ()) st.lit;
  let table = match st.other with Some o -> Some (table_of o) | None -> None in
  let i = ref (base - 1) in
  let rec go r =
    let is_shared = match table with Some t -> is_in t r (size r) | None -> false in
    match r with
    | Leaf c ->
        incr i;
        obj
          [ ("leaf", str (String.make 1 c)); ("lit", bool (Hashtbl.mem lit !i)); ("caret", bool (!i = st.caret));
            ("shared", bool is_shared) ]
    | Cat (l, r) ->
        let a = go l in
        let b = go r in
        obj [ ("l", a); ("r", b); ("shared", bool is_shared) ]
  in
  let tree = go r in
  obj [ ("tree", tree); ("above", num above); ("first", num base); ("leaves", num (size r)) ]

let panel_json st : string =
  match st.panel with
  | No_panel -> "null"
  | Stream_panel (name, xs, produced) ->
      obj [ ("kind", str "stream"); ("name", str name); ("elements", arr (List.map str xs)); ("produced", num produced) ]
  | Trace_panel (lines, finished) ->
      obj [ ("kind", str "trace"); ("lines", arr (List.map str lines)); ("finished", bool finished) ]
  | Versions_panel (i, j, a, b, common) ->
      obj [ ("kind", str "versions"); ("i", num i); ("j", num j); ("nodes_i", num a); ("nodes_j", num b); ("shared", num common) ]

let levels_json st : string =
  arr
    (List.map
       (fun level ->
         obj
           [ ("level", str level);
             ("status", str (Checks.level_status st.checks level));
             ("gives", str (unlocks level));
             ( "checks",
               arr
                 (List.filter_map
                    (fun (c : Checks.check) ->
                      if c.level <> level then None
                      else
                        Some
                          (obj
                             [ ("name", str c.name);
                               ( "status",
                                 str (match c.status with Pass -> "pass" | Fail _ -> "FAIL" | Todo _ -> "todo") );
                               ("why", str (match c.status with Pass -> "" | Fail w -> w | Todo w -> w)) ]))
                    st.checks) ) ])
       Checks.levels)

(* a part of the state that a hole or a wrong answer can break is null then *)
let guard (f : unit -> string) : string = try f () with _ -> "null"

let json st : string =
  let shown, off, numbered, cost = try window st with _ -> ([], 0, false, []) in
  let s = flat st.doc in
  let modified =
    guard (fun () ->
        if locked st "A5" || locked st (cursor_level st) then "null"
        else bool (not (same (seq_of st st.saved) (seq_of st st.doc))))
  in
  obj
    [ ("file", str st.file);
      ("appears", bool (not (locked st "A0")));
      ("length", guard (fun () -> num (length st.doc)));
      ("depth", guard (fun () -> num (depth st.doc)));
      ("lines", num (count_lines s));
      ("caret", num st.caret);
      ("top", num st.top);
      ("offset", num off);
      ("rows", arr (List.map str shown));
      ("numbered", bool numbered);
      ("modified", modified);
      ("versions", if locked st "A2" then "null" else obj [ ("count", num (List.length (versions st))); ("at", num (List.length st.hist.past)) ]);
      ("seq", str (if st.cps then "cps" else "stack"));
      ("way", num st.way);
      ("message", str st.message);
      ("counters", obj (List.map (fun (k, v) -> (k, num v)) (cost @ st.counters)));
      ("sel", match st.sel with Some (a, b) -> arr [ num a; num b ] | None -> "null");
      ("levels", levels_json st);
      ("rope", if locked st "A0" then "null" else guard (fun () -> rope_json st));
      ("panel", panel_json st) ]

(* ---- the state as text, for --ed ----------------------------------------- *)

let text st : string =
  let shown, off, numbered, cost = try window st with _ -> ([], 0, false, []) in
  let b = Buffer.create 1024 in
  let pos = ref off in
  List.iteri
    (fun i l ->
      let a = !pos in
      let e = a + String.length l in
      let l' =
        if st.caret >= a && st.caret <= e then String.sub l 0 (st.caret - a) ^ "|" ^ String.sub l (st.caret - a) (e - st.caret)
        else l
      in
      if numbered then Buffer.add_string b (Printf.sprintf "%4d  " (st.top + i + 1));
      Buffer.add_string b l';
      Buffer.add_char b '\n';
      pos := e + 1)
    shown;
  if locked st "A0" then Buffer.add_string b "(level A0 makes the file appear)\n";
  let counters = String.concat ", " (List.map (fun (k, v) -> Printf.sprintf "%s %d" k v) (cost @ st.counters)) in
  Buffer.add_string b (Printf.sprintf "-- %s  caret %d  %s\n" st.file st.caret counters);
  if st.message <> "" then Buffer.add_string b ("-- " ^ st.message ^ "\n");
  (match st.panel with
  | Stream_panel (name, xs, produced) ->
      Buffer.add_string b (Printf.sprintf "-- %s: %s; cells produced %d\n" name (String.concat " " xs) produced)
  | Trace_panel (lines, _) -> List.iter (fun l -> Buffer.add_string b ("-- " ^ l ^ "\n")) lines
  | Versions_panel (i, j, a, c, common) ->
      Buffer.add_string b (Printf.sprintf "-- version %d has %d nodes, version %d has %d; %d of them are shared\n" i a j c common)
  | No_panel -> ());
  Buffer.contents b
