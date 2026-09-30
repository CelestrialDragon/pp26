(* Provided. The checks, one line each: `dune exec ./main.exe` *)

let part = function
  | 'A' -> "Part A, the rope (rope.ml)"
  | 'B' -> "Part B, sequences (stream.ml)"
  | _ -> "Part C, what comes next (next.ml)"

let () =
  let ok = ref 0 and bad = ref 0 and todo = ref 0 in
  let last = ref ' ' in
  List.iter
    (fun (c : Checks.check) ->
      if c.level.[0] <> !last then begin
        last := c.level.[0];
        print_endline (part !last)
      end;
      match c.status with
      | Pass ->
          incr ok;
          Printf.printf "  pass  %s %s\n" c.level c.name
      | Fail why ->
          incr bad;
          Printf.printf "  FAIL  %s %s: %s\n" c.level c.name why
      | Todo msg ->
          incr todo;
          Printf.printf "  todo  %s %s (%s)\n" c.level c.name msg)
    (Checks.all ());
  Printf.printf "\n%d passed, %d failed, %d to do\n" !ok !bad !todo
