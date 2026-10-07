(* Provided; not to be edited.
     dune exec ./edit.exe                        the editor on samples/notes.txt
     dune exec ./edit.exe -- samples/long.txt    on another file
     dune exec ./edit.exe -- --ed                without a browser: commands typed, one per line
     dune exec ./edit.exe -- --port 8200         on another port than 8150 *)

let read_file (path : string) : string =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let help =
  "commands: type TEXT | enter | backspace | delete | left | right | up | down | home | end\n\
  \          caret N | scroll N | undo | redo | save | change | seq stack | seq cps\n\
  \          way N | findline TEXT | findchar C | pattern P | stream NAME N | trace | resume\n\
  \          versions I J | checks | quit"

let ed (st : Protocol.state) : unit =
  print_string (Protocol.text st);
  print_endline help;
  let rec loop () =
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | "quit" | "q" -> ()
    | line ->
        let line = String.trim line in
        let word, rest =
          match String.index_opt line ' ' with
          | Some i -> (String.sub line 0 i, String.sub line (i + 1) (String.length line - i - 1))
          | None -> (line, "")
        in
        (match word with
        | "" -> ()
        | "help" -> print_endline help
        | "checks" ->
            List.iter
              (fun l -> Printf.printf "  %s  %s: %s\n" (Checks.level_status st.checks l) l (Protocol.unlocks l))
              Checks.levels
        | "type" -> String.iter (fun c -> Protocol.run st [ "insert"; string_of_int (Char.code c) ]) rest
        | "enter" -> Protocol.run st [ "insert"; "10" ]
        | "findline" | "findchar" | "pattern" -> Protocol.run st [ word; rest ]
        | "stream" -> (
            match String.rindex_opt rest ' ' with
            | Some i -> Protocol.run st [ word; String.sub rest 0 i; String.sub rest (i + 1) (String.length rest - i - 1) ]
            | None -> Protocol.run st [ word; rest; "10" ])
        | _ -> Protocol.run st (List.filter (fun w -> w <> "") (String.split_on_char ' ' line)));
        if word <> "help" && word <> "checks" then print_string (Protocol.text st);
        loop ()
  in
  loop ()

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let line_mode = List.mem "--ed" args in
  let rec port = function
    | "--port" :: p :: _ -> (match int_of_string_opt p with Some p -> p | None -> 8150)
    | _ :: rest -> port rest
    | [] -> 8150
  in
  let rec plain = function
    | "--port" :: _ :: rest -> plain rest
    | a :: rest -> if String.length a > 0 && a.[0] <> '-' then a :: plain rest else plain rest
    | [] -> []
  in
  let files = plain args in
  let file = match files with f :: _ -> f | [] -> Filename.concat "samples" "notes.txt" in
  match read_file file with
  | exception Sys_error m ->
      prerr_endline ("cannot read " ^ m ^ "\nthe editor is started from the folder of the lab: dune exec ./edit.exe");
      exit 1
  | contents ->
      let st = Protocol.make file contents in
      if line_mode then ed st else Server.serve st (port args)
