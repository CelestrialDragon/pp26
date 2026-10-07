(* Provided; not to be edited. One page served on localhost, and the commands
   of the page answered with the state as JSON. Nothing but the unix library. *)

let decode (s : string) : string =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    (match s.[!i] with
    | '%' when !i + 2 < n -> (
        match int_of_string_opt ("0x" ^ String.sub s (!i + 1) 2) with
        | Some c ->
            Buffer.add_char b (Char.chr c);
            i := !i + 2
        | None -> Buffer.add_char b '%')
    | c -> Buffer.add_char b c);
    incr i
  done;
  Buffer.contents b

let read_head (fd : Unix.file_descr) : string =
  let b = Buffer.create 1024 in
  let chunk = Bytes.create 4096 in
  let rec go () =
    let s = Buffer.contents b in
    let finished =
      let n = String.length s in
      n >= 4 && (let rec at i = i + 4 <= n && (String.sub s i 4 = "\r\n\r\n" || at (i + 1)) in at 0)
    in
    if finished || Buffer.length b > 65536 then s
    else
      let k = Unix.read fd chunk 0 4096 in
      if k = 0 then s
      else begin
        Buffer.add_subbytes b chunk 0 k;
        go ()
      end
  in
  go ()

let send (fd : Unix.file_descr) (status : string) (kind : string) (body : string) : unit =
  let head =
    Printf.sprintf "HTTP/1.1 %s\r\nContent-Type: %s; charset=utf-8\r\nContent-Length: %d\r\nCache-Control: no-store\r\nConnection: close\r\n\r\n"
      status kind (String.length body)
  in
  let all = Bytes.of_string (head ^ body) in
  let rec write off =
    if off < Bytes.length all then
      let k = Unix.write fd all off (Bytes.length all - off) in
      write (off + k)
  in
  write 0

(* the page sends this header with every command; a page of another site cannot *)
let from_our_page (head : string) : bool =
  List.exists
    (fun l -> String.lowercase_ascii (String.trim l) = "x-lab: 5")
    (String.split_on_char '\n' head)

let answer (st : Protocol.state) (fd : Unix.file_descr) : unit =
  let head = read_head fd in
  let path =
    match String.split_on_char ' ' (List.hd (String.split_on_char '\r' head)) with
    | _ :: p :: _ -> p
    | _ -> "/"
  in
  match String.split_on_char '/' path with
  | [ ""; "" ] -> send fd "200 OK" "text/html" Assets.page
  | [ ""; "rope-view.js" ] -> send fd "200 OK" "text/javascript" Assets.rope_view
  | "" :: "cmd" :: args when from_our_page head ->
      (* every argument comes with a ~ in front, so that none is "." or empty *)
      let strip a = if a <> "" && a.[0] = '~' then String.sub a 1 (String.length a - 1) else a in
      Protocol.run st (List.map (fun a -> strip (decode a)) args);
      send fd "200 OK" "application/json" (Protocol.json st)
  | "" :: "cmd" :: _ -> send fd "403 Forbidden" "text/plain" "commands come from the editor's page"
  | _ -> send fd "404 Not Found" "text/plain" "not found"

let serve (st : Protocol.state) (port : int) : unit =
  if not Sys.win32 then Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  let rec bind p =
    match Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, p)) with
    | () -> p
    | exception Unix.Unix_error (Unix.EADDRINUSE, _, _) when p < port + 20 -> bind (p + 1)
  in
  let port = bind port in
  Unix.listen sock 16;
  Printf.printf "the editor is at  http://localhost:%d/   (Ctrl-C stops it)\n%!" port;
  while true do
    let fd, _ = Unix.accept sock in
    (try answer st fd with e -> Printf.eprintf "request: %s\n%!" (Printexc.to_string e));
    try Unix.close fd with _ -> ()
  done
