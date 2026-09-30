(* Provided. The counters that the checks and the editor read. *)

let backs = ref 0

(* called each time a search goes back to a choice it had left open *)
let back () = incr backs

let reset () = backs := 0
