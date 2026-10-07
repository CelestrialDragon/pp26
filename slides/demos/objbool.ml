type mybool = < and_ : mybool -> mybool ; choose : int -> int -> int >

let mytrue : mybool =
  object method and_ o = o           method choose x _ = x end
let myfalse : mybool =
  object (self) method and_ _ = self  method choose _ y = y end

let () =
  Printf.printf "%d %d %d\n"
    ((mytrue#and_ myfalse)#choose 1 0)
    ((mytrue#and_ mytrue)#choose 1 0)
    ((myfalse#and_ mytrue)#choose 1 0)
