
let main () =
  let optlist = [
    ] in
  let message = "Welcome to the diffing testing utility." in
  let _ = Arg.parse optlist (fun x -> ()) message in
  let g = Graphs.G.create () in ()

let _ = main ()
