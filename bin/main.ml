let main () =
  let optlist = [
    ] in
  let message = "Welcome to the diffing testing utility." in
  let _ = Arg.parse optlist (fun _ -> ()) message in
  ()

let _ = main ()
