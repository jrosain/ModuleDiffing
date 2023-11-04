let main () =
  let optlist = [
    ] in
  let message = "Welcome to the diffing testing utility." in
  let _ = Arg.parse optlist (fun x -> ()) message in
  Test.launch_test ();
  (*let g = Sig.G.create () in*)
  ()

let _ = main ()
