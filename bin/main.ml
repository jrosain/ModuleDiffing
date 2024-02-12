let main () =
  let optlist = [
    ] in
  let message = "Welcome to the diffing testing utility." in
  let _ = Arg.parse optlist (fun _ -> ()) message in
  ModuleDiffing.Example.launch_test ();
  (*let g = Sig.G.create () in*)
  ()

let _ = main ()
