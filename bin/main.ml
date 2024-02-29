open ModuleDiffing

let main () =
  let optlist = [
      ("-dico", Arg.Set Opt.dico, "Set diffing algorithm to dictionnary diffing. Default: MH-DIFF.");
    ] in
  let message = "Welcome to the diffing testing utility." in
  let _ = Arg.parse optlist (fun _ -> ()) message in
  Example.launch_test ();
  (*let g = Sig.G.create () in*)
  ()

let _ = main ()
