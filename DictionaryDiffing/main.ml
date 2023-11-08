let lexbuf = Lexing.from_channel stdin

let parse () = Parser.main Lexer.token lexbuf
      
let main () =
  try
    let d1 = parse () in
    let d2 = parse () in
    Printf.printf "Checking difference between following dict:\n";
    Printf.printf "d1 = "; Display.display_dict d1; print_newline ();
    Printf.printf "d2 = "; Display.display_dict d2; print_newline ();
    let patch = Diffing.diff d1 d2 in
    Printf.printf "The patch is the following:\n";
    Display.display_patch patch; print_newline ();
    Printf.printf "Applying the patch to test...\n";
    let new_d = Patch.apply_patch d1 patch in
    if not (Dict.is_dict_equal new_d d2) then begin
      Printf.printf "d2 = "; Display.display_dict d2; print_newline ();
      Printf.printf "d1 + patch = "; Display.display_dict new_d; print_newline ();
      Printf.printf "d1 + patch is different from d2, the patch was not correct\n"
    end else
      Printf.printf "the patch is good\nTerminate.\n"
  with
  | Patch.ApplyPatchError -> Printf.printf "error during the application of the patch\n"
  | _                     -> Printf.printf "parsing error\n"
;;

let _ = main()
