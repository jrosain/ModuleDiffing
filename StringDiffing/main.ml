
let jaccard = ref false
let jaro_winkler = ref false
let jaccard_granularity = ref 2
let winkler_p = ref 0.1

let main () =
  let optlist = [
      ("-jaccard", Arg.Set jaccard, "Run the jaccard's diffing algorithm.");
      ("-jaro-winkler", Arg.Set jaro_winkler, "Run the jaro-winkler's diffing algorithm.");
      ("-p", Arg.Set_float winkler_p, "Select the p parameter for jaro-winkler's algorithm.");
      ("-n", Arg.Set_int jaccard_granularity, "Select jaccard's granularity.");
    ] in
  let message = "Welcome to the string diffing utility." in
  let _ = Arg.parse optlist (fun x -> ()) message in
  let str1 = read_line() in
  let str2 = read_line() in
  if !jaccard then
    let res = Jaccard.diffing str1 str2 !jaccard_granularity in
    Printf.printf "Jaccard similarity: %f\n" res; ()
  else if !jaro_winkler then
    let res = Jaro_winkler.diffing str1 str2 ~p:!winkler_p in
    Printf.printf "Jaro-Winkler similarity: %f\n" res; ()

let _ = main ()

    
  


            
