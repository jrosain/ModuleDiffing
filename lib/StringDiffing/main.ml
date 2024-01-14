
let jaccard = ref false
let jaccard_granularity = ref 2

let jaro_winkler = ref false
let winkler_p = ref 0.1

let levenshtein = ref false
let damerau = ref false

let main () =
  let optlist = [
      ("-jaccard", Arg.Set jaccard, "Run the jaccard's diffing algorithm.");
      ("-n", Arg.Set_int jaccard_granularity, "Select jaccard's granularity.");
      ("-jaro-winkler", Arg.Set jaro_winkler, "Run the jaro-winkler's diffing algorithm.");
      ("-p", Arg.Set_float winkler_p, "Select the p parameter for jaro-winkler's algorithm.");
      ("-levenshtein", Arg.Set levenshtein, "Run the levenshtein's diffing algorithm.");
      ("-d", Arg.Set damerau, "Change Levenshtein to Damerau Levenshtein's diffing algorithm.");
    ] in
  let message = "Welcome to the string diffing utility." in
  let _ = Arg.parse optlist (fun x -> ()) message in
  let str1 = read_line() in
  let str2 = read_line() in
  if !jaccard then
    let res = Jaccard.diffing str1 str2 !jaccard_granularity in
    Printf.printf "Jaccard similarity: %f\n" res;
  if !jaro_winkler then
    let res = Jaro_winkler.diffing str1 str2 ~p:!winkler_p in
    Printf.printf "Jaro-Winkler similarity: %f\n" res;
  if !levenshtein then
    let res = Damerau_levenshtein.diffing str1 str2 !damerau in
    if !damerau then
      Printf.printf "Damerau ";
    Printf.printf "Levenshtein similarity: %d\n" res; ()
   
let _ = main ()

    
  


            
