
let jaccard = ref false
let jaccard_granularity = ref 2

let main () =
  let optlist = [
      ("-jaccard", Arg.Set jaccard, "Run the jaccard's diffing algorithm.");
      ("-n", Arg.Set_int jaccard_granularity, "Select jaccard's granularity.");
    ] in
  let message = "Welcome to the string diffing utility." in
  let _ = Arg.parse optlist (fun x -> ()) message in
  let str1 = read_line() in
  let str2 = read_line() in
  if !jaccard then
    let res = Jaccard.diffing str1 str2 !jaccard_granularity in
    Printf.printf "Jaccard similarity: %f\n" res; ()

let _ = main ()

    
  


            
